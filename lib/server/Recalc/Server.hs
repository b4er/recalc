{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Recalc.Server
Description : Generic json-rpc backend with a Servant-like API.

This module implements a reactor pattern for `Recalc`, ensuring asynchronous
processing of spreadsheet updates and computations. A single threaded loop
listens for json-rpc messages and maintains jobs.

Jobs are picked up by multiple "worker" threads such that long-running
computations do not block messaging.

=== Example

@
type Protocol

type Api = ToApi Protocol

initialState = ...

main :: IO ()
main = runHandler @Api initialState $ \state ->
  hoist @Api (`runReaderT` state) (namedHandlers server)
 where
  server :: Protocol (AsServerT (Handler EngineState))
  server = Protocol { ... }
@
-}
module Recalc.Server
  ( Handler
  , liftEngine
  , scheduleJob
  , runHandler
  , sendBS
  , module Recalc.Server.Generic
  , aesonOptions
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LB
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

import Recalc.Server.Generic
import Recalc.Server.Json (aesonOptions)

-- | send json-rpc message on stdout
sendBS :: LB.ByteString -> IO ()
sendBS = BS.putStr . BS.toStrict . toRpc
 where
  toRpc bs =
    LB.concat
      [ "Content-Length: " <> showLB (LB.length bs)
      , "\r\n\r\n"
      , bs
      ]

  showLB = LB.fromStrict . Text.encodeUtf8 . Text.pack . show

data State engine = State
  { jobs :: TChan (Job engine)
  , engine :: TVar engine
  }

newtype Job engine = Job (ReaderT (State engine) IO ())

runJob :: Job engine -> State engine -> IO ()
runJob (Job t) = runReaderT t

{- "Handler" monad & utilities -}

-- | handler threads are in IO and have access to the state
type Handler engine = ReaderT (State engine) IO

scheduleJob :: Job engine -> Handler engine ()
scheduleJob x = liftIO . atomically . (`writeTChan` x) =<< asks jobs

liftEngine :: () -> Handler engine a
liftEngine = error "liftEngine" engine

-- | reads lines from stdin and maintains a channel of reactor inputs with new requests,
-- handler threads deal with requests by reading from the request channel.
runHandler
  :: forall api engine. HasHandler api => engine -> (State engine -> HandlerT api IO) -> IO ()
runHandler engine0 handlers = (`Exception.catches` failureHandlers) $ do
  mapM_ (`hSetBuffering` NoBuffering) [stdin, stdout]
  hSetBuffering stderr LineBuffering

  hPutStrLn stderr "entered Haskell.main"

  state <- State <$> atomically newTChan <*> newTVarIO engine0

  replicateM_ 8 . forkIO . forever
    $ (`runJob` state) =<< atomically (readTChan (jobs state))

  forever $ do
    -- Content-Length: XX\n\r\n\r{....}
    (ln0, ln1) <- (,) <$> getLine <*> getLine
    case (readMaybe $ drop (length @[] "Content-Length: ") ln0, ln1) of
      (Just k, "\r") -> do
        reqRaw <- LB.hGet stdin k
        -- mapM_ (appendFile debugOutput) [ln0, ln1, Text.unpack (decodeUtf8 $ LB.toStrict reqRaw)]
        case do x <- Json.eitherDecode reqRaw; handle @api x (handlers state) of
          Left err -> hPutStrLn stderr err
          Right io -> atomically . writeTChan (jobs state) $ Job (liftIO io)
      _ -> do
        -- mapM_ (appendFile debugOutput) [ln0, ln1]
        hPutStrLn stderr ("unexpected inputs: " <> ln0 <> ln1)
 where
  -- debugOutput = "debugOutput.txt"
  failureHandlers = [Exception.Handler ioExcept, Exception.Handler someExcept]

  ioExcept (e :: Exception.IOException) = print e >> exitFailure
  someExcept (e :: Exception.SomeException) = print e >> exitFailure

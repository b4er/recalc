{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Recalc.Server
Description : Generic JSON-RPC backend with a Servant-like API.

This module implements a reactor pattern for `Recalc`, ensuring asynchronous
processing of spreadsheet updates and computations. A single threaded loop
listens for JSON-RPC messages and maintains jobs.

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
  , debug
  , dumpEngineState
  , scheduleJob
  , runHandler
  , module Recalc.Server.Generic
  , module Recalc.Server.Types
  , aesonOptions
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict qualified as State
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as LB
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.IO.Error (isEOFError)
import Text.Read (readMaybe)

import Recalc.Server.Generic
import Recalc.Server.Json (aesonOptions)
import Recalc.Server.Types

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

scheduleJob :: ReaderT (State engine) IO () -> Handler engine ()
scheduleJob x = liftIO . atomically . (`writeTChan` Job x) =<< asks jobs

liftEngine :: State.State engine a -> Handler engine a
liftEngine f = liftIO . atomically . (`stateTVar` State.runState f) =<< asks engine

debug :: Show a => String -> a -> Handler engine ()
debug message dat = liftIO $ hPutStrLn stderr (message <> ": " <> show dat)

dumpEngineState :: Show engine => Handler engine ()
dumpEngineState = liftIO . (hPrint stderr <=< readTVarIO) =<< asks engine

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
    (ln0, ln1) <- (,) <$> getLine' <*> getLine
    case (readMaybe $ drop (length @[] "Content-Length: ") ln0, ln1) of
      (Just k, "\r") -> do
        reqRaw <- LB.hGet stdin k
        case do x <- Json.eitherDecode reqRaw; handle @api x (handlers state) of
          Left err -> hPutStrLn stderr err
          Right io -> atomically . writeTChan (jobs state) $ Job (liftIO io)
      _ -> do
        hPutStrLn stderr ("unexpected inputs: " <> ln0 <> ln1)
 where
  failureHandlers = [Exception.Handler ioExcept, Exception.Handler someExcept]

  ioExcept (e :: Exception.IOException) = hPrint stderr e >> exitFailure
  someExcept (e :: Exception.SomeException) = hPrint stderr e >> exitFailure

  getLine' =
    getLine `Exception.catch` \case
      err
        | isEOFError err -> exitSuccess
        | otherwise -> exitFailure

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (liftIO, runReaderT)
import Data.Aeson qualified as Json

import Recalc.Server
import Recalc.Server.Protocol
import System.IO (hPutStrLn, stderr)

type EngineState = ()

type SheetsApi = ToApi SpreadsheetProtocol

main :: IO ()
main = runHandler @SheetsApi () $ \state ->
  hoist @SheetsApi (`runReaderT` state) (namedHandlers server)
 where
  server :: SpreadsheetProtocol (AsServerT (Handler EngineState))
  server =
    SpreadsheetProtocol
      { rpcOpen = Main.rpcOpen
      , rpcClose = Main.rpcClose
      }

rpcOpen :: (Maybe Id, OpenParams) -> Handler EngineState Json.Value
rpcOpen (i, p) =
  Json.String "ok"
    <$ liftIO (hPutStrLn stderr $ "'open' not implemented " <> show (i, p))

rpcClose :: (Maybe Id, CloseParams) -> Handler EngineState ()
rpcClose _ = fail "'close' not implemented"

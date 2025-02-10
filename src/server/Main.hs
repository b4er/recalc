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
      , rpcSave = Main.rpcSave
      , rpcClose = Main.rpcClose
      , rpcSetRangeValues = Main.rpcSetRangeValues
      , rpcInsertSheet = Main.rpcInsertSheet
      , rpcRemoveSheet = Main.rpcRemoveSheet
      , rpcSetWorksheetOrder = Main.rpcSetWorksheetOrder
      , rpcSetWorksheetName = Main.rpcSetWorksheetName
      }

rpcOpen :: (Maybe Id, OpenParams) -> Handler EngineState Json.Value
rpcOpen (i, p) =
  Json.String "ok"
    <$ liftIO (hPutStrLn stderr $ "'open' not implemented " <> show (i, p))

rpcSave :: (Maybe Id, SaveParams) -> Handler EngineState ()
rpcSave _ = fail "'save' not implemented"

rpcClose :: (Maybe Id, CloseParams) -> Handler EngineState ()
rpcClose _ = fail "'close' not implemented"

rpcSetRangeValues :: (Maybe Id, SetRangeValuesParams) -> Handler EngineState Cells
rpcSetRangeValues _ = fail "'setRangeValues' not implemented"

rpcInsertSheet :: (Maybe Id, InsertSheetParams) -> Handler EngineState ()
rpcInsertSheet _ = fail "'rpcInsertSheet' not implemented"

rpcRemoveSheet :: (Maybe Id, RemoveSheetParams) -> Handler EngineState ()
rpcRemoveSheet _ = fail "'rpcRemoveSheet' not implemented"

rpcSetWorksheetOrder :: (Maybe Id, SetWorksheetOrderParams) -> Handler EngineState ()
rpcSetWorksheetOrder _ = fail "'rpcSetWorksheetOrder' not implemented"

rpcSetWorksheetName :: (Maybe Id, SetWorksheetNameParams) -> Handler EngineState ()
rpcSetWorksheetName _ = fail "'rpcSetWorksheetName' not implemented"

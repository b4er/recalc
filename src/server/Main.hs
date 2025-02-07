module Main where

import Control.Monad.Reader (runReaderT)
import Data.Aeson qualified as Json

import Recalc.Server
import Recalc.Server.Protocol

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
rpcOpen _ = fail "'open' not implemented"

rpcClose :: (Maybe Id, CloseParams) -> Handler EngineState ()
rpcClose _ = fail "'close' not implemented"

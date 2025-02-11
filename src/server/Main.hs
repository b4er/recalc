{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.List (foldl', sortOn)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, parse)

import Control.Arrow (Arrow (first, second))
import Debug.Trace (traceShowId)
import List_add
import Recalc.Engine qualified as Engine
import Recalc.Server
import Recalc.Server.Protocol

-- | the 'EngineState' describes which additional state per element in the
-- 'DocumentStore' we keep
type EngineState = Engine.EngineState DocState SheetName CellState Term

type DocumentStore = Engine.DS DocState SheetName CellState Term

-- | the document tracks the sheet order
type DocState = [(Text, Text)]

type SheetName = Text

type CellState = Engine.MetaOf CellData

data Term = Term deriving (Show)
data Value = Value deriving (Show)

instance Engine.Language Term where
  deps _ = mempty

  type EnvOf Term = Engine.SheetId
  newEnv = id

  type ErrorOf Term = ()
  type ValueOf Term = Value

  infer _ = throwError (Engine.OtherError ())
  inferValue = pure

  eval _ = pure Value

instance Engine.Input CellData where
  type TermOf CellData = Term

  type MetaOf CellData = CellData -- do we need all?
  metaOf = id

  exprOrValueOf loc CellData{..} = case (cellData'f, cellData'v) of
    (Is formula, _) -> Just (Left <$> parseFormula loc formula)
    (_, Is value) -> Just (Right <$> parseValue loc value)
    _ -> Nothing
   where
    parseFormula :: String -> Text -> Engine.Parsed Term
    parseFormula _ _ = pure Term

    parseValue :: String -> Text -> Engine.Parsed Value
    parseValue _ "12" = pure Value
    parseValue _ _ = case parse eof loc "jaja" of
      Left l -> Left l
      _ -> error "what"

type SheetsApi = ToApi SpreadsheetProtocol

main :: IO ()
main = runHandler @SheetsApi Engine.newEngineState $ \state ->
  hoist @SheetsApi (`runReaderT` state) (namedHandlers server)
 where
  -- \| ignore request id
  params = snd

  server :: SpreadsheetProtocol (AsServerT (Handler EngineState))
  server =
    SpreadsheetProtocol
      { rpcOpen = Main.rpcOpen . params
      , rpcSave = Main.rpcSave . params
      , rpcClose = Main.rpcClose . params
      , rpcSetRangeValues = Main.rpcSetRangeValues . params
      , rpcInsertSheet = Main.rpcInsertSheet . params
      , rpcRemoveSheet = Main.rpcRemoveSheet . params
      , rpcSetWorksheetOrder = Main.rpcSetWorksheetOrder . params
      , rpcSetWorksheetName = Main.rpcSetWorksheetName . params
      }

modifyDocs :: (DocumentStore -> DocumentStore) -> Handler EngineState ()
modifyDocs = liftEngine . Engine.modifyDocs

renderValue :: Value -> Text
renderValue _ = "(Value)"

{- JSON-RPC Handlers -}

rpcOpen :: OpenParams -> Handler EngineState ()
rpcOpen OpenParams{open'uri = uri, open'sheetOrder = sheetOrder} = do
  -- insert the document at uri, then insert each sheet
  let insertDocAndSheets =
        foldl'
          (\f (s, n) -> Engine.insertSheet (uri, s) n . f)
          (Engine.insertDocument uri sheetOrder)
          sheetOrder
  modifyDocs insertDocAndSheets

rpcSave :: SaveParams -> Handler EngineState ()
rpcSave _ = do
  dumpEngineState
  fail "'save' not implemented"

rpcClose :: CloseParams -> Handler EngineState ()
rpcClose _ = fail "'close' not implemented"

rpcSetRangeValues :: SetRangeValuesParams -> Handler EngineState Cells
rpcSetRangeValues SetRangeValuesParams{setRangeValues'cells = Cells rcMap, ..} = do
  let
    sheetId = (setRangeValues'uri, setRangeValues'sheetId)
    (metaChanges, (errors, values, formulas)) =
      Engine.validateCells sheetId (Map.assocs rcMap)

    allMetaChanges =
      metaChanges
        <> [(ca, m) | (ca, m, _) <- errors]
        <> [(ca, m) | (ca, m, _) <- values]
        <> [(ca, m) | (ca, m, _) <- formulas]

  scheduleJob
    $
    -- update meta-changes and recompute
    liftEngine
      ( do
          Engine.updateMeta sheetId allMetaChanges
          Engine.recompute @CellData sheetId (errors, values, formulas)
      )
      -- send back the results
      >>= liftIO . sendResult . traceShowId . \case
        Left cycle' -> [(ca, cellCyclicalError) | ca <- cycle']
        Right okChanges ->
          [ either ((ca,) . cellFetchError) ((ca,) . cellValue) x
          | (ca, x) <- okChanges
          ]

  -- return syntax errors
  pure
    $ Cells
    $ Map.fromList
      [ (ca, cellSyntaxError (Text.pack (errorBundlePretty err)))
      | (ca, _, err) <- errors
      ]
 where
  sendResult = sendIO . JsonRpcNotification "setCells" . nestedMap

  nestedMap
    :: [((Engine.SheetId, Engine.CellAddr), CellData)]
    -> Map.Map Engine.URI (Map.Map Text (Map.Map Engine.CellAddr CellData))
  nestedMap =
    Map.fromList
      . map
        ( second
            $ Map.fromList
              -- filter only cell address make map
              . map (second $ Map.fromList . map (first snd))
              . quotientOn' (snd . fst . fst) -- quotient by sheet
        )
      . quotientOn' (fst . fst . fst) -- quotient by uri
  quotientOn' f = quotientOn f . sortOn f

  cellErrors = CellData Missing Missing Missing Missing Missing . Is . (`CustomData` [])
  cellValue v = CellData Missing (Is (renderValue v)) Missing Missing Missing Missing

  cellCyclicalError = cellErrors [Annotation "Cyclic Dependency" "This cell is part of a cycle."]
  cellFetchError err = cellErrors [Annotation "Semantic Error" (Text.pack (show err))]
  cellSyntaxError err = cellErrors [Annotation "Syntax Error" err]

rpcInsertSheet :: InsertSheetParams -> Handler EngineState ()
rpcInsertSheet InsertSheetParams{..} =
  modifyDocs
    $ Engine.updateDocument
      insertSheet'uri
      (insertAt insertSheet'index (insertSheet'sheetId, insertSheet'sheetName))

rpcRemoveSheet :: RemoveSheetParams -> Handler EngineState ()
rpcRemoveSheet RemoveSheetParams{..} =
  modifyDocs
    $ Engine.updateDocument removeSheet'uri (removeAt removeSheet'sheetId)

rpcSetWorksheetOrder :: SetWorksheetOrderParams -> Handler EngineState ()
rpcSetWorksheetOrder SetWorksheetOrderParams{..} =
  modifyDocs
    $ Engine.updateDocument
      setWorksheetOrder'uri
      (moveList setWorksheetOrder'from setWorksheetOrder'to)

rpcSetWorksheetName :: SetWorksheetNameParams -> Handler EngineState ()
rpcSetWorksheetName SetWorksheetNameParams{..} =
  modifyDocs
    $ Engine.updateDocument
      setWorksheetName'uri
      (updateList setWorksheetName'sheetId setWorksheetName'sheetName)

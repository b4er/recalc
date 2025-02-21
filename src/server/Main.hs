{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Arrow (first, second)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.List (foldl', sortOn)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec (ParseErrorBundle, eof, parse)

import List_add
import Recalc.Engine qualified as Engine
import Recalc.Semantics (semanticErrorTitle, valueP)
import Recalc.Server
import Recalc.Server.Protocol
import Recalc.Syntax.Parser (Parser, formulaP)
import Recalc.Syntax.Term

-- | the 'EngineState' describes which additional state per element in the
-- 'DocumentStore' we keep
type EngineState = Engine.EngineState DocState SheetName CellState (Term Infer)

type DocumentStore = Engine.DS DocState SheetName CellState (Term Infer)

-- | the document tracks the sheet order
type DocState = [(Text, Text)]

type SheetName = Text

type CellState = Engine.MetaOf CellData

instance Engine.Input CellData where
  type TermOf CellData = (Term Infer)

  type MetaOf CellData = CellData -- do we need all?
  metaOf = id

  termOrValueOf sheetId ca CellData{..} = case (cellData'f, cellData'v) of
    (Is formula, _) -> Just (Left <$> parse' formulaP formula)
    (_, Is value) -> Just (Right <$> parse' valueP value)
    _ -> Nothing
   where
    parse' :: Parser a -> Text -> Either (ParseErrorBundle String Void) a
    parse' p = parse (runReaderT p sheetId <* eof) (showExcel26 ca) . Text.unpack

type SheetsApi = ToApi SpreadsheetProtocol

main :: IO ()
main = runHandler @SheetsApi Engine.newEngineState $ \state ->
  hoist @SheetsApi (`runReaderT` state) (namedHandlers server)
 where
  -- ignore request id
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
      , rpcDefineFunction = Main.rpcDefineFunction . params
      }

modifyDocs :: (DocumentStore -> DocumentStore) -> Handler EngineState ()
modifyDocs = liftEngine . Engine.modifyDocs

renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

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

    isValue ca = case Map.lookup ca rcMap of
      Just CellData{cellData'v = Is _} -> True
      _ -> False

  scheduleJob
    $
    -- update meta-changes and recompute
    liftEngine
      ( do
          Engine.updateMeta sheetId allMetaChanges
          Engine.recompute @CellData sheetId (errors, values, formulas)
      )
      -- send back the results
      >>= liftIO . sendResult . \case
        Left cycle' -> [(cref, cellCyclicalError) | cref <- cycle']
        Right okChanges ->
          [ either
            ((cref,) . cellFetchError (if isValue ca then Missing else Is "#error"))
            ((cref,) . cellValue)
            x
          | (cref@(_, ca), x) <- okChanges
          ]

  -- return syntax errors
  pure
    $ Cells
    $ Map.fromList
      [ (ca, cellFetchError xv (Engine.InvalidFormula @(Engine.ErrorOf (Term Infer)) err))
      | (ca, _, err) <- errors
      , -- only write value for formulas!
      let xv = if isValue ca then Missing else Is "#syntax"
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

  -- Univer cell data construtors
  cellValue v = CellData Missing (Is (renderPretty v)) Missing Missing Missing Missing

  cellErrors' xv = CellData Missing xv Missing Missing Missing . Is . (`CustomData` [])

  cellCyclicalError = cellErrors' (Is "cycle") [Annotation "Cyclic Dependency" "This cell is part of a cycle."]
  cellFetchError xv err = cellErrors' xv [Annotation (Engine.errorTitle semanticErrorTitle err) (renderPretty err)]

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

rpcDefineFunction :: DefineFunctionParams -> Handler EngineState ()
rpcDefineFunction def@DefineFunctionParams{} = debug "rpcDefineFunction" def

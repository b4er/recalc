{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recalc.Univer (Annotation (..), UniverError (..), univerMain) where

import Control.Monad.Reader (MonadIO (liftIO), runReaderT)
import Control.Monad.State (modify, state)
import Data.Bifunctor (bimap, second)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceShowM)
import Network.URI (URI)
import Prettyprinter (Pretty)
import Text.Megaparsec (errorBundlePretty)

import Recalc.Engine
import Recalc.Server
import Recalc.Univer.Internal
import Recalc.Univer.Protocol

class UniverError err where
  errorAnnotation :: err -> Annotation

type SheetsApi = ToApi SpreadsheetProtocol

univerMain
  :: forall t
   . (Recalc t, Pretty t, Show (ErrorOf t), UniverError (ErrorOf t))
  => EnvOf t
  -> IO ()
univerMain env = runHandler @SheetsApi (newEngineState env) $ \st ->
  hoist @SheetsApi (`runReaderT` st) (namedHandlers server)
 where
  -- ignore request id
  params = snd

  server :: SpreadsheetProtocol (AsServerT (Handler (EngineStateOf t)))
  server =
    SpreadsheetProtocol
      { rpcOpen = Recalc.Univer.rpcOpen . params
      , rpcSave = Recalc.Univer.rpcSave . params
      , rpcClose = Recalc.Univer.rpcClose . params
      , rpcSetRangeValues = Recalc.Univer.rpcSetRangeValues . params
      , rpcInsertSheet = Recalc.Univer.rpcInsertSheet . params
      , rpcRemoveSheet = Recalc.Univer.rpcRemoveSheet . params
      , rpcSetWorksheetOrder = Recalc.Univer.rpcSetWorksheetOrder . params
      , rpcSetWorksheetName = Recalc.Univer.rpcSetWorksheetName . params
      , rpcDefineFunction = Recalc.Univer.rpcDefineFunction . params
      }

modifyDocs
  :: (DocumentStore err t v -> DocumentStore err t v) -> Handler (EngineState env err t v) ()
modifyDocs = liftEngine . modify . mapDocs

modifyDocument
  :: URI
  -> ([Text] -> [Text])
  -> (Map Text (Sheet err t v) -> Map Text (Sheet err t v))
  -> Handler (EngineState env err t v) ()
modifyDocument uri f g =
  modifyDocs
    $ Map.update
      (\(Document sheetOrder sheets) -> Just $ Document (f sheetOrder) (g sheets))
      uri

{- JSON-RPC Handlers -}

rpcOpen :: OpenParams -> Handler (EngineState env err t v) ()
rpcOpen OpenParams{open'uri = uri, open'sheetOrder = sheetOrder} = do
  -- insert the document at uri & initialise each sheet
  let
    insertDocAndSheets =
      Map.insert uri . Document sheetOrder
        $ appEndo (foldMap (Endo . (`Map.insert` mempty)) sheetOrder) mempty
  modifyDocs insertDocAndSheets

rpcSave :: (Show err, Pretty t, Pretty v) => SaveParams -> Handler (EngineState env err t v) ()
rpcSave _ = dumpEngineState >> fail "'save' not implemented"

rpcClose :: CloseParams -> Handler (EngineState env err t v) ()
rpcClose _ = fail "'close' not implemented"

rpcSetRangeValues
  :: (Recalc t, UniverError (ErrorOf t))
  => SetRangeValuesParams
  -> Handler (EngineStateOf t) Cells
rpcSetRangeValues SetRangeValuesParams{setRangeValues'cells = Cells rcMap, ..} = do
  traceShowM ("set" :: String, rcMap)

  scheduleJob $ do
    let
      sheetId = (setRangeValues'uri, setRangeValues'sheetId)

      inputs :: [(CellRef, (Maybe (String, CellType), Meta))]
      inputs = map (bimap (sheetId,) unpackCellData) (Map.toList rcMap)

    liftEngine (state (recalc inputs)) >>= liftIO . either sendResult sendResult

  pure (Cells mempty)

sendResult
  :: (Recalc t, UniverError (ErrorOf t)) => [(CellRef, Cell (ErrorOf t) t (ValueOf t))] -> IO ()
sendResult = sendIO . JsonRpcNotification "setCells" . map (second packCell)

rpcInsertSheet :: InsertSheetParams -> Handler (EngineState env err t v) ()
rpcInsertSheet InsertSheetParams{..} =
  modifyDocument
    insertSheet'uri
    (insertAt insertSheet'index insertSheet'sheetName)
    (Map.insert insertSheet'sheetName mempty)

rpcRemoveSheet :: RemoveSheetParams -> Handler (EngineState env err t v) ()
rpcRemoveSheet RemoveSheetParams{..} =
  modifyDocument
    removeSheet'uri
    (filter (/= removeSheet'sheetName))
    (Map.delete removeSheet'sheetName)

rpcSetWorksheetOrder :: SetWorksheetOrderParams -> Handler (EngineState env err t v) ()
rpcSetWorksheetOrder SetWorksheetOrderParams{..} =
  modifyDocument
    setWorksheetOrder'uri
    (moveList setWorksheetOrder'from setWorksheetOrder'to)
    id

rpcSetWorksheetName :: SetWorksheetNameParams -> Handler (EngineState env err t v) ()
rpcSetWorksheetName SetWorksheetNameParams{..} =
  modifyDocument
    setWorksheetName'uri
    (updateList setWorksheetName'sheetName setWorksheetName'newName)
    id

rpcDefineFunction :: DefineFunctionParams -> Handler (EngineState env err t v) ()
rpcDefineFunction def@DefineFunctionParams{} = debug "rpcDefineFunction" def

{- Serialisation -}

packCell
  :: forall t. (Recalc t, UniverError (ErrorOf t)) => Cell (ErrorOf t) t (ValueOf t) -> CellData
packCell Cell{..} =
  CellData
    Missing
    ( case cell of
        Nothing -> Missing
        Just ((_, CellValue), _) -> Missing
        Just ((_, CellFormula), content) -> Is
          $ case cellError of
            Just InvalidFormula{} -> "#SYNTAX"
            Just RefError -> "#REF"
            Just SemanticError{} -> "#ERROR"
            Nothing -> case content of
              Just (_term, Just (_typ, Just val)) -> renderPretty val
              _ -> "?ERROR"
    )
    Missing
    Missing
    Missing
    ( Is
        ( case (cell, cellError) of
            -- given an error, set the error
            (_, Just err) -> CustomData [fetchErrorAnnotation @t err] [] []
            -- given a typing, set the type annotation
            -- (regardless of whether it is a formula or value)
            (Just (_, Just (_, Just (ty, _))), _) ->
              CustomData [] [] [Annotation "" (renderPretty ty)]
            -- there should not be another state
            _ -> CustomData [] [Annotation "Invalid State" "This cell is in an invalid/unknown state."] []
        )
    )

fetchErrorAnnotation
  :: forall t. (Recalc t, UniverError (ErrorOf t)) => FetchError (ErrorOf t) -> Annotation
fetchErrorAnnotation = \case
  InvalidFormula err -> Annotation "Syntax Error" (Text.pack (errorBundlePretty err))
  RefError -> Annotation "Invalid Reference" ""
  SemanticError err -> errorAnnotation err

unpackCellData :: CellData -> (Maybe (String, CellType), Meta)
unpackCellData CellData{..} = case (cellData'f, cellData'v) of
  (Is f, _) -> (Just (Text.unpack f, CellFormula), Meta)
  (_, Is v) -> (Just (Text.unpack v, CellValue), Meta)
  _ -> (Nothing, Meta)

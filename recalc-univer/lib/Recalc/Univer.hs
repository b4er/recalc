{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.Univer
Description : Type class and generic language server implementation (backend for a Univer frontend).

This module defines the interface "UniverRecalc" for which 'univerMain' is defined:

@
univerMain @Term (env0 :: EnvOf Term) :: IO ()
@

which will run a spreadsheet language server serving a Univer frontend.
-}
module Recalc.Univer
  ( Annotation (..)
  , FunctionDescription (..)
  , FunctionParameter (..)
  , FunctionType (..)
  , UniverRecalc (..)
  , univerMain
  ) where

import Control.Monad.Reader (MonadIO (liftIO), runReaderT)
import Control.Monad.State (gets, modify, state)
import Data.Bifunctor (bimap, second)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (URI)
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec (errorBundlePretty)

import Recalc.Engine
import Recalc.Server
import Recalc.Univer.Internal
import Recalc.Univer.Protocol

class (Recalc t, Pretty t, Show (ErrorOf t)) => UniverRecalc t where
  errorAnnotation :: ErrorOf t -> Annotation

  define
    :: Text
    -> Map CellAddr (Maybe ((String, CellType), Maybe (t, Maybe (TypeOf t, Maybe (ValueOf t)))))
    -> [(Text, CellRange)]
    -> CellRange
    -> EnvOf t
    -> Either (ErrorOf t) (EnvOf t)
  define _ _ _ _ = Right

type SheetsApi = ToApi SpreadsheetProtocol

univerMain :: forall t. UniverRecalc t => EnvOf t -> IO ()
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
  :: (DocumentStoreOf t -> DocumentStoreOf t) -> Handler (EngineStateOf t) ()
modifyDocs = liftEngine . modify . mapDocs

modifyDocument
  :: URI
  -> ([Text] -> [Text])
  -> (Map Text (SheetOf t) -> Map Text (SheetOf t))
  -> Handler (EngineStateOf t) ()
modifyDocument uri f g =
  modifyDocs
    $ Map.update
      (\(Document sheetOrder sheets) -> Just $ Document (f sheetOrder) (g sheets))
      uri

{- JSON-RPC Handlers -}

rpcOpen :: OpenParams -> Handler (EngineStateOf t) ()
rpcOpen OpenParams{open'uri = uri, open'sheetOrder = sheetOrder} = do
  -- insert the document at uri & initialise each sheet
  let
    insertDocAndSheets =
      Map.insert uri . Document sheetOrder
        $ appEndo (foldMap (Endo . (`Map.insert` mempty)) sheetOrder) mempty
  modifyDocs insertDocAndSheets

rpcSave :: (Recalc t, Show (ErrorOf t)) => SaveParams -> Handler (EngineStateOf t) ()
rpcSave _ = dumpEngineState >> fail "'save' not implemented"

rpcClose :: CloseParams -> Handler (EngineStateOf t) ()
rpcClose _ = fail "'close' not implemented"

rpcSetRangeValues
  :: UniverRecalc t
  => SetRangeValuesParams
  -> Handler (EngineStateOf t) Cells
rpcSetRangeValues SetRangeValuesParams{setRangeValues'cells = Cells rcMap, ..} = do
  scheduleJob $ do
    let
      sheetId = (setRangeValues'uri, setRangeValues'sheetName)

      inputs :: [(CellRef, (Maybe (String, CellType), Meta))]
      inputs = map (bimap (sheetId,) unpackCellData) (Map.toList rcMap)

    liftEngine (state (recalc inputs)) >>= liftIO . either sendResult sendResult

  pure (Cells mempty)

rpcInsertSheet :: InsertSheetParams -> Handler (EngineStateOf t) ()
rpcInsertSheet InsertSheetParams{..} =
  modifyDocument
    insertSheet'uri
    (insertAt insertSheet'index insertSheet'sheetName)
    (Map.insert insertSheet'sheetName mempty)

rpcRemoveSheet :: RemoveSheetParams -> Handler (EngineStateOf t) ()
rpcRemoveSheet RemoveSheetParams{..} =
  modifyDocument
    removeSheet'uri
    (filter (/= removeSheet'sheetName))
    (Map.delete removeSheet'sheetName)

rpcSetWorksheetOrder :: SetWorksheetOrderParams -> Handler (EngineStateOf t) ()
rpcSetWorksheetOrder SetWorksheetOrderParams{..} =
  modifyDocument
    setWorksheetOrder'uri
    (moveList setWorksheetOrder'from setWorksheetOrder'to)
    id

rpcSetWorksheetName
  :: UniverRecalc t => SetWorksheetNameParams -> Handler (EngineStateOf t) ()
rpcSetWorksheetName SetWorksheetNameParams{..} = do
  modifyDocument
    setWorksheetName'uri
    (updateList setWorksheetName'sheetName setWorksheetName'newName)
    renameSheet
  -- recompute everything & clear the whole sheet from the deps
  -- (since parsed cells may refer to the sheet)
  let sheetId = (setWorksheetName'uri, setWorksheetName'sheetName)
  scheduleJob
    $ liftEngine (state (second (deleteSheetId sheetId) . recalcAll))
      >>= liftIO . either sendResult sendResult
 where
  renameSheet m = case Map.lookup setWorksheetName'sheetName m of
    Just el -> Map.insert setWorksheetName'newName el (Map.delete setWorksheetName'sheetName m)
    _ -> m

rpcDefineFunction
  :: forall t
   . UniverRecalc t
  => DefineFunctionParams
  -> Handler (EngineStateOf t) (Either Text [FunctionDescription])
rpcDefineFunction DefineFunctionParams{..}
  -- make sure the inputs don't overlap the output
  | null errors = liftEngine $ do
      -- retrieve document to call @define@ with, modify state accordingly (@id@ when error)
      let getSheet = \case
            Just r -> case Map.lookup defineFunction'sheetName (sheets r) of
              Just x -> Map.map cell x
              _ -> mempty
            _ -> mempty
      sheet <- gets (getSheet . Map.lookup defineFunction'uri . engineDocs)
      state $ \st ->
        let
          env = engineEnv st
          def = define @t defineFunction'sheetName sheet defineFunction'inputs defineFunction'output
        in
          case def env of
            Left err ->
              let
                Annotation _ msg = errorAnnotation @t err
              in
                (Left $ "Cannot save function '" <> defineFunction'sheetName <> "': " <> msg, st)
            Right env' -> (Right [functionDescription], mapEnv (const env') st)
  -- overlapping input(s) with output
  | otherwise =
      pure . Left
        $ ( "Cannot save function '"
              <> defineFunction'sheetName
              <> "' because inputs and output overlap: "
              <> showt (map (second (bimap showExcel26 showExcel26)) errors)
          )
 where
  functionDescription =
    FunctionDescription
      defineFunction'sheetName
      User
      defineFunction'description
      "abstract"
      [ FunctionParameter inputName "example" "detail"
      | (inputName, _range) <- defineFunction'inputs
      ]

  errors = filter (intersects defineFunction'output . snd) defineFunction'inputs

  intersects :: CellRange -> CellRange -> Bool
  intersects ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) =
    not (x1 < x0' || x0 > x1' || y1 < y0' || y0 > y1')

{- IO & Serialisation -}

sendResult :: UniverRecalc t => [(CellRef, CellOf t)] -> IO ()
sendResult = sendIO . JsonRpcNotification "setRangeValues" . map (second packCell)

-- sendError, sendInfo :: Text -> IO ()
-- sendError = sendIO . JsonRpcNotification "error"
-- sendInfo = sendIO . JsonRpcNotification "info"

showt :: Show a => a -> Text
showt = Text.pack . show

renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

packCell :: forall t. UniverRecalc t => CellOf t -> CellData
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

fetchErrorAnnotation :: forall t. UniverRecalc t => FetchError (ErrorOf t) -> Annotation
fetchErrorAnnotation = \case
  InvalidFormula err -> Annotation "Syntax Error" (Text.pack (errorBundlePretty err))
  RefError -> Annotation "Invalid Reference" ""
  SemanticError err -> errorAnnotation @t err

unpackCellData :: CellData -> (Maybe (String, CellType), Meta)
unpackCellData CellData{..} = case (cellData'f, cellData'v) of
  (Is f, _) -> (Just (Text.unpack f, CellFormula), Meta)
  (_, Is v) -> (Just (Text.unpack v, CellValue), Meta)
  _ -> (Nothing, Meta)

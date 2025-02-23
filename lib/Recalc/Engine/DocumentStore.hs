{-|
Module      : Recalc.Engine.DocumentStore
Description : Provides a DocumentStore tracks all spreadsheets for a session.

This module exports the core interface interacting with a 'DocumentStore'. The
store maps a 'URI' to 'Document's, documents contain named 'Sheet's which in
turn maps 'CellAddr's to a cell.

The document storage is abstract and supports arbitrary meta-data. Types and
values are assumed to coincide (dependent types).
-}
module Recalc.Engine.DocumentStore
  ( -- * Tracking Documents
    DocumentStore
  , newDocumentStore
  , cellError
  , cellTerm
  , cellValue

    -- * Type Classes
  , Isn't (..)
  , Meta (..)

    -- * Setters
  , insertDocument
  , insertSheet
  , deleteSheet
  , setCell
  , setCellError
  , setCellType
  , setCellValue

    -- * Updates
  , alterCellMeta
  , updateDocument
  , updateSheet

    -- * Lookups
  , lookupCellDeps
  , lookupCellTerm
  , lookupCellType
  , lookupCellValue
  ) where

import Control.Monad ((<=<))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text qualified as Text

import Recalc.Engine.Core (CellAddr, CellRange, SheetId)
import Recalc.Engine.Language hiding (Cell)

-- | The document store keeps a 'Document' for each resource id.
newtype DocumentStore doc sheet cell term value err
  = DocumentStore
      (Map URI (Document doc sheet cell term value err))

instance
  (Show doc, Show sheet, Show cell, Show term, Show value, Show err)
  => Show (DocumentStore doc sheet cell term value err)
  where
  show (DocumentStore m) = show m

newDocumentStore :: DocumentStore doc sheet cell term value err
newDocumentStore = DocumentStore mempty

-- | A document consists of multiple named 'Sheet's
newtype Document doc sheet cell term value err
  = Document (Map Text (Sheet sheet cell term value err), doc)
  deriving (Show)

-- | A sheet stores all cells
newtype Sheet sheet cell term value err = Sheet (Map CellAddr (Cell cell term value err), sheet)
  deriving (Show)

-- | Each cell stores its content, pre-computed dependencies and meta data
data Cell cell term value err = Cell
  { content :: CellContent term value err
  , range :: Set CellRange
  , cell :: cell
  }
  deriving (Show)

data CellContent term value err
  = CellTerm
      (Maybe value)
      -- ^ inferred type
      term
      -- ^ "raw" term
      (Maybe value)
      -- ^ computed value
  | CellValue
      (Maybe value)
      -- ^ inferred type
      value
      -- ^ value
  | -- | error
    CellError err
  deriving (Show)

{- smart ctors -}
cellTerm :: term -> Set CellRange -> cell -> Cell cell term value err
cellTerm e = Cell (CellTerm Nothing e Nothing)

cellValue :: Maybe value -> value -> cell -> Cell cell term value err
cellValue t v = Cell (CellValue t v) mempty

cellError :: err -> cell -> Cell cell term value err
cellError err = Cell (CellError err) mempty

{- setters -}

-- | Typeclass for values that "default-when-absent". For example,
-- "Bool" when not given should be "the same" as @False@. Used
-- for meta-data only.
class Isn't cell where
  isn't :: cell -> Bool

instance Isn't () where isn't _ = False
instance Isn't Bool where isn't = not
instance Isn't Int where isn't = (== 0)
instance Isn't Text where isn't = Text.null

-- | can be merged (à la "missing data kept, null removes it")
class Meta cell where
  merge :: cell -> cell -> cell

insertDocument
  :: URI
  -> doc
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
insertDocument uri doc (DocumentStore docs) =
  DocumentStore
    $ Map.insert uri (Document (mempty, doc)) docs

insertSheet
  :: SheetId
  -> sheet
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
insertSheet (uri, sheetId) s (DocumentStore docs) =
  DocumentStore
    $ Map.update
      ( Just . \case
          Document (d, doc) -> Document (Map.insert sheetId (Sheet (mempty, s)) d, doc)
      )
      uri
      docs

deleteSheet
  :: SheetId
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
deleteSheet (uri, sheetId) (DocumentStore docs) =
  DocumentStore
    $ Map.update
      ( Just . \case
          Document (d, doc) -> Document (Map.delete sheetId d, doc)
      )
      uri
      docs

alterCell
  :: Isn't cell
  => SheetId
  -> CellAddr
  -> (Maybe (Cell cell term value err) -> Maybe (Cell cell term value err))
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
alterCell (uri, sheetName) ca updateCell (DocumentStore docs) =
  DocumentStore (Map.update document uri docs)
 where
  document (Document (doc, x)) = Just $ Document (Map.update sheet sheetName doc, x)
  sheet (Sheet (cells, x)) = Just $ Sheet (Map.alter updateCell' ca cells, x)

  updateCell' c =
    let c' = updateCell c
    in  if maybe True (isn't . cell) c' then Nothing else c'

alterCellMeta
  :: Isn't cell
  => SheetId
  -> CellAddr
  -> (cell -> cell)
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
alterCellMeta sheetId ca updateMeta = alterCell sheetId ca $ \case
  Just c -> Just c{cell = updateMeta (cell c)}
  x -> x

setCell
  :: Isn't cell
  => SheetId
  -> CellAddr
  -> Cell cell term value err
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
setCell sheetId ca = alterCell sheetId ca . const . Just

setCellError
  :: Isn't cell
  => SheetId
  -> CellAddr
  -> err
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
setCellError sheetId ca err = alterCell sheetId ca $ \case
  Just Cell{..} -> Just Cell{content = CellError err, range = range, cell = cell}
  x -> x

setCellType
  :: Isn't cell
  => SheetId
  -> CellAddr
  -> value
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
setCellType sheetId ca ty = alterCell sheetId ca $ \case
  Just Cell{content = CellTerm _ e v, ..} -> Just Cell{content = CellTerm (Just ty) e v, range = range, cell = cell}
  Just Cell{content = CellValue _ v, ..} -> Just Cell{content = CellValue (Just ty) v, range = range, cell = cell}
  x -> x

setCellValue
  :: Isn't cell
  => SheetId
  -> CellAddr
  -> value
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
setCellValue sheetId ca v' = alterCell sheetId ca $ \case
  Just Cell{content = CellTerm ty e _, ..} -> Just Cell{content = CellTerm ty e (Just v'), range = range, cell = cell}
  x -> x

{- lookups -}

lookupCell'
  :: SheetId
  -> CellAddr
  -> DocumentStore doc sheet cell term value err
  -> Maybe (Cell cell term value err)
lookupCell' (uri, sheetName) ca (DocumentStore docs) = do
  Document (sheets, _) <- Map.lookup uri docs
  Sheet (sheet, _) <- Map.lookup sheetName sheets
  Map.lookup ca sheet

-- | Look up the pre-computed dependencies of a cell
lookupCellDeps
  :: SheetId -> CellAddr -> DocumentStore doc sheet cell term value err -> Maybe (Set CellRange)
lookupCellDeps sheetId ca = fmap range . lookupCell' sheetId ca

-- | Look up the term associated to a cell
lookupCellTerm
  :: SheetId -> CellAddr -> DocumentStore doc sheet cell term value err -> Maybe term
lookupCellTerm sheetId ca = unpack <=< lookupCell' sheetId ca
 where
  unpack = (\case CellTerm _ e _ -> Just e; _ -> Nothing) . content

lookupCellType
  :: SheetId -> CellAddr -> DocumentStore doc sheet cell term value err -> Maybe value
lookupCellType sheetId ca = unpack <=< lookupCell' sheetId ca
 where
  unpack = (\case CellTerm ty _ _ -> ty; CellValue ty _ -> ty; _ -> Nothing) . content

lookupCellValue
  :: SheetId -> CellAddr -> DocumentStore doc sheet cell term value err -> Maybe value
lookupCellValue sheetId ca = unpack <=< lookupCell' sheetId ca
 where
  unpack =
    ( \case
        CellValue _ v -> Just v
        CellTerm _ _ (Just v) -> Just v
        _ -> Nothing
    )
      . content

updateDocument
  :: URI
  -> (doc -> doc)
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
updateDocument uri f (DocumentStore docs) =
  DocumentStore
    $ (`Map.update` uri)
      (Just . \(Document (sheets, doc)) -> Document (sheets, f doc))
      docs

updateSheet
  :: SheetId
  -> (sheet -> sheet)
  -> DocumentStore doc sheet cell term value err
  -> DocumentStore doc sheet cell term value err
updateSheet (uri, sheetId) f (DocumentStore docs) =
  DocumentStore
    $ (`Map.update` uri)
      ( Just . \(Document (sheets, doc)) ->
          let
            sheets' = Map.update (Just . update) sheetId sheets
          in
            Document (sheets', doc)
      )
      docs
 where
  update (Sheet (m, x)) = Sheet (m, f x)

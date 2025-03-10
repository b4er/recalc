{-# LANGUAGE DataKinds #-}

{-|
Module      : Recalc.Server.Protocol where
Description : Named protocol implementation for the JSON-RPC api.
-}
module Recalc.Univer.Protocol where

import Control.Arrow (first, second)
import Data.Aeson qualified as Json
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI

import Recalc.Engine
import Recalc.Server

data SpreadsheetProtocol mode = SpreadsheetProtocol
  { rpcOpen :: mode :- JsonRpc "open" OpenParams ()
  , rpcSave :: mode :- JsonRpc "save" SaveParams ()
  , rpcClose :: mode :- JsonRpc "close" CloseParams ()
  , rpcSetRangeValues :: mode :- JsonRpc "setRangeValues" SetRangeValuesParams Cells
  , rpcInsertSheet :: mode :- JsonRpc "insertSheet" InsertSheetParams ()
  , rpcRemoveSheet :: mode :- JsonRpc "removeSheet" RemoveSheetParams ()
  , rpcSetWorksheetOrder :: mode :- JsonRpc "setWorksheetOrder" SetWorksheetOrderParams ()
  , rpcSetWorksheetName :: mode :- JsonRpc "setWorksheetName" SetWorksheetNameParams ()
  , rpcDefineFunction
      :: mode :- JsonRpc "defineFunction" DefineFunctionParams (Either Text [FunctionDescription])
  }
  deriving (Generic)

data OpenParams = OpenParams
  { open'uri :: URI
  , open'sheetOrder :: [Text]
  }
  deriving (Show, Generic)

data SaveParams = SaveParams {save'uri :: URI, save'asUri :: URI}
  deriving (Generic, Show)

newtype CloseParams = CloseParams {close'uri :: URI}
  deriving (Generic, Show)

data SetRangeValuesParams = SetRangeValuesParams
  { setRangeValues'uri :: URI
  , setRangeValues'sheetName :: Text
  , setRangeValues'cells :: Cells
  }
  deriving (Generic, Show)

data InsertSheetParams = InsertSheetParams
  { insertSheet'uri :: URI
  , insertSheet'index :: Int
  , insertSheet'sheetId :: Text
  , insertSheet'sheetName :: Text
  }
  deriving (Show, Generic)

data RemoveSheetParams = RemoveSheetParams
  { removeSheet'uri :: URI
  , removeSheet'sheetName :: Text
  }
  deriving (Show, Generic)

data SetWorksheetOrderParams = SetWorksheetOrderParams
  { setWorksheetOrder'uri :: URI
  , setWorksheetOrder'sheetName :: Text
  , setWorksheetOrder'from :: Int
  , setWorksheetOrder'to :: Int
  }
  deriving (Show, Generic)

instance Json.FromJSON SetWorksheetOrderParams where
  parseJSON = Json.genericParseJSON aesonOptions

data SetWorksheetNameParams = SetWorksheetNameParams
  { setWorksheetName'uri :: URI
  , setWorksheetName'sheetName :: Text
  , setWorksheetName'newName :: Text
  }
  deriving (Show, Generic)

data DefineFunctionParams = DefineFunctionParams
  { defineFunction'uri :: URI
  , defineFunction'sheetName :: Text
  , defineFunction'description :: Text
  , defineFunction'inputs :: [(Text, CellRange)]
  , defineFunction'output :: CellRange
  }
  deriving (Show, Generic)

-- | as defined in @\@univerjs/engine-formula#basics/function.ts@
data FunctionType
  = -- | Financial Functions
    Financial
  | -- | Date and Time Functions
    Date
  | -- | Math and Trigonometry Functions
    Math
  | -- | Statistical Functions
    Statistical
  | -- | Lookup and Reference Functions
    Lookup
  | -- | Database Functions
    Database
  | -- | Text Functions
    Text
  | -- | Logical Functions
    Logical
  | -- | Information Functions
    Information
  | -- | Engineering Functions
    Engineering
  | -- | Cube Functions
    Cube
  | -- | Compatibility Functions
    Compatibility
  | -- | Web Functions
    Web
  | -- | Array Functions
    Array
  | -- | Univer-specific functions
    Univer
  | -- | User-defined functions
    User
  | -- | Defined name
    DefinedName
  deriving (Show, Enum)

data FunctionParameter = FunctionParameter
  { parameter'name :: Text
  , parameter'detail :: Text
  , parameter'example :: Text
  }
  deriving (Show, Generic)

data FunctionDescription = FunctionDescription
  { functionDescription'name :: Text
  , functionDescription'type :: FunctionType
  , functionDescription'description :: Text
  , functionDescription'abstract :: Text
  , functionDescription'params :: [FunctionParameter]
  }
  deriving (Show, Generic)

{- Cells -}

data Annotation = Annotation
  { ann'title, ann'message :: Text
  }
  deriving (Eq, Generic, Show)

-- | keep errors, warnings for each cell
data CustomData = CustomData
  { customData'errors :: [Annotation]
  , customData'warnings :: [Annotation]
  , customData'info :: [Annotation]
  }
  deriving (Generic, Show)

-- instance Isn't CustomData where
--  isn't (CustomData es ws nfo) = null es && null ws && null nfo

newtype BooleanInt = BooleanInt {boolean :: Bool}

data CellStyle f = CellStyle
  { cellStyle'bl :: f BooleanInt
  -- ^ bold
  , cellStyle'it :: f BooleanInt
  -- ^ italic
  }
  deriving (Generic)

-- | corresponds to ICellData
-- (see: https://univer.ai/typedoc/@univerjs/core/interfaces/ICellData)
data CellData = CellData
  { cellData's :: Nullable (CellStyle Maybe)
  -- ^ Cell style id or style object
  , cellData'v :: Nullable Text
  -- ^ Cell original value
  , cellData'f :: Nullable Text
  -- ^ Formula
  , cellData'si :: Nullable Text
  -- ^ Formula id
  , cellData'p :: Nullable Text
  -- ^ Rich text, also a Univer Doc
  , cellData'custom :: Nullable CustomData
  -- ^ Custom field
  }
  deriving (Generic)

instance Show CellData where
  show = show . Json.encode

-- instance Isn't CellData where
--  isn't CellData{..} =
--    and
--      [ isn't cellData's
--      , isn't cellData'v
--      , isn't cellData'f
--      , isn't cellData'si
--      , isn't cellData'p
--      , isn't cellData'custom
--      ]

-- instance Meta CellData where
--  CellData s v f si p custom `merge` CellData s' v' f' si' p' custom' =
--    CellData
--      (s `merge` s')
--      (v `merge` v')
--      (f `merge` f')
--      (si `merge` si')
--      (p `merge` p')
--      (custom `merge` custom')

newtype Cells = Cells (Map (Int, Int) CellData)
  deriving (Show)

{- JSON -}

{-- Params --}

instance Json.FromJSON OpenParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON SaveParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON CloseParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON SetRangeValuesParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON InsertSheetParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON RemoveSheetParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON SetWorksheetNameParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.FromJSON DefineFunctionParams where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.ToJSON FunctionType where
  toJSON = Json.toJSON . fromEnum

instance Json.ToJSON FunctionParameter where
  toJSON = Json.genericToJSON aesonOptions

instance Json.ToJSON FunctionDescription where
  toJSON = Json.genericToJSON aesonOptions

{-- Cells --}

instance Json.FromJSON BooleanInt where
  parseJSON v = BooleanInt . (0 /=) <$> Json.parseJSON @Int v

instance Json.ToJSON BooleanInt where
  toJSON (BooleanInt b) = Json.toJSON @Int (if b then 1 else 0)

instance Json.FromJSON Annotation where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.ToJSON Annotation where
  toJSON = Json.genericToJSON aesonOptions

instance Json.FromJSON CustomData where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.ToJSON CustomData where
  toJSON = Json.genericToJSON aesonOptions

instance Json.FromJSON (CellStyle Maybe) where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.ToJSON (CellStyle Maybe) where
  toJSON = Json.genericToJSON aesonOptions

instance Json.FromJSON CellData where
  parseJSON = Json.genericParseJSON aesonOptions

instance Json.ToJSON CellData where
  toJSON = Json.genericToJSON aesonOptions

--

instance Json.ToJSON Cells where
  toJSON (Cells cells) =
    Json.toJSON
      . Map.fromList
      . map (second (foldMap snd))
      . quotientOn fst
      . sortOn fst
      -- [](j,[((i,j),x)]) -> [](j,Map i x)
      . map (second (Map.fromList . map (first row)))
      -- [((i,j),x)] -> [(j,[..])]
      . quotientOn (column . fst)
      $ Map.assocs cells

-- | assumes the input list is sorted and that the @repr@ function respects the order
quotientOn :: Eq q => (a -> q) -> [a] -> [(q, [a])]
quotientOn repr = map repack . groupBy ((==) `on` repr)
 where
  repack xs = (repr (head xs), xs)

instance Json.FromJSON Cells where
  parseJSON = fmap (Cells . flattenMap) . Json.parseJSON
   where
    flattenMap = Map.foldlWithKey' mergeMaps mempty

    mergeMaps m i = (m <>) . Map.foldlWithKey' (collectEntries i) mempty
    collectEntries i m j a = Map.insert (i, j) a m

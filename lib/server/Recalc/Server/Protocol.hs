{-# LANGUAGE DataKinds #-}

{-|
Module      : Recalc.Server.Protocol where
Description : Named protocol implementation for the json-rpc api.
-}
module Recalc.Server.Protocol where

import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics
import Network.URI

import Recalc.Server.Generic
import Recalc.Server.Json

data SpreadsheetProtocol mode = SpreadsheetProtocol
  { rpcOpen :: mode :- JsonRpc "open" OpenParams Json.Value
  , rpcSave :: mode :- JsonRpc "save" SaveParams ()
  , rpcClose :: mode :- JsonRpc "close" CloseParams ()
  , rpcSetRangeValues :: mode :- JsonRpc "setRangeValues" SetRangeValuesParams Cells
  , rpcInsertSheet :: mode :- JsonRpc "insertSheet" InsertSheetParams ()
  , rpcRemoveSheet :: mode :- JsonRpc "removeSheet" RemoveSheetParams ()
  , rpcSetWorksheetOrder :: mode :- JsonRpc "setWorksheetOrder" SetWorksheetOrderParams ()
  , rpcSetWorksheetName :: mode :- JsonRpc "setWorksheetName" SetWorksheetNameParams ()
  }
  deriving (Generic)

data OpenParams = OpenParams
  { open'uri :: URI
  , open'sheetOrder :: [(Text, Text)]
  }
  deriving (Show, Generic)

instance Json.FromJSON OpenParams where
  parseJSON = Json.genericParseJSON aesonOptions

newtype SaveParams = SaveParams {save'uri :: URI}
  deriving (Generic, Show)

instance Json.FromJSON SaveParams where
  parseJSON = Json.genericParseJSON aesonOptions

newtype CloseParams = CloseParams {close'uri :: URI}
  deriving (Generic, Show)

instance Json.FromJSON CloseParams where
  parseJSON = Json.genericParseJSON aesonOptions

type Cells =
  Json.Value
  -- ^ leave unspecified for now (FIXME)

data SetRangeValuesParams = SetRangeValuesParams
  { setRangeValues'uri :: URI
  , setRangeValues'sheetId :: Text
  , setRangeValues'cells :: Cells
  }
  deriving (Generic, Show)

instance Json.FromJSON SetRangeValuesParams where
  parseJSON = Json.genericParseJSON aesonOptions

data InsertSheetParams = InsertSheetParams
  { insertSheet'uri :: URI
  , insertSheet'index :: Int
  , insertSheet'sheetId :: Text
  , insertSheet'sheetName :: Text
  }
  deriving (Show, Generic)

instance Json.FromJSON InsertSheetParams where
  parseJSON = Json.genericParseJSON aesonOptions

data RemoveSheetParams = RemoveSheetParams
  { removeSheet'uri :: URI
  , removeSheet'sheetId :: Text
  }
  deriving (Show, Generic)

instance Json.FromJSON RemoveSheetParams where
  parseJSON = Json.genericParseJSON aesonOptions

data SetWorksheetOrderParams = SetWorksheetOrderParams
  { setWorksheetOrder'uri :: URI
  , setWorksheetOrder'sheetId :: Text
  , setWorksheetOrder'from :: Int
  , setWorksheetOrder'to :: Int
  }
  deriving (Show, Generic)

instance Json.FromJSON SetWorksheetOrderParams where
  parseJSON = Json.genericParseJSON aesonOptions

data SetWorksheetNameParams = SetWorksheetNameParams
  { setWorksheetName'uri :: URI
  , setWorksheetName'sheetId :: Text
  , setWorksheetName'sheetName :: Text
  }
  deriving (Show, Generic)

instance Json.FromJSON SetWorksheetNameParams where
  parseJSON = Json.genericParseJSON aesonOptions

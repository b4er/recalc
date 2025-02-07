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

data OpenParams = OpenParams
  { open'uri :: URI
  , open'sheetOrder :: [(Text, Text)]
  }
  deriving (Show, Generic)

instance Json.FromJSON OpenParams where
  parseJSON = Json.genericParseJSON aesonOptions

newtype CloseParams = CloseParams {close'uri :: URI}
  deriving (Generic, Show)

instance Json.FromJSON CloseParams where
  parseJSON = Json.genericParseJSON aesonOptions

data SpreadsheetProtocol mode = SpreadsheetProtocol
  { rpcOpen :: mode :- JsonRpc "open" OpenParams Json.Value
  , rpcClose :: mode :- JsonRpc "close" CloseParams ()
  }
  deriving (Generic)

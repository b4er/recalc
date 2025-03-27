{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Recalc.Server.Generic
Description : Servant-like combinators for defining and handling
              JSON-RPC APIs.

Provides a generic way to define and handle JSON-RPC APIs using
a Servant-like approach. Given a protocol type with named routes,
this module allows defining handlers generically.

=== Example:

@
data NamedApi mode = NamedApi
  { rpcGetValue :: mode :- JsonRpc "open" GetParams GetResult
  , rpcAddValue :: mode :- JsonRpc "close" AddParams ()
  } deriving (Generic)

type Api = ToApi NamedApi

data Api mode =
  { getValue :: mode :- JsonRpc "get" () Int
  , addValue :: mode :- JsonRpc "add" Int ()
  }

handlers :: HandlerT Api IO
handlers = namedHandlers server

server :: NamedApi Server
server = NamedApi { ... }
@

which can then be used to handle a @req :: JsonRpcRequest Json.Value@ using:

@
handle @Api req handlers :: Either String (IO ())
@
-}
module Recalc.Server.Generic
  ( Id
  , JsonRpcRequest (..)
  , pattern JsonRpcNotification
  , JsonRpc
  , HasHandler
  , HandlerT
  , handle
  , hoist
  , GenericMode (..)
  , AsServerT
  , AsServer
  , AsApi
  , ToApi
  , namedHandlers
  , (:>)
  , (:<|>) (..)
  ) where

import Control.Exception (SomeException, displayException, try)
import Data.Aeson (Options (..))
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LB
import Data.Data (Typeable, typeRep)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Recalc.Server.Json (aesonOptions)

-- | inject jsonrpc version string into a @'Json.Value'@
genericToVersionedJSON
  :: (Generic a, Json.GToJSON' Json.Value Json.Zero (Rep a))
  => Options
  -> a
  -> Json.Value
genericToVersionedJSON options = inject "jsonrpc" (Json.String "2.0") . Json.genericToJSON options
 where
  inject k v = \case
    Json.Object obj -> Json.Object (KeyMap.insert (Key.fromString k) v obj)
    err -> error ("not an object: " <> show err)

showJson :: Json.ToJSON a => a -> String
showJson = Text.unpack . Text.decodeUtf8 . LB.toStrict . Json.encode

type Id = Word64

data JsonRpcRequest params = JsonRpcRequest
  {request'method :: String, request'id :: Maybe Id, request'params :: params}
  deriving (Generic, Show)

pattern JsonRpcNotification :: String -> params -> JsonRpcRequest params
pattern JsonRpcNotification method params = JsonRpcRequest method Nothing params

instance Json.FromJSON params => Json.FromJSON (JsonRpcRequest params) where
  parseJSON = Json.genericParseJSON aesonOptions{Json.rejectUnknownFields = False}

instance Json.ToJSON params => Json.ToJSON (JsonRpcRequest params) where
  toJSON = genericToVersionedJSON aesonOptions

data JsonRpcResult rsp = JsonRpcResult
  {response'id :: Maybe Id, response'result :: rsp}
  deriving (Generic, Show)

instance Json.FromJSON rsp => Json.FromJSON (JsonRpcResult rsp)

instance Json.ToJSON rsp => Json.ToJSON (JsonRpcResult rsp) where
  toJSON =
    genericToVersionedJSON
      aesonOptions
        { omitNothingFields = False -- make sure to encode @JsonRpcResult ()@ as @{..result: []}@
        }

data JsonRpcErrorObject = JsonRpcErrorObject
  { errorObject'code :: Int
  -- ^ -32099 to -32000 are server-defined JSON-RPC codes
  , errorObject'message :: Text
  }
  deriving (Generic, Show)

instance Json.FromJSON JsonRpcErrorObject

instance Json.ToJSON JsonRpcErrorObject where
  toJSON = Json.genericToJSON aesonOptions

data JsonRpcError = JsonRpcError
  {errorResponse'id :: Maybe Id, errorResponse'error :: JsonRpcErrorObject}
  deriving (Generic, Show)

instance Json.FromJSON JsonRpcError

instance Json.ToJSON JsonRpcError where
  toJSON = genericToVersionedJSON aesonOptions

jsonRpcError :: Maybe Id -> String -> JsonRpcError
jsonRpcError xId = JsonRpcError xId . JsonRpcErrorObject (-32001) . Text.pack

type Segment = String
class HasHandler api where
  type HandlerT api (m :: Type -> Type)
  handle'
    :: (JsonRpcRequest Json.Value, [Segment]) -> Handler api -> Either String (IO Json.Value)
  hoist :: (forall x. m x -> n x) -> HandlerT api m -> HandlerT api n

handle
  :: forall api
   . HasHandler api
  => JsonRpcRequest Json.Value
  -> Handler api
  -> Either String (IO Json.Value)
handle = handle' @api . (,[])

type Handler api = HandlerT api IO

infixr 4 :>
infixr 3 :<|>
infixl 0 :-

data (path :: Symbol) :> (a :: Type) :: Type
data a :<|> b = a :<|> b

data JsonRpc (sym :: Symbol) (params :: Type) (rsp :: Type)

instance (KnownSymbol path, HasHandler api) => HasHandler (path :> api) where
  type HandlerT (path :> api) m = HandlerT api m
  handle' (req, segments) = handle' @api (req, symbolVal @path Proxy : segments)
  hoist = hoist @api

instance (HasHandler a, HasHandler b) => HasHandler (a :<|> b) where
  type HandlerT (a :<|> b) m = HandlerT a m :<|> HandlerT b m
  handle' req (a :<|> b) = handle' @a req a <> handle' @b req b
  hoist nt (a :<|> b) = hoist @a nt a :<|> hoist @b nt b

instance
  ( KnownSymbol sym
  , Json.FromJSON params
  , Json.ToJSON rsp
  , Typeable params
  )
  => HasHandler (JsonRpc sym params rsp)
  where
  type HandlerT (JsonRpc sym params rsp) m = (Maybe Id, params) -> m rsp
  handle' (JsonRpcRequest{..}, segments) f
    | methodMatches
    , Json.Success params' <- Json.fromJSON @params request'params =
        Right
          $ try @SomeException (f (request'id, params'))
          <&> \case
            Left err -> Json.toJSON (jsonRpcError request'id (displayException err))
            Right res -> Json.toJSON (JsonRpcResult request'id res)
    | methodMatches =
        Left
          $ "handler for method '"
            <> request'method
            <> "' expects '"
            <> show (typeRep $ Proxy @params)
            <> "' (got: '"
            <> showJson request'params
            <> "')"
    | otherwise = Left $ "no handler for method '" <> request'method <> "' " <> show request'params
   where
    path = reverse (symbolVal @sym Proxy : segments)
    methodMatches = intercalate "/" path == request'method

  hoist nt f = nt . f

-- | generic modes for protocol datatypes
class GenericMode mode where
  type mode :- api :: Type

data AsServerT (m :: Type -> Type)

-- \^ server mode

instance GenericMode (AsServerT m) where
  type AsServerT m :- api = HandlerT api m

type AsServer =
  AsServerT IO
  -- ^ the default server

data AsApi

-- \^ api mode

instance GenericMode AsApi where
  type AsApi :- api = api

class GSum f where
  type GToSum f
  gtoHandler :: f p -> GToSum f

instance (GSum l, GSum r) => GSum (l :*: r) where
  type GToSum (l :*: r) = GToSum l :<|> GToSum r
  gtoHandler (l :*: r) = gtoHandler l :<|> gtoHandler r

instance GSum f => GSum (M1 i c f) where
  type GToSum (M1 i c f) = GToSum f
  gtoHandler = gtoHandler . unM1

instance GSum (K1 i c) where
  type GToSum (K1 i c) = c
  gtoHandler = unK1

-- the sum representation of a generic protocol datatype
type ToApi api' = GToSum (Rep (api' AsApi))

-- | generically converts an @Api ('AsServerT' m)@ to its sum representation @ep0 :<|> ep1 :<|> ...@
namedHandlers
  :: (GenericMode m, Generic (api' m), GSum (Rep (api' m)))
  => api' m
  -> GToSum (Rep (api' m))
namedHandlers = gtoHandler . from

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Recalc.Server.TypeScript (typeScriptInterface) where

import Data.Aeson.TypeScript.Internal
import Data.Aeson.TypeScript.TH
import Data.List (intercalate)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

import Recalc.Server

-- | create @type MethodType = IMethodType@ with @interface IMethodType<T1, T2> { params: T1; result: T2; }@
data MethodType p r = MethodType {methodType'params :: p, methodType'result :: r}
  deriving (Generic)

$(deriveTypeScript aesonOptions ''MethodType)

-- | get the TypeScript declarations for a protocol datatype, for example using:
--
-- @
-- data Ping m = Ping
--   { hello  :: m :- JsonRpc "ping" String ()
--   , isEven :: m :- "is" :> JsonRpc "even" Int Bool
--   } deriving Generic
-- @
--
-- You can create the TypeScript interfaces using @typeScriptInterface \@Ping@ which
-- would lead to:
--
-- @
-- type MethodType<T1, T2> = IMethodType<T1, T2>;
--
-- interface IMethodType<T1, T2> {
--   params: T1;
--   result: T2;
-- }
--
-- interface Ping {
--   "hello": MethodType<string, void>;
--   "is/even": MethodType<number, boolean>;
-- }
-- @
typeScriptInterface
  :: forall api' m c f
   . (Rep (api' m) ~ M1 D c f, Datatype c, TypeScriptApi (ToApi api'))
  => [TSDeclaration]
typeScriptInterface = methodTypeDecl <> decls <> [TSInterfaceDeclaration name [] fields Nothing]
 where
  methodTypeDecl = getTypeScriptDeclarations @(MethodType () ()) Proxy
  (fields, decls') = toTypeScriptApi @(ToApi api') []
  name = datatypeName @c undefined
  decls = decls' -- FIXME: dedup

class TypeScriptApi t where
  toTypeScriptApi :: [String] -> ([TSField], [TSDeclaration])

instance (KnownSymbol method, TypeScriptApi api) => TypeScriptApi (method :> api) where
  toTypeScriptApi = toTypeScriptApi @api . (symbolVal @method Proxy :)

instance (TypeScriptApi a, TypeScriptApi b) => TypeScriptApi (a :<|> b) where
  toTypeScriptApi segments = toTypeScriptApi @a segments <> toTypeScriptApi @b segments

instance
  (KnownSymbol method, TypeScript params, TypeScript rsp)
  => TypeScriptApi (JsonRpc method params rsp)
  where
  toTypeScriptApi segments = ([field], decls)
   where
    fieldName = intercalate "/" . reverse $ symbolVal @method Proxy : segments
    field = TSField False (show fieldName) (getTypeScriptType @(MethodType params rsp) Proxy) Nothing
    decls = getTypeScriptDeclarations @params Proxy ++ getTypeScriptDeclarations @rsp Proxy

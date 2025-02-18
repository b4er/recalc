module Recalc.Server.TH (deriveTypeScriptProtocol, deriveTypeScriptProtocolWithout) where

import Data.Aeson.TypeScript.TH
import Language.Haskell.TH

import Recalc.Server (aesonOptions)

-- | extract all ''*Param and (some) ''*Return, then @deriveTypeScript@ on them
deriveTypeScriptProtocol :: Name -> Q [Dec]
deriveTypeScriptProtocol = (`deriveTypeScriptProtocolWithout` [])

-- | extract some ''*Param and ''*Return, then @deriveTypeScript@ on them
deriveTypeScriptProtocolWithout :: Name -> [Name] -> Q [Dec]
deriveTypeScriptProtocolWithout protocolName excludeNames =
  reify protocolName >>= \case
    TyConI (DataD _ _ _ _ [RecC _ tys] _) -> do
      let
        isDataOrNewtype = \case
          TyConI DataD{} -> True
          TyConI NewtypeD{} -> True
          _ -> False

        -- this is a bit annoying since we wouldn't want to rederive eg. Json.Value
        names =
          concat
            [ name
              : ( case result of
                    ConT r
                      | nameModule r == nameModule protocolName
                      , r `notElem` excludeNames ->
                          [r]
                    _ -> []
                )
            | (_, _, AppT _ (AppT (AppT (AppT (ConT _) _) (ConT name)) result)) <- tys
            ]

      -- derive TypeScript for all newtypes and data defs
      foldMap (deriveTypeScript aesonOptions . fst)
        . filter (isDataOrNewtype . snd)
        . zip names
        =<< mapM reify names
    _ -> error "invalid Protocol type."

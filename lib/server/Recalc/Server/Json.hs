{-|
Module      : Recalc.Server.Json where
Description : Shared JSON options for (de-)serialization.
-}
module Recalc.Server.Json where

import Data.Aeson as Json
import Data.Char (toLower)

-- | JSON Instances
aesonOptions :: Json.Options
aesonOptions =
  Json.defaultOptions
    { Json.fieldLabelModifier = labelModifier -- commandUri -> uri
    , Json.constructorTagModifier = consMap toLower id -- Command -> command
    , Json.omitNothingFields = True
    , Json.allowOmittedFields = True
    , Json.rejectUnknownFields = True
    }
 where
  labelModifier str
    | '\'' `elem` str = tail $ dropWhile (/= '\'') str
    | otherwise = str

  consMap f g (x : xs) = f x : g xs
  consMap _ g [] = g []

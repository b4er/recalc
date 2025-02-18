{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recalc.Server.Types where

import Data.Aeson qualified as Json

import Recalc.Engine (Isn't (..), Meta (..))

instance Isn't Json.Value where
  isn't = (Json.Null ==)

data Nullable t
  = Missing
  | Null
  | Is t
  deriving (Eq, Ord, Show)

instance Isn't a => Isn't (Nullable a) where
  isn't (Is x) = isn't x
  isn't _ = True

instance Json.FromJSON t => Json.FromJSON (Nullable t) where
  parseJSON Json.Null = pure Null
  parseJSON v = Is <$> Json.parseJSON v

  omittedField = Just Missing

instance Json.ToJSON t => Json.ToJSON (Nullable t) where
  toJSON = \case
    Null -> Json.Null
    Missing -> Json.Null
    Is t -> Json.toJSON t

  omitField = \case Missing -> True; _ -> False

instance Meta (Nullable t) where
  Missing `merge` x = x
  x `merge` Missing = x
  Null `merge` x = x
  _ `merge` x = x

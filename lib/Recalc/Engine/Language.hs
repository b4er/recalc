{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Recalc.Engine.Language
Description : Spreadsheet language interface.

This module specifies the Language interface used by the recalculation engine.
-}
module Recalc.Engine.Language
  ( module Recalc.Engine.Language
  , Text
  , URI
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import Network.URI (URI)
import Prettyprinter
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import Recalc.Engine.Core (CellAddr, CellRange, SheetId)

data Kind = Type | Value deriving (Eq, Ord, Show)

-- | The spreadsheet engine can be queried for re-evaluation of
-- cells (types and values) and volatile results.
data Ix
  = Cell Kind SheetId CellAddr
  | Volatile
  deriving (Eq, Ord, Show)

-- | Evaluation of cells can always fail due to invalid formulas or refs
data FetchError err
  = InvalidFormula (ParseErrorBundle String Void)
  | RefError
  | OtherError err
  deriving (Eq, Show)

instance Pretty err => Pretty (FetchError err) where
  pretty = \case
    InvalidFormula parseError -> pretty (errorBundlePretty parseError)
    RefError -> "#REF"
    OtherError err -> pretty err

errorTitle :: (a -> Text) -> FetchError a -> Text
errorTitle otherTitle = \case
  InvalidFormula{} -> "Syntax Error"
  RefError -> "Invalid Reference"
  OtherError err -> otherTitle err

type FetchResult err f = ExceptT (FetchError err) f

-- | Fetch callbacks can fail and have access to other cells, as well as
-- a user-defined environment (typically used to deal with named sheets)
type Fetch env err value a =
  forall f
   . Monad f
  => ReaderT (Ix -> FetchResult err f value, env) (FetchResult err f) a

-- | The spreadsheet language abstraction
class Language term where
  -- | Compute dependencies of a term
  deps :: term -> Set CellRange

  type EnvOf term

  -- \^ terms must have associated 'Fetch' environment
  newEnv :: SheetId -> EnvOf term

  type ErrorOf term

  -- \^ terms must have associated 'Fetch' errors
  type ValueOf term

  -- \^ terms must have associated values

  -- | All terms can be evaluated in the 'Fetch' context
  -- (a dependently typed language is assumed: values and types coincide)
  infer :: term -> Fetch (EnvOf term) (ErrorOf term) (ValueOf term) (ValueOf term)

  -- | All values can be inferred
  inferValue :: EnvOf term -> ValueOf term -> Maybe (ValueOf term)

  -- | All terms can be evaluated in the 'Fetch' context
  eval :: term -> Fetch (EnvOf term) (ErrorOf term) (ValueOf term) (ValueOf term)

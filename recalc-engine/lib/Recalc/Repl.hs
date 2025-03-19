{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.Repl
Description : Utility functions for experimenting in GHCi

Provides a few utilities to experiment with the core language
to examine outcomes of parsing, type checking and inference,
evaluation and sheet recomputation.

Only the recalculation family of operations implements cell
references (only single sheeted - assumes that the current
'URI' is @\"file://repl.rc\"@, and the current 'SheetName' is
@\"Test\"@).

For an example usage of this module, refer to
[Recalc.EngineSpec](./recalc-engine-spec/Recalc-EngineSpec.html).
-}
module Recalc.Repl
  ( Result

    -- ** Parsing
  , parseFormula
  , parseValue

    -- ** Type Inference & Evaluation
  , infer
  , eval

    -- ** Spreadshheet Recalculation
  , newEngineState
  , recalc
  , evalRecalc
  , execRecalc
  --  -- * re-export
  , pretty
  ) where

import Control.Arrow (Arrow (first))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (bimap)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI)
import Prettyprinter (pretty)

import Recalc.Engine hiding (eval, infer, newEngineState, recalc)

import Recalc.Engine qualified as Recalc

uri :: URI
uri = fromJust (parseURI "file://repl.rc")

testId :: SheetId
testId = (uri, "Test")

type Result e = Either (FetchError e)

{- parsing -}

parseCell' :: Recalc t => CellType -> String -> Result (ErrorOf t) t
parseCell' k = mapLeft InvalidFormula . parseTerm (testId, (0, 0)) . (,k)
 where
  mapLeft f = \case
    Left x -> Left (f x)
    Right y -> Right y

-- | parse a formula using the specified parser
parseFormula :: Recalc t => String -> Result (ErrorOf t) t
parseFormula = parseCell' CellFormula

-- | parse a value using the specified parser
parseValue :: Recalc t => String -> Result (ErrorOf t) t
parseValue = parseCell' CellValue

{- evaluation and type checking -}

-- | infer the type of a term under a certain context (cell references not
-- supported, use 'recalc' instead)
infer
  :: forall t
   . Recalc t
  => EnvOf t
  -- ^ custom context as specified
  -> t
  -- ^ term to infer type from
  -> Result (ErrorOf t) (TypeOf t)
  -- ^ either a type error or the inferred type
infer env t = runFetch @t env (Recalc.infer @t t)

-- | evaluate a term (cell references not supported, use 'recalc' instead)
eval
  :: forall t
   . Recalc t
  => EnvOf t
  -- ^ custom context as specified
  -> ElaborationOf t
  -- ^ term to evaluate
  -> Result (ErrorOf t) (ValueOf t)
eval env t = runFetch @t env (Recalc.eval @t t)

{- spreadsheet operations -}

-- | recalculate a spreadsheet from new inputs and a engine state (see
-- 'newEngineState' to create a new one)
recalc
  :: (Recalc t, Show (ErrorOf t))
  => [(CellAddr, Maybe String)]
  -- ^ a list of inputs ('Nothing' for removing a cell)
  -> EngineStateOf t
  -- ^ the current engine state
  -> (ResultsOf t, EngineStateOf t)
  -- ^ all changed cells and their values, and the new engine state
recalc inputs =
  first (either (error . ("cycle: " <>) . show) id)
    . Recalc.recalc (mkInputs inputs)

-- | same as 'recalc' but only returns the results
evalRecalc
  :: (Recalc t, Show (ErrorOf t))
  => [(CellAddr, Maybe String)]
  -> EngineStateOf t
  -> ResultsOf t
evalRecalc inputs = fst . recalc inputs

-- | same as 'recalc' but only returns the new engine state
execRecalc
  :: (Recalc t, Show (ErrorOf t)) => [(CellAddr, Maybe String)] -> EngineStateOf t -> EngineStateOf t
execRecalc inputs = snd . recalc inputs

{- internal -}

runFetch :: EnvOf t -> FetchOf t a -> Result (ErrorOf t) a
runFetch env = runFetchWith env (\_ -> throwError RefError)

-- | create a new engine state, initialising the custom context
newEngineState :: EnvOf t -> EngineStateOf t
newEngineState =
  mapDocs (Map.insert uri (Document [sheetName] (Map.singleton sheetName mempty)))
    . Recalc.newEngineState
 where
  sheetName = snd testId

mkInputs :: [(CellAddr, Maybe String)] -> Inputs
mkInputs = map (bimap (testId,) alg)
 where
  alg =
    (,Meta) . maybe Nothing \case
      s@('=' : _) -> Just (s, CellFormula)
      s -> Just (s, CellValue)

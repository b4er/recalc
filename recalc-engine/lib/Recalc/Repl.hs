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
-}
module Recalc.Repl
  ( Result
  , parseFormula
  , parseValue
  , infer
  , eval
  , recalc
  , evalRecalc
  , execRecalc
  --  -- * re-export
  , pretty
  , newEngineState
  ) where

import Control.Arrow (Arrow (first))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (bimap)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI)
import Prettyprinter (pretty)

import Recalc.Engine hiding (eval, infer, newEngineState, recalc, runFetch)

-- (CellAddr, CellType(..), Document(..), EngineStateOf, Fetch, FetchError(..), Inputs, Meta(..), Results, SheetId, mapDocs, newEngineState, parseTerm, runFetchWith)
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

parseFormula :: Recalc t => String -> Result (ErrorOf t) t
parseFormula = parseCell' CellFormula

parseValue :: Recalc t => String -> Result (ErrorOf t) t
parseValue = parseCell' CellValue

{- evaluation and type checking -}

infer :: forall t. Recalc t => EnvOf t -> t -> Result (ErrorOf t) (TypeOf t) -- Type!
infer env t = runFetch @t env (Recalc.infer @t t)

eval :: forall t. Recalc t => EnvOf t -> t -> Result (ErrorOf t) (ValueOf t)
eval env t = runFetch @t env (Recalc.eval @t t)

{- spreadsheet operations -}

recalc
  :: (Recalc t, Show (ErrorOf t))
  => [(CellAddr, Maybe String)]
  -> EngineStateOf t
  -> (Results (ErrorOf t) t (ValueOf t), EngineStateOf t)
recalc inputs =
  first (either (error . ("cycle: " <>) . show) id)
    . Recalc.recalc (mkInputs inputs)

evalRecalc
  :: (Recalc t, Show (ErrorOf t))
  => [(CellAddr, Maybe String)]
  -> EngineStateOf t
  -> Results (ErrorOf t) t (ValueOf t)
evalRecalc inputs = fst . recalc inputs

execRecalc
  :: (Recalc t, Show (ErrorOf t)) => [(CellAddr, Maybe String)] -> EngineStateOf t -> EngineStateOf t
execRecalc inputs = snd . recalc inputs

{- internal -}

type TypeOf t = ValueOf t

runFetch :: EnvOf t -> Fetch (EnvOf t) (ErrorOf t) (ValueOf t) a -> Result (ErrorOf t) a
runFetch env = runFetchWith env (\_ _ -> throwError RefError)

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

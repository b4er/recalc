{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Eta reduce" -}

{-|
Module      : Recalc.Engine.Monad
Description : Recalculation monad for spreadsheets using terms
              satisfying the 'Language' interface.

Mainly abstractions around "Build systems à la carte" abstractions,
see "Recalc.Engine" for its usage.
-}
module Recalc.Engine.Monad where

import Build.Scheduler
import Build.Task
import Control.Arrow (second)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (asks, local, runReaderT)
import Control.Monad.State.Strict

import Recalc.Engine.Core (CellAddr, SheetId)
import Recalc.Engine.DependencyMap qualified as Deps
import Recalc.Engine.DocumentStore
import Recalc.Engine.Language

fetchType :: SheetId -> CellAddr -> Fetch env err value value
fetchType sheetId ca = lift . ($ Cell Type sheetId ca) =<< asks fst

fetchValue :: SheetId -> CellAddr -> Fetch env err value value
fetchValue sheetId ca = lift . ($ Cell Value sheetId ca) =<< asks fst

getEnv :: Fetch env err value env
getEnv = asks snd

type FetchErrorOf term = FetchError (ErrorOf term)

localEnv :: (env -> env) -> Fetch env err value a -> Fetch env err value a
localEnv f x = local (second f) x

-- | A spreadsheet value is a monadic task that computes a type, value
-- or result of a volatile function
type Spreadsheet value = Task Monad Ix value

runFetch :: env -> Fetch env err value value -> Spreadsheet (Either (FetchError err) value)
runFetch env x f = runExceptT $ runReaderT x (ExceptT . f, env)

type Spreadsheets value = Tasks Monad Ix value

-- | compute the 'Tasks' for a 'DocumentStore'
spreadsheetsOf
  :: Language term
  => EnvOf term
  -> DocumentStore doc sheet cell term (ValueOf term) (FetchErrorOf term)
  -> Ix
  -> Maybe (Spreadsheet (Either (FetchErrorOf term) (ValueOf term)))
spreadsheetsOf env ds = \case
  Cell k sheetId ca
    | k == Type -> runFetch env . infer <$> lookupCellTerm sheetId ca ds
    | k == Value -> runFetch env . eval <$> lookupCellTerm sheetId ca ds
  _ -> Nothing -- FIXME: implement volatile functions

type DS doc sheet cell term =
  DocumentStore doc sheet cell term (ValueOf term) (FetchErrorOf term)

data EngineState dm doc sheet cell term
  = EngineState !(Chain Ix) !(DS doc sheet cell term) !(dm CellAddr)

mapDS
  :: (DS doc sheet cell term -> DS doc sheet cell term)
  -> EngineState dm doc sheet cell term
  -> EngineState dm doc sheet cell term
mapDS f (EngineState chain ds dm) = EngineState chain (f ds) dm

instance
  (Show doc, Show sheet, Show cell, Show term, Show (ValueOf term), Show (ErrorOf term))
  => Show (EngineState dm doc sheet cell term)
  where
  show (EngineState _ ds _) = show ds

newEngineState :: Deps.DependencyMap dm => EngineState dm doc sheet cell term
newEngineState = EngineState [] newDocumentStore Deps.empty

type EngineT dm doc sheet cell term f =
  StateT (EngineState dm doc sheet cell term) f

runEngineT
  :: EngineT dm doc sheet cell term f a
  -> EngineState dm doc sheet cell term
  -> f (a, EngineState dm doc sheet cell term)
runEngineT = runStateT

modifyDocs
  :: Monad f
  => (DS doc sheet cell term -> DS doc sheet cell term)
  -> EngineT dm doc sheet cell term f ()
modifyDocs = modify . mapDS

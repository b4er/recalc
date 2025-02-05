{-# LANGUAGE ImpredicativeTypes #-}
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
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State.Strict

import Recalc.Engine.DependencyMap qualified as Deps
import Recalc.Engine.DocumentStore
import Recalc.Engine.Language

fetchType :: URI -> Text -> CellAddr -> Fetch env err value value
fetchType uri sheetId ca = lift . ($ Cell Type (uri, sheetId) ca) =<< asks fst

fetchValue :: URI -> Text -> CellAddr -> Fetch env err value value
fetchValue uri sheetId ca = lift . ($ Cell Value (uri, sheetId) ca) =<< asks fst

getEnv :: Fetch env err value env
getEnv = asks snd


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
  -> DocumentStore doc sheet cell term value
  -> Ix
  -> Maybe (Spreadsheet (Either (FetchError (ErrorOf term)) (ValueOf term)))
spreadsheetsOf env ds = \case
  Cell k sheetId ca
    | k == Type -> runFetch env . infer <$> lookupCellTerm sheetId ca ds
    | k == Value -> runFetch env . eval <$> lookupCellTerm sheetId ca ds
  _ -> Nothing -- FIXME: implement volatile functions

type DS doc sheet cell term = DocumentStore doc sheet cell term (ValueOf term)

data EngineState dm doc sheet cell term
  = EngineState !(Chain Ix) !(DS doc sheet cell term) !(dm CellAddr)

newEngineState :: Deps.DependencyMap dm => EngineState dm doc sheet cell term
newEngineState = EngineState [] newDocumentStore Deps.empty

type EngineT dm doc sheet cell term f =
  StateT (EngineState dm doc sheet cell term) f

runEngineT
  :: EngineT dm doc sheet cell term f a
  -> EngineState dm doc sheet cell term
  -> f (a, EngineState dm doc sheet cell term)
runEngineT = runStateT

docs
  :: Monad f
  => (DS doc sheet cell term -> (a, DS doc sheet cell term))
  -> EngineT dm doc sheet cell term f a
docs f = state $ \(EngineState chain documentStore depMap) ->
  let (x, documentStore') = f documentStore
   in (x, EngineState chain documentStore' depMap)

modifyDocs
  :: Monad f
  => (DS doc sheet cell term -> DS doc sheet cell term)
  -> EngineT dm doc sheet cell term f ()
modifyDocs f = docs (((),) . f)

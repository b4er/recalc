{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Recalc.Engine (module Recalc.Engine, module Recalc.Engine.Core) where

import Build.Rebuilder (dirtyBitRebuilder)
import Build.Store (Store, getInfo, getValue, initialise)
import Build.Task (Task, Tasks)

import Build.Scheduler (Chain, restarting)
import Control.Monad ((<=<))
import Control.Monad.Except
  ( Except
  , ExceptT (..)
  , MonadError (..)
  , runExcept
  , runExceptT
  )
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks, lift)
import Data.Bifunctor (bimap, first, second)
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Network.URI (URI (uriPath))
import Prettyprinter hiding (column)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, Parsec, parse)

import Recalc.Engine.Core
import Recalc.Engine.DependencyMap (Slow)
import Recalc.Engine.DependencyMap qualified as Deps

-- import Recalc.Server.Protocol (Annotation)

{- Interface -}
class (Pretty t, Pretty (ValueOf t)) => Recalc t where
  type EnvOf t
  type ErrorOf t
  type ValueOf t

  parseCell :: CellType -> ReaderT SheetId (Parsec Void String) t

  depsOf :: t -> Set CellRangeRef

  infer :: t -> Fetch (EnvOf t) (ErrorOf t) (ValueOf t) (ValueOf t)
  eval :: t -> Fetch (EnvOf t) (ErrorOf t) (ValueOf t) (ValueOf t)

{- Engine -}

data Kind = Type | Value deriving (Eq, Ord, Show)

-- | The spreadsheet engine can be queried for re-evaluation of
-- cells (types and values) and volatile results.
data Ix = CellIx Kind CellRef | VolatileIx
  deriving (Eq, Ord, Show)

type ParseError = ParseErrorBundle String Void

-- | Evaluation of cells can always fail due to invalid formulas or refs
data FetchError err
  = InvalidFormula ParseError
  | RefError
  | SemanticError err
  deriving (Eq, Show)

throwSemanticError :: err -> Fetch env err v a
throwSemanticError = throwError . SemanticError

-- | Fetch callbacks can fail and have access to other cells + environment
newtype Fetch env err v a
  = Fetch
      ( forall f
         . Monad f
        => ReaderT (Ix -> ExceptT (FetchError err) f v, env) (ExceptT (FetchError err) f) a
      )
  deriving (Functor)

fetch :: Ix -> Fetch env err v v
fetch ix = Fetch $ lift . ($ ix) =<< asks fst

fetchValue, fetchType :: CellRef -> Fetch env err v v
fetchValue = fetch . CellIx Value
fetchType = fetch . CellIx Type

runFetchWith
  :: env
  -> (Kind -> CellRef -> Except (FetchError err) v)
  -> Fetch env err v a
  -> Either (FetchError err) a
runFetchWith env f (Fetch v) = runExcept . runReaderT v . (,env) $ \case
  CellIx k ref -> f k ref
  _ -> throwError RefError

-- Applicative instance (cannot be derived due to the higher-rank type)
instance Applicative (Fetch env err v) where
  Fetch f <*> Fetch x = Fetch (f <*> x)
  pure a = Fetch (pure a)

-- Applicative instance (cannot be derived due to the higher-rank type)
instance Monad (Fetch env err v) where
  Fetch x >>= f = Fetch (x >>= \y' -> let Fetch y = f y' in y)

instance MonadError (FetchError err) (Fetch env err v) where
  throwError err = Fetch (throwError err)
  catchError (Fetch x) handler =
    Fetch
      $ x `catchError` (\e' -> let Fetch e = handler e' in e)

instance MonadReader env (Fetch env err v) where
  ask = Fetch (asks snd)
  local f (Fetch x) = Fetch (local (second f) x)

type DocumentStore err t v = Map URI (Document err t v)

data Document err t v = Document
  { sheetOrder :: ![Text]
  , sheets :: !(Map Text (Sheet err t v))
  }

instance (Show err, Pretty t, Pretty v) => Show (Document err t v) where
  show Document{..} =
    "Document {"
      <> "sheetOrder = "
      <> show sheetOrder
      <> ", sheets = "
      <> show (showKeys sheets)
      <> "}"
   where
    -- make sure keys show as [Col][Row] excel-style
    showKeys = Map.map (Map.mapKeys showExcel26)

type Sheet err t v = Map CellAddr (Cell err t v)

type Parsed = Either ParseError

data Meta = Meta deriving (Show)

data CellType = CellFormula | CellValue deriving (Show)

data Cell err t v = Cell
  { cell :: !(Maybe ((String, CellType), Maybe (t, Maybe (v, Maybe v))))
  -- ^ a cell maybe only meta data. when it has a term, it can have a type etc.
  , cellDeps :: !(Set CellRangeRef)
  , cellError :: !(Maybe (FetchError err))
  , cellMeta :: !Meta
  }

instance (Show err, Pretty t, Pretty v) => Show (Cell err t v) where
  show Cell{..} =
    "Cell { cell = "
      <> maybe "#ERR" showCell cell
      <> ", cellDeps = "
      <> show cellDeps
      <> maybe "" (\e -> ", cellError = " <> show e) cellError
      <> "}"
   where
    showCell ((s, _ct), xtv) =
      s
        <> maybe
          ""
          ( \(x, tv) ->
              " ("
                <> prettyString x
                <> maybe
                  ""
                  ( \(t, v') ->
                      ": "
                        <> prettyString t
                        <> maybe "" (\v -> " [↝ " <> prettyString v <> "]") v'
                  )
                  tv
                <> ")"
          )
          xtv

renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

prettyString :: Pretty a => a -> String
prettyString = Text.unpack . renderPretty

cellOf :: Parsed t -> Maybe (t, Maybe (ValueOf t, Maybe (ValueOf t)))
cellOf = \case
  Right t -> Just (t, Nothing)
  _ -> Nothing

errorOf :: Parsed a -> Maybe (FetchError e)
errorOf = \case
  Left err -> Just (InvalidFormula err)
  _ -> Nothing

-- set an error
setError :: FetchError err -> Cell err t v -> Cell err t v
setError err c = c{cellError = Just err}

-- set type (setting value to @Nothing@) when a term is stored
setType :: v -> Cell err t v -> Cell err t v
setType typ c =
  c
    { cell = second (fmap (second (const (Just (typ, Nothing))))) <$> cell c
    , cellError = Nothing
    }

-- set value when a term and type are stored
setValue :: v -> Cell err t v -> Cell err t v
setValue val c =
  c
    { cell = second (fmap (second ((,Just val) . fst <$>))) <$> cell c
    , cellError = Nothing
    }

alterCell
  :: CellRef
  -> (Maybe (Cell err t v) -> Maybe (Cell err t v))
  -> DocumentStore err t v
  -> DocumentStore err t v
alterCell ((uri, sheetName), ca) f = (`Map.update` uri) $ \doc ->
  Just
    doc
      { sheets = ($ sheets doc) . (`Map.update` sheetName) $ \sheet ->
          Just (Map.alter f ca sheet)
      }

lookupCell :: CellRef -> DocumentStore err t v -> Maybe (Cell err t v)
lookupCell ((uri, sheetName), ca) =
  Map.lookup ca <=< Map.lookup sheetName . sheets <=< Map.lookup uri

lookupCellTerm :: CellRef -> DocumentStore err t v -> Maybe (t, CellType)
lookupCellTerm ref ds = do
  (ct, x) <- fmap (first snd) . cell =<< lookupCell ref ds
  (,ct) . fst <$> x

lookupCellType :: CellRef -> DocumentStore err t v -> Maybe v
lookupCellType ref =
  fmap fst . snd <=< snd <=< cell <=< lookupCell ref

lookupCellValue :: CellRef -> DocumentStore err t v -> Maybe v
lookupCellValue ref ds = case lookupCellError ref ds of
  Just _ -> Nothing
  _ -> snd =<< snd =<< snd =<< cell =<< lookupCell ref ds

lookupCellError :: CellRef -> DocumentStore err t v -> Maybe (FetchError err)
lookupCellError ref ds = cellError =<< lookupCell ref ds

data EngineState env err t v = EngineState
  { engineEnv :: !env
  , engineChain :: !(Chain Ix)
  , engineDocs :: !(DocumentStore err t v)
  , engineDeps :: !(Slow CellRef)
  }

type EngineStateOf t = EngineState (EnvOf t) (ErrorOf t) t (ValueOf t)

instance (Show err, Pretty t, Pretty v) => Show (EngineState env err t v) where
  show EngineState{engineDocs = docs} = "EngineState {" <> show (showKeys docs) <> "}"
   where
    -- make sure URIs show with quotes
    showKeys = Map.mapKeys show

newEngineState :: env -> EngineState env err t v
newEngineState env = EngineState env [] mempty Deps.empty

mapEnv :: (env -> env) -> EngineState env err t v -> EngineState env err t v
mapEnv f st = st{engineEnv = f (engineEnv st)}

mapDocs
  :: (DocumentStore err t v -> DocumentStore err t v)
  -> EngineState env err t v
  -> EngineState env err t v
mapDocs f st = st{engineDocs = f (engineDocs st)}

type Inputs = [(CellRef, (Maybe (String, CellType), Meta))]

type Cycle err t v = [(CellRef, Cell err t v)]
type Results err t v = [(CellRef, Cell err t v)]

recalc
  :: forall t
   . Recalc t
  => Inputs
  -> EngineState (EnvOf t) (ErrorOf t) t (ValueOf t)
  -> ( Either (Cycle (ErrorOf t) t (ValueOf t)) (Results (ErrorOf t) t (ValueOf t))
     , EngineState (EnvOf t) (ErrorOf t) t (ValueOf t)
     )
recalc inputs (EngineState env chain docs deps) =
  let
    validatedInputs :: [(CellRef, Maybe ((String, CellType), Parsed t), Meta)]
    validatedInputs =
      [ (ref, (\x -> (x, parseTerm ref x)) <$> input, meta)
      | (ref, (input, meta)) <- inputs
      ]

    {- compute updates -}

    -- for each parsed input at a cell-ref:
    --   * update the dependency map with the newly computed dependencies
    --   * update the document store with the new data (term, deps & error)
    --
    -- allUpdates :: [(CellRef, (Endo DepMap, Endo DocumentStore))]
    allUpdates =
      [ (ref, (updateDepsEndo, updateCellEndo))
      | (ref, validInput, meta) <- validatedInputs
      , let
          -- validInput :: Maybe ((String, CellType), Parsed t)

          oldDeps = maybe mempty cellDeps (lookupCell ref docs)
          newDeps = maybe mempty (foldMap depsOf . snd) validInput

          updateDepsEndo = updateDeps (ref, oldDeps, newDeps)

          -- preliminary entry for the document store
          cell =
            Cell
              (second cellOf <$> validInput) -- type and value are not known yet
              newDeps
              (errorOf . snd =<< validInput) --
              meta

          -- entry is deleted when there is neither a term nor meta data
          updateCellEndo =
            Endo
              $ if isNothing validInput && isn't meta
                then alterCell ref (const Nothing)
                else alterCell ref (const (Just cell))
      ]

    -- compute new dependency map and document store
    (depsEndo, docsEndo) = foldMap snd allUpdates

    deps' = appEndo depsEndo deps
    docs' = appEndo docsEndo docs

    {- determine which cells need to be recalculated -}
    cycleOrDirty = dfs (map snd . Deps.query deps') [r | (r, _, _) <- validatedInputs]

    -- annotate a cycle [CellRef] with the cells to [(CellRef, Cell)]
    withCells refs = catMaybes [(ref,) <$> lookupCell ref docs' | ref <- refs]
  in
    -- either we're done with a cyclical error, or we do the actual recalculation yielding Results
    either (\cycl -> (Left (withCells cycl),)) (\dirty -> first Right . recalc' dirty) cycleOrDirty
      $ EngineState env chain docs' deps'

-- | a spreadsheet action is a monadic build task indexed by "Ix"
type Spreadsheet a = Task Monad Ix a

runFetch :: env -> Fetch env err v v -> Spreadsheet (Either (FetchError err) v)
runFetch env (Fetch x) f = runExceptT $ runReaderT x (ExceptT . f, env)

-- | a mapping from indices to potential spreadsheet actions defines our spreadsheets
type Spreadsheets a = Tasks Monad Ix a

spreadsheetsOf
  :: Recalc t
  => EnvOf t
  -> DocumentStore (ErrorOf t) t (ValueOf t)
  -> Spreadsheets (Either (FetchError (ErrorOf t)) (ValueOf t))
spreadsheetsOf env ds =
  (runFetch env <$>) . \case
    CellIx k ref -> alg k . fst <$> lookupCellTerm ref ds
    VolatileIx -> Just (throwError RefError)
 where
  alg = \case
    Type -> infer
    Value -> eval

type BuildStore err v = Store (Ix -> Bool, Chain Ix) Ix (Either (FetchError err) v)

getChain :: BuildStore err v -> Chain Ix
getChain = snd . getInfo

-- | get new values (by "Kind") in "BuildStore", partitioned into @(errors, new)@
getValues :: Kind -> [CellRef] -> BuildStore err v -> ([(CellRef, FetchError err)], [(CellRef, v)])
getValues kind refs store =
  partitionEithers
    [bimap (ref,) (ref,) (getValue (CellIx kind ref) store) | ref <- refs]

recalc'
  :: forall t
   . Recalc t
  => [CellRef]
  -> EngineState (EnvOf t) (ErrorOf t) t (ValueOf t)
  -> (Results (ErrorOf t) t (ValueOf t), EngineState (EnvOf t) (ErrorOf t) t (ValueOf t))
recalc' dirty (EngineState env chain docs deps) =
  let
    spreadsheets :: Spreadsheets (Either (FetchError (ErrorOf t)) (ValueOf t))
    spreadsheets = spreadsheetsOf env docs

    typecheck
      , run
        :: BuildStore (ErrorOf t) (ValueOf t) -> [CellRef] -> BuildStore (ErrorOf t) (ValueOf t)
    typecheck = foldr (restarting dirtyBitRebuilder spreadsheets . CellIx Type)
    run = foldr (restarting dirtyBitRebuilder spreadsheets . CellIx Value)

    store :: BuildStore (ErrorOf t) (ValueOf t)
    store = initialise (setDirty dirty, chain) $ \case
      CellIx kind ref
        | let
            lookupCell' = case kind of
              Type -> lookupCellType
              Value -> lookupCellValue
        , Just result <- lookupCell' ref docs ->
            pure result
        -- if the value cannot be found it may be invalid
        | Just err <- lookupCellError ref docs -> throwError err
        -- if there is no entry at all, it's a reference error
        | otherwise -> throwError RefError
      VolatileIx -> error "not implemented"

    {- recompute -}

    -- typecheck
    store' = store `typecheck` dirty

    (typeErrors, newTypes) = getValues Type dirty store'

    -- evaluate well-typed terms
    wellTyped = map fst newTypes
    store'' = store' `run` wellTyped

    (runtimeErrors, newValues) = getValues Value wellTyped store''

    {- calculate new EngineState (recalculation of dirty cells does not affect deps!) -}
    chain' = getChain store''

    -- set a result (error, type, or value)
    mapCell setter (ref, x) = Endo (alterCell ref (Just . setter x =<<))

    docs' =
      -- set types, then values, then errors (note order!)
      (`appEndo` docs)
        $ foldMap (mapCell setError) (typeErrors ++ runtimeErrors)
          <> foldMap (mapCell setValue) newValues
          <> foldMap (mapCell setType) newTypes

    engineState = EngineState env chain' docs' deps

    -- return all recalculated cells
    results = catMaybes [(ref,) <$> lookupCell ref docs' | ref <- dirty]
  in
    (results, engineState)
 where
  setDirty cs = \case
    CellIx _kind ref -> ref `elem` cs
    VolatileIx{} -> True

-- | Given a location + its old and new dependencies, update the dependency map accordingly
updateDeps :: Eq ref => (ref, Set CellRangeRef, Set CellRangeRef) -> Endo (Slow ref)
updateDeps (ref, oldDeps, newDeps) =
  foldMap (alg Deps.insert) (newDeps \\ oldDeps) <> foldMap (alg Deps.delete) (oldDeps \\ newDeps)
 where
  alg dm (sheetId, range) = Endo (dm sheetId range ref)

-- | Compute a depth-first ordering, returns @Left@ when a cycle is detected
dfs
  :: Ord a
  => (a -> [a])
  -- ^ graph successors
  -> [a]
  -> Either [a] [a]
dfs ds = fmap concat . mapM (alg mempty)
 where
  alg (acc, visited) x
    | Set.member x visited = Left (x : acc)
    | let ds' = ds x
    , not (null ds') =
        concat <$> mapM (alg (x : acc, Set.insert x visited)) (ds x)
    | otherwise = Right (x : acc)

parseTerm :: Recalc t => CellRef -> (String, CellType) -> Parsed t
parseTerm (sheetId@(uri, sheetName), ca) (str, ct) = parse (runReaderT (parseCell ct) sheetId <* eof) locStr str
 where
  locStr = Text.unpack ("[" <> Text.pack (uriPath uri) <> "]" <> sheetName <> "!") <> showExcel26 ca

isn't :: Meta -> Bool
isn't _ = True

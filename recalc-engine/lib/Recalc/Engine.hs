{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Recalc.Engine
Description : The recalculation engine for generic spreadsheet languages.

The evaluation strategy is inspired by the paper "Build systems à la carte",
it maintains a 'DocumentStore' that tracks types, values and meta data (dummied
out for now) for all cells, and a dependency graph of the cells for checking
which cells are dirty (need to be recomputed).

__References:__
  Andrey Mokhov, Neil Mitchell and Simon Peyton Jones.
  [Build Systems à la Carte](https://dl.acm.org/doi/10.1145/3236774).
  Proceedings of the ACM on Programming Languages, Volume 2, Issue ICFP.
-}
module Recalc.Engine
  ( -- * Engine Operations
    Inputs
  , CycleOf
  , ResultsOf
  , recalc
  , recalcAll
  , parseTerm

    -- * Spreadsheet Language Interface
  , Recalc (..)

    -- ** Fetch Monad
  , Fetch
  , FetchOf
  , FetchError (..)

    -- *** Actions
  , fetchType
  , fetchValue
  , throwSemanticError

    -- *** Run Action (for debugging purposes)
  , runFetchWith

    -- * Engine State
  , EngineStateOf
  , newEngineState
  , engineEnv
  , engineDocs

    -- ** State Updates
  , mapEnv
  , mapDocs
  , deleteSheetId

    -- ** Types
  , DocumentStoreOf
  , Document (..)
  , SheetOf
  , Cell (..)
  , CellOf
  , CellType (..)
  , Meta (..)

    -- ** re-export core definitions
  , module Recalc.Engine.Core
  ) where

import Build.Rebuilder (dirtyBitRebuilder)
import Build.Scheduler (Chain, restarting)
import Build.Store (Store, getInfo, getValue, initialise)
import Build.Task (Task, Tasks)

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
import Data.List (foldl')
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

{- Interface -}

-- | The spreadsheet language interface. An instantiation of it can be used
-- to declare the semantics of the provided language in a spreadsheet environment.
class (Pretty t, Pretty (TypeOf t), Pretty (ValueOf t)) => Recalc t where
  -- | a custom environment (e.g. to bind global variables)
  type EnvOf t

  -- | a custom error type (errors during type inference, or evaluation)
  type ErrorOf t

  -- | the types of values @t@
  type TypeOf t

  -- | the value @t@ evaluates to
  type ValueOf t

  -- | the language may differentiate between cells parsed as formula and value
  parseCell :: CellType -> ReaderT SheetId (Parsec Void String) t

  -- | specify how to compute the cell references a term depends on
  depsOf :: t -> Set CellRangeRef

  -- | specify how a term's type is inferred
  infer :: t -> FetchOf t (TypeOf t)
  infer = fmap fst . inferElaborate

  -- | specify how a term's type is inferred with term elaboration
  -- (defaults to identity elaboration)
  inferElaborate :: t -> FetchOf t (TypeOf t, t)
  inferElaborate t = (,t) <$> infer t

  -- | specify how a term is evaluated
  eval :: t -> FetchOf t (ValueOf t)

  {-# MINIMAL (infer | inferElaborate), parseCell, depsOf, eval #-}

{- Engine -}

-- | The kind of information we are fetching
data Kind = Type | Value deriving (Eq, Ord, Show)

-- | The spreadsheet engine can be queried for re-evaluation of
-- cells (types and values) and volatile results.
data Ix = CellIx !Kind !CellRef | VolatileIx
  deriving (Eq, Ord, Show)

-- assume megaparsec for now
type ParseError = ParseErrorBundle String Void

-- | Evaluation of cells can always fail due to invalid formulas or refs
data FetchError err
  = InvalidFormula ParseError
  | RefError
  | SemanticError err
  deriving (Eq, Show)

-- | Fetch callbacks can fail (using 'MonadError') and have access to other
-- cells (see 'fetchType', 'fetchValue'), and the custom context (using 'MonadReader')
newtype Fetch env err r a
  = Fetch
      ( forall f
         . Monad f
        => ReaderT (Ix -> ExceptT (FetchError err) f r, env) (ExceptT (FetchError err) f) a
      )
  deriving (Functor)

type FetchOf t = Fetch (EnvOf t) (ErrorOf t) (Either (TypeOf t, t) (ValueOf t))

fetch :: Ix -> Fetch env err v v
fetch ix = Fetch $ lift . ($ ix) =<< asks fst

-- | fetch the value stored at a cell reference
fetchValue :: CellRef -> FetchOf t (ValueOf t)
fetchValue = fmap projectRight . fetch . CellIx Value

-- | fetch the type of the value at a cell reference
fetchType :: CellRef -> FetchOf t (TypeOf t)
fetchType = fmap (fst . projectLeft) . fetch . CellIx Type

runFetchWith
  :: env
  -> (Kind -> CellRef -> Except (FetchError err) v)
  -- ^ callback that returns a value/type (depending on its kind)
  -> Fetch env err v a
  -- ^ a fetch task to run
  -> Either (FetchError err) a
runFetchWith env f (Fetch v) = runExcept . runReaderT v . (,env) $ \case
  CellIx k ref -> f k ref
  _ -> throwError RefError

-- Applicative instance (cannot be derived due to the higher-rank type)
instance Applicative (Fetch env err r) where
  Fetch f <*> Fetch x = Fetch (f <*> x)
  pure a = Fetch (pure a)

-- Applicative instance (cannot be derived due to the higher-rank type)
instance Monad (Fetch env err r) where
  Fetch x >>= f = Fetch (x >>= \y' -> let Fetch y = f y' in y)

instance MonadError (FetchError err) (Fetch env err r) where
  throwError err = Fetch (throwError err)
  catchError (Fetch x) handler =
    Fetch
      $ x `catchError` (\e' -> let Fetch e = handler e' in e)

-- | throw user-defined error
throwSemanticError :: err -> Fetch env err r a
throwSemanticError = throwError . SemanticError

instance MonadReader env (Fetch env err r) where
  ask = Fetch (asks snd)
  local f (Fetch x) = Fetch (local (second f) x)

type DocumentStore err f t v = Map URI (Document err f t v)
type DocumentStoreOf f = DocumentStore (ErrorOf f) f (TypeOf f) (ValueOf f)

-- a univer document
data Document err f t v = Document
  { sheetOrder :: ![Text]
  -- ^ sheets may be re-ordered (visually)
  , sheets :: !(Map SheetName (Sheet err f t v))
  -- ^ store of sheets by their name
  }

instance (Show err, Pretty f, Pretty t, Pretty v) => Show (Document err f t v) where
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

type Sheet err f t v = Map CellAddr (Cell err f t v)
type SheetOf f = Sheet (ErrorOf f) f (TypeOf f) (ValueOf f)

type Parsed = Either ParseError

data Meta = Meta deriving (Show)

data CellType
  = -- | cell formulas (starting with @=@)
    CellFormula
  | -- | regular values
    CellValue
  deriving (Show)

data Cell err f t v = Cell
  { cell :: !(Maybe ((String, CellType), Maybe (f, Maybe (t, Maybe v))))
  -- ^ a cell maybe only meta data. when it has a term (and therefore the input
  -- and celltype are available), it can have a type. if it has a type, it can
  -- have a value.
  , cellDeps :: !(Set CellRangeRef)
  -- ^ dependencies of the cell
  , cellError :: !(Maybe (FetchError err))
  -- ^ when the cell encounters an error (during inference or evaluation)
  , cellMeta :: !Meta
  -- ^ meta data (currently useless, should include style info like it once did)
  }

type CellOf f = Cell (ErrorOf f) f (TypeOf f) (ValueOf f)

instance (Show err, Pretty f, Pretty t, Pretty v) => Show (Cell err f t v) where
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

cellOf :: Parsed t -> Maybe (t, Maybe (TypeOf t, Maybe (ValueOf t)))
cellOf = \case
  Right t -> Just (t, Nothing)
  _ -> Nothing

errorOf :: Parsed a -> Maybe (FetchError e)
errorOf = \case
  Left err -> Just (InvalidFormula err)
  _ -> Nothing

-- set an error
setError :: FetchError err -> Cell err f t v -> Cell err f t v
setError err c = c{cellError = Just err}

-- set type (setting value to @Nothing@) when a term is stored
setType :: (t, f) -> Cell err f t v -> Cell err f t v
setType (typ, term) c =
  c
    { cell = second (fmap (const (term, Just (typ, Nothing)))) <$> cell c
    , cellError = Nothing
    }

-- set value when a term and type are stored
setValue :: v -> Cell err f t v -> Cell err f t v
setValue val c =
  c
    { cell = second (fmap (second ((,Just val) . fst <$>))) <$> cell c
    , cellError = Nothing
    }

alterCell
  :: CellRef
  -> (Maybe (Cell err f t v) -> Maybe (Cell err f t v))
  -> DocumentStore err f t v
  -> DocumentStore err f t v
alterCell ((uri, sheetName), ca) f = (`Map.update` uri) $ \doc ->
  Just
    doc
      { sheets = ($ sheets doc) . (`Map.update` sheetName) $ \sheet ->
          Just (Map.alter f ca sheet)
      }

lookupCell :: CellRef -> DocumentStore err f t v -> Maybe (Cell err f t v)
lookupCell ((uri, sheetName), ca) =
  Map.lookup ca <=< Map.lookup sheetName . sheets <=< Map.lookup uri

lookupCellTerm :: CellRef -> DocumentStore err f t v -> Maybe (f, CellType)
lookupCellTerm ref ds = do
  (ct, x) <- fmap (first snd) . cell =<< lookupCell ref ds
  (,ct) . fst <$> x

lookupCellType :: CellRef -> DocumentStore err f t v -> Maybe t
lookupCellType ref =
  fmap fst . snd <=< snd <=< cell <=< lookupCell ref

lookupCellTermType :: CellRef -> DocumentStore err f t v -> Maybe (t, f)
lookupCellTermType ref ds = do
  (f, _) <- lookupCellTerm ref ds
  (,f) <$> lookupCellType ref ds

lookupCellValue :: CellRef -> DocumentStore err f t v -> Maybe v
lookupCellValue ref ds = case lookupCellError ref ds of
  Just _ -> Nothing
  _ -> snd =<< snd =<< snd =<< cell =<< lookupCell ref ds

lookupCellError :: CellRef -> DocumentStore err f t v -> Maybe (FetchError err)
lookupCellError ref ds = cellError =<< lookupCell ref ds

data EngineState env err f t v = EngineState
  { engineEnv :: !env
  , _engineChain :: !(Chain Ix)
  , engineDocs :: !(DocumentStore err f t v)
  , engineDeps :: !(Slow CellRef)
  }

type EngineStateOf t = EngineState (EnvOf t) (ErrorOf t) t (TypeOf t) (ValueOf t)

instance (Show err, Pretty f, Pretty t, Pretty v) => Show (EngineState env err f t v) where
  show EngineState{engineDocs = docs} = "EngineState {" <> show (showKeys docs) <> "}"
   where
    -- make sure URIs show with quotes
    showKeys = Map.mapKeys show

-- | create a new engine state. the engine state is completely empty, no handles on
-- any documents.
newEngineState :: env -> EngineState env err f t v
newEngineState env = EngineState env [] mempty Deps.empty

-- | modify the custom environment
mapEnv :: (env -> env) -> EngineState env err f t v -> EngineState env err f t v
mapEnv f st = st{engineEnv = f (engineEnv st)}

-- | modify the document store
mapDocs
  :: (DocumentStore err f t v -> DocumentStore err f t v)
  -> EngineState env err f t v
  -> EngineState env err f t v
mapDocs f st = st{engineDocs = f (engineDocs st)}

-- | delete a whole sheet by its id from the dependency graph (useful when renaming a sheet)
deleteSheetId :: SheetId -> EngineState env err f t v -> EngineState env err f t v
deleteSheetId sheetId st = st{engineDeps = Deps.deleteSheetId sheetId (engineDeps st)}

-- | inputs are given by the location, maybe an input (and its type), and meta data
type Inputs = [(CellRef, (Maybe (String, CellType), Meta))]

-- | when there is a dependency cycle, the cycle's locations are returned
-- together with the data stored there
type CycleOf t = [(CellRef, CellOf t)]

-- | the results are locations and the data stored there
type ResultsOf t = [(CellRef, CellOf t)]

-- | recalculate the whole sheet (marking everything as dirty),
-- or return the cycle if there is a dependency cycle.
recalcAll
  :: forall t
   . Recalc t
  => EngineStateOf t
  -> (Either (CycleOf t) (ResultsOf t), EngineStateOf t)
recalcAll es@EngineState{engineDocs} =
  foldl' (\(_, st) input -> recalc [input] st) (Right [], es) inputs
 where
  inputs :: Inputs
  inputs =
    [ (((uri, sheet), ca), (Just (s, ct), Meta))
    | (uri, Document{..}) <- Map.toList engineDocs
    , (sheet, sheetMap) <-
        [ (sheet, sheetMap)
        | sheet <- sheetOrder
        , Just sheetMap <- [Map.lookup sheet sheets]
        ]
    , (ca, Cell{cell = Just ((s, ct), _)}) <- Map.toList sheetMap
    ]

-- | recalculate the inputs and everything that depends on those (transitively),
-- or return the cycle if there is a dependency cycle.
recalc
  :: forall f
   . Recalc f
  => Inputs
  -> EngineStateOf f
  -> (Either (CycleOf f) (ResultsOf f), EngineStateOf f)
recalc inputs (EngineState env chain docs deps) =
  let
    validatedInputs :: [(CellRef, Maybe ((String, CellType), Parsed f), Meta)]
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

          oldDeps, newDeps :: Set CellRangeRef
          oldDeps = maybe mempty cellDeps (lookupCell ref docs)
          newDeps = maybe mempty (foldMap depsOf . snd) validInput

          updateDepsEndo :: Endo (Slow CellRef)
          updateDepsEndo = updateDeps (ref, oldDeps, newDeps)

          -- preliminary entry for the document store
          cell :: CellOf f
          cell =
            Cell
              (second cellOf <$> validInput) -- type and value are not known yet
              newDeps
              (errorOf . snd =<< validInput)
              meta

          -- entry is deleted when there is neither a term nor meta data
          updateCellEndo :: Endo (DocumentStoreOf f)
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
  :: Recalc f
  => EnvOf f
  -> DocumentStoreOf f
  -> Spreadsheets (Either (FetchError (ErrorOf f)) (Either (TypeOf f, f) (ValueOf f)))
spreadsheetsOf env ds =
  (runFetch env <$>) . \case
    CellIx k ref -> alg k . fst <$> lookupCellTerm ref ds
    VolatileIx -> Just (throwError RefError)
 where
  alg = \case
    Type -> fmap Left . inferElaborate
    Value -> fmap Right . eval

type BuildStore err v = Store (Ix -> Bool, Chain Ix) Ix (Either (FetchError err) v)
type BuildStoreOf t = BuildStore (ErrorOf t) (Either (TypeOf t, t) (ValueOf t))

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
  -> EngineStateOf t
  -> (ResultsOf t, EngineStateOf t)
recalc' dirty (EngineState env chain docs deps) =
  let
    build ds = restarting dirtyBitRebuilder (spreadsheetsOf env ds)

    typecheck, run :: DocumentStoreOf t -> BuildStoreOf t -> [CellRef] -> BuildStoreOf t
    typecheck ds = foldr (build ds . CellIx Type)
    run ds = foldr (build ds . CellIx Value)

    ix ds = \case
      CellIx kind ref
        | let
            lookupCell' = case kind of
              Type -> (fmap Left .) . lookupCellTermType
              Value -> (fmap Right .) . lookupCellValue
        , Just result <- lookupCell' ref ds ->
            pure result
        -- if the value cannot be found it may be invalid
        | Just err <- lookupCellError ref ds -> throwError err
        -- if there is no entry at all, it's a reference error
        | otherwise -> throwError RefError
      VolatileIx -> error "not implemented"

    {- recompute -}

    -- typecheck
    store' = typecheck docs (initialise (setDirty dirty, chain) (ix docs)) dirty

    (typeErrors, newTypes) = second (map (second projectLeft)) (getValues Type dirty store')

    -- evaluate well-typed terms
    wellTyped = map fst newTypes

    -- propagate elaborated terms
    docs' = appEndo (foldMap (mapCell setType) newTypes) docs

    store'' = run docs' (initialise (setDirty wellTyped, []) (ix docs')) wellTyped

    (runtimeErrors, newValues) = second (map (second projectRight)) (getValues Value wellTyped store'')

    {- calculate new EngineState (recalculation of dirty cells does not affect deps!) -}
    chain' = getChain store''

    -- set a result (error, type, or value)
    mapCell setter (ref, x) = Endo (alterCell ref (Just . setter x =<<))

    -- already updated types + elaborated terms, now do values followed by errors
    docs'' =
      (`appEndo` docs')
        $ foldMap (mapCell setError) (typeErrors ++ runtimeErrors)
          <> foldMap (mapCell setValue) newValues

    engineState = EngineState env chain' docs'' deps

    -- return all recalculated cells
    results = catMaybes [(ref,) <$> lookupCell ref docs'' | ref <- dirty]
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

-- | parse a cell (depends on the cell's location) returns formula or value (both as term)
parseTerm :: Recalc t => CellRef -> (String, CellType) -> Parsed t
parseTerm (sheetId@(uri, sheetName), ca) (str, ct) = parse (runReaderT (parseCell ct) sheetId <* eof) locStr str
 where
  locStr = Text.unpack ("[" <> Text.pack (uriPath uri) <> "]" <> sheetName <> "!") <> showExcel26 ca

isn't :: Meta -> Bool
isn't _ = True

{- only used for extracting results from type checking @Left@ and evaluation @Right@): -}

projectLeft :: Either a b -> a
projectLeft (Left a) = a
projectLeft _ = error "the impossible happened (Left)"

projectRight :: Either a b -> b
projectRight (Right b) = b
projectRight _ = error "the impossible happened (Right)"

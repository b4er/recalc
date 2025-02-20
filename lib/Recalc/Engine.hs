{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Recalc.Engine
Description : Recalculation engine for 'Spreadsheets'.

The evaluation strategy is inspired by the paper "Build systems à la carte",
it maintains a 'DocumentStore' that tracks types, values and meta data for
all cells.

The implementation maintains a 'DocumentStore' that tracks types, values
and meta data for all cells.

References:
  Andrey Mokhov, Neil Mitchell and Simon Peyton Jones.
  Build Systems à la Carte.
  Proceedings of the ACM on Programming Languages, Volume 2, Issue ICFP.
-}
module Recalc.Engine
  ( -- * Cell-IDs
    SheetId
  , URI
  , Text
  , CellAddr
  , CellRange

    -- * Generic spreadsheet language interface
  , Language (..)
  , FetchError (..)
  , errorTitle
  , Isn't (..)
  , Meta (..)
  , Ix (..)
  , module Recalc.Engine
  , DS
  , newEngineState
  , modifyDocs
  , insertDocument
  , insertSheet
  , updateDocument
  , fetchType
  , fetchValue
  , getEnv
  , localEnv
  ) where

import Build.Rebuilder (dirtyBitRebuilder)
import Build.Scheduler (Chain, restarting)
import Build.Store (Store, getInfo, getValue, initialise)
import Control.Monad.Except (throwError)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Strict (get, put)
import Data.Either (partitionEithers)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Recalc.Engine.Core (CellAddr, CellRange, SheetId)
import Recalc.Engine.DependencyMap (Slow)
import Recalc.Engine.DependencyMap qualified as Deps
import Recalc.Engine.DocumentStore
import Recalc.Engine.Language hiding (Fetch)
import Recalc.Engine.Language qualified as Language
import Recalc.Engine.Monad hiding (EngineState, EngineT, runEngineT)
import Recalc.Engine.Monad qualified

type EngineT doc sheet cell term f =
  Recalc.Engine.Monad.EngineT Slow doc sheet cell term f

type Engine doc sheet cell term =
  EngineT doc sheet cell term Identity

type EngineState = Recalc.Engine.Monad.EngineState Slow

type Fetch term a = Language.Fetch (EnvOf term) (ErrorOf term) (ValueOf term) a

runEngineT
  :: EngineT doc sheet cell term f a
  -> EngineState doc sheet cell term
  -> f (a, EngineState doc sheet cell term)
runEngineT = Recalc.Engine.Monad.runEngineT

{- input validation -}

type Parsed = Either (ParseErrorBundle String Void)

-- | input data can be arbitrary
class Language (TermOf dat) => Input dat where
  type TermOf dat
  termOrValueOf
    :: SheetId
    -> CellAddr
    -> dat
    -> Maybe (Parsed (Either (TermOf dat) (ValueOf (TermOf dat))))

  type MetaOf dat
  metaOf :: dat -> MetaOf dat

-- | meta changes which don't map to terms/values,
-- do not affect re-computation
type MetaChangesOf dat = [(CellAddr, MetaOf dat)]

-- | validated cells, all changes affect recomputation
type ChangesOf dat =
  ( [(CellAddr, MetaOf dat, ParseErrorBundle String Void)]
  , [(CellAddr, MetaOf dat, ValueOf (TermOf dat))]
  , [(CellAddr, MetaOf dat, TermOf dat)]
  )

validateCells
  :: Input dat
  => SheetId
  -> [(CellAddr, dat)]
  -> (MetaChangesOf dat, ChangesOf dat)
validateCells sheetId = go [] [] [] []
 where
  go meta errors values formulas ((ca, dat) : cs) =
    case termOrValueOf sheetId ca dat of
      Nothing -> go ((ca, metaOf dat) : meta) errors values formulas cs
      Just q -> case q of
        Left e -> go meta ((ca, metaOf dat, e) : errors) values formulas cs
        Right (Left x) -> go meta errors values ((ca, metaOf dat, x) : formulas) cs
        Right (Right v) -> go meta errors ((ca, metaOf dat, v) : values) formulas cs
  go meta errors values formulas [] = (meta, (errors, values, formulas))

updateMeta
  :: (Isn't cell, Meta cell, Monad f)
  => SheetId
  -> [(CellAddr, cell)]
  -> EngineT doc sheet cell term f ()
updateMeta sheetId meta =
  modifyDocs $ \ds -> foldl' alg ds meta
 where
  alg ds (ca, c) = alterCellMeta sheetId ca (merge c) ds

-- | recompute new store from validated new inputs
recompute
  :: forall dat doc sheet f
   . ( Monad f
     , Input dat
     , Isn't (MetaOf dat)
     )
  => SheetId
  -> ChangesOf dat
  -> EngineT
      doc
      sheet
      (MetaOf dat)
      (TermOf dat)
      f
      ( Either
          [(SheetId, CellAddr)]
          [((SheetId, CellAddr), Either (FetchError (ErrorOf (TermOf dat))) (ValueOf (TermOf dat)))]
      )
recompute sheetId (errors, values, formulas) = do
  Recalc.Engine.Monad.EngineState chain documentStore depMap <- get
  let
    -- static environment
    env = newEnv @(TermOf dat) sheetId

    -- delete outdated ranges from errors and values
    deleteOldDeps :: [(CellAddr, d, a)] -> Slow CellAddr -> Slow CellAddr
    deleteOldDeps xs dm =
      foldl'
        ( \dm' (ca, _, _) ->
            let
              oldDeps = fromMaybe mempty (lookupCellDeps sheetId ca documentStore)
            in
              updateDeps Deps.delete ca oldDeps dm'
        )
        dm
        xs

    -- delete outdated ranges and insert new ones computed from expressions
    insertNewDeps
      :: Slow CellAddr -> ([(CellAddr, (TermOf dat, Set CellRange, MetaOf dat))], Slow CellAddr)
    insertNewDeps dm =
      foldl'
        ( \(acc, dm') (ca, c, x) ->
            let oldDeps = fromMaybe mempty (lookupCellDeps sheetId ca documentStore)
                newDeps = deps x
            in  ( (ca, (x, newDeps, c)) : acc
                , updateDeps Deps.insert ca (newDeps \\ oldDeps)
                    $ updateDeps Deps.delete ca (oldDeps \\ newDeps) dm'
                )
        )
        ([], dm)
        formulas

    -- compute new dependency map and successor function of dependency graph:
    (formulas', depMap') = insertNewDeps . deleteOldDeps values $ deleteOldDeps errors depMap

    -- cells that need recomputation for each changed address (or circular error)
    xrecompute =
      dfs (Deps.query depMap')
        $ map (sheetId,) (map fst3 errors ++ map fst3 values ++ map fst3 formulas)

    {- update document store (insert updated cells, keeping track of 1dependencies) -}
    documentStore' :: DS doc sheet (MetaOf dat) (TermOf dat)
    documentStore' =
      foldl'
        ( \ds (ca, (e, r, c)) -> setCell sheetId ca (cellTerm e r c) ds
        )
        ( foldl'
            ( \ds (ca, c, v) -> setCell sheetId ca (cellValue (inferValue @(TermOf dat) env v) v c) ds
            )
            ( foldl'
                ( \ds (ca, c, _) -> setCell sheetId ca (cellError c) ds
                )
                documentStore
                errors
            )
            values
        )
        formulas'

    {- if there were no circular errors: do the recomputation steps -}
    (xchanges, chain', documentStore'') = case xrecompute of
      Right (dirty :: [((URI, Text), CellAddr)]) ->
        let
          spreadsheets = spreadsheetsOf env documentStore'

          -- current store
          store
            :: Store (Ix -> Bool, Chain Ix) Ix (Either (FetchError (ErrorOf (TermOf dat))) (ValueOf (TermOf dat)))
          store = initialise (setDirty dirty, chain) $ \case
            Cell kind sheetId' ca'
              | Just v <- (if kind == Type then lookupCellType else lookupCellValue) sheetId' ca' documentStore' ->
                  pure v
              | Just err <- lookup ca' [(k, v) | (k, _, v) <- errors] ->
                  throwError (InvalidFormula err)
              | otherwise -> throwError RefError
            Volatile -> error "not implemented"

          -- typecheck
          tc = foldr (restarting dirtyBitRebuilder spreadsheets . uncurry (Cell Type))
          store' = store `tc` dirty

          (typeErrors, newTypes) =
            partitionEithers
              [ biseq (u, getValue (Cell Type s ca) store')
              | u@(s, ca) <- dirty
              ]

          -- evaluate
          run = foldr (restarting dirtyBitRebuilder spreadsheets . uncurry (Cell Value))

          store'' = store' `run` [d | d <- dirty, d `notElem` map fst typeErrors]

          newValues = [(u, getValue (Cell Value s ca) store'') | u@(s, ca) <- dirty]

          setDocumentStoreValue ((si, ca), Left _) = setCellError si ca
          setDocumentStoreValue ((si, ca), Right v') = setCellValue si ca v'

          setDocumentStoreType ((si, ca), t) = setCellType si ca t

          setDocumentStoreError ((si, ca), _e) = setCellError si ca
        in
          -- propagate new calc-chain and documentStore updated with new values
          ( Right newValues
          , snd (getInfo store'')
          , foldr
              setDocumentStoreError
              ( foldr
                  setDocumentStoreValue
                  (foldr setDocumentStoreType documentStore' newTypes)
                  newValues
              )
              typeErrors
          )
      Left err -> (Left err, chain, documentStore')

  -- return the new changes and update storage
  xchanges <$ put (Recalc.Engine.Monad.EngineState chain' documentStore'' depMap')
 where
  updateDeps f ca ds dm = Set.foldr' (\a -> f sheetId a ca) dm ds

  setDirty cs = \case
    Cell _k x ca -> (x, ca) `elem` cs
    Volatile{} -> True

  fst3 (a, _, _) = a

  biseq :: (a, Either b c) -> Either (a, b) (a, c)
  biseq (x, y) = either (Left . (x,)) (Right . (x,)) y

  -- \| Compute a depth-first ordering, returns @Left@ when a cycle is detected
  dfs
    :: Ord a
    => (a -> [a])
    -- \^ graph successors
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

{-|
Module      : Recalc.Core.DependencyMap
Description : A dependency map interface for managing cell dependencies.

Simple (slow but fast enough) implementation.
-}
module Recalc.Engine.DependencyMap (DependencyMap(..), Slow) where

import Control.Arrow (second)
import Data.Functor.Classes (Show1 (..))
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import Recalc.Engine.Language (CellAddr, CellRange, SheetId)

-- point :: x -> (x, x)
-- point x = (x, x)

contains :: CellRange -> CellAddr -> Bool
contains ((x0, y0), (x1, y1)) (x, y) =
  and
    [ min x0 x1 <= x
    , x <= max x0 x1
    , min y0 y1 <= y
    , y <= max y0 y1
    ]

class Show1 t => DependencyMap t where
  query :: t a -> (SheetId, CellAddr) -> [(SheetId, a)]
  delete :: Eq a => SheetId -> CellRange -> a -> t a -> t a
  insert :: SheetId -> CellRange -> a -> t a -> t a
  empty :: t a

-- FIXME: use sth. like Data.IntervalMap.FingerTree
newtype Slow a = Slow (Map SheetId [(CellRange, (SheetId, a))])
  deriving (Show)

instance Show1 Slow where
  liftShowsPrec sp _ x (Slow m) =
    showString . show
      $ second (map (second (second (($ "") . sp x)))) <$> Map.assocs m

instance DependencyMap Slow where
  query (Slow m) (sid, ca) =
    [a | (x, a) <- fromMaybe [] (Map.lookup sid m), x `contains` ca]
  delete sid x a (Slow m) =
    Slow
      $ Map.update
        (\xs -> case filter (/= (x, (sid, a))) xs of [] -> Nothing; xs' -> Just xs')
        sid
        m
  insert sid x a (Slow m) = Slow $ Map.alter (Just . ((x, (sid, a)) :) . fromMaybe []) sid m
  empty = Slow mempty

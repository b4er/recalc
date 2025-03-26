{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Recalc.Array
Description : Core array operations.

This module provides a few core operations on multi-dimensional arrays.
-}
module Recalc.Array (Array, Array' (..), shape, fromList, toList, toMassiv) where

import Data.Massiv.Array (B, Ix1)
import Data.Massiv.Array qualified as Array

data Array' r a = Array [Int] (Array.Array r Ix1 a)

deriving instance Eq (Array.Array r Ix1 a) => Eq (Array' r a)
deriving instance Show (Array.Array r Ix1 a) => Show (Array' r a)

instance Functor (Array.Array r Ix1) => Functor (Array' r) where
  fmap f (Array dims arr) = Array dims (f <$> arr)

instance Foldable (Array.Array r Ix1) => Foldable (Array' r) where
  foldMap f (Array _dims arr) = foldMap f arr

instance Traversable (Array.Array r Ix1) => Traversable (Array' r) where
  traverse f (Array dims arr) = Array dims <$> traverse f arr

type Array = Array' B

shape :: Array' r a -> [Int]
shape (Array dims _) = dims

fromList :: [Int] -> [a] -> Array a
fromList dims = Array dims . Array.fromList Array.Seq

toList :: Array a -> [a]
toList (Array _ arr) = Array.toList arr

toMassiv :: Array.Size r => Array' r e -> IO (Array.Array r Int e)
toMassiv (Array dims arr) = do
  sz <- Array.mkSzM @_ @IO (length dims)
  Array.resizeM sz arr

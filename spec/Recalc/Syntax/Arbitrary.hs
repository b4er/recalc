{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recalc.Syntax.Arbitrary () where

import Data.Text qualified as Text
import Test.QuickCheck

import Recalc.Syntax.Term

instance Arbitrary CaseInsensitive where
  arbitrary = CaseInsensitive . Text.pack . varName <$> choose @Int (0, 9)
   where
    varName i
      | i == 0 = "x"
      | otherwise = "x" ++ show i

instance Arbitrary (Term Infer) where
  arbitrary = genTermI 0 5

instance Arbitrary (Term Check) where
  arbitrary = genTermC 0 5

genTermI :: Int -> Int -> Gen (Term Infer)
genTermI bindersCount depth =
  choose @Int (if depth > 0 then 0 else 3, 5) >>= \case
    0 ->
      Ann
        <$> genTermC bindersCount (depth - 1)
        <*> genTermI bindersCount (depth - 1)
    1 ->
      Pi
        <$> arbitrary
        <*> genTermC bindersCount (depth - 1)
        <*> genTermC (bindersCount + 1) (depth - 1)
    2 ->
      (:$)
        <$> genTermI bindersCount (depth - 1)
        <*> genTermC bindersCount (depth - 1)
    3 | bindersCount > 0 -> Bound <$> choose (0, bindersCount - 1)
    4 -> Free . Global <$> arbitrary
    _ -> pure (Set 0)

genTermC :: Int -> Int -> Gen (Term Check)
genTermC bindersCount depth =
  choose @Int (0, if depth > 0 then 1 else 0) >>= \case
    0 -> Inf <$> genTermI bindersCount (depth - 1)
    _ -> Lam <$> arbitrary <*> genTermC (bindersCount + 1) (depth - 1)

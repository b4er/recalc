{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.Syntax.Arbitrary
Description : Fully arbitrary terms.

Implementation of the "Arbitrary" type class for terms, the
arbitrary terms are fully random (they may not typecheck). A
better approach in general would be to produce a restricted
type and generate a term for it (à la Djinn).
-}
module Recalc.Syntax.Arbitrary (SheetName (..), Set0 (..)) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI
import Test.QuickCheck

import Recalc.Syntax.Term

instance Arbitrary CaseInsensitive where
  arbitrary = CaseInsensitive . Text.pack . varName <$> choose @Int (0, 9)
   where
    varName i
      | i == 0 = "x"
      | otherwise = "x" ++ show i

instance CoArbitrary CaseInsensitive where
  coarbitrary =
    foldr (\c f -> variant (fromEnum c) . f) id
      . Text.unpack
      . caseInsensitive

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

newtype Set0 = Set0 (Term Infer)
  deriving (Show) via (Term Infer)

instance Arbitrary Set0 where
  arbitrary = Set0 <$> genSet0Term 0 0

genSet0Term :: Int -> Int -> Gen (Term Infer)
genSet0Term depth bindersCount =
  oneof
    $ pure (Lit Bool)
      : pure (Lit Int)
      : map (pure . Bound) [0 .. bindersCount - 1]
        <> nested
 where
  genSet0TermC d b = Inf <$> genSet0Term (d - 1) b

  nested
    | 0 < depth =
        [ (`Ann` Set 0) <$> genSet0TermC (depth - 1) bindersCount
        , Pi
            <$> arbitrary
            <*> genSet0TermC (depth - 1) bindersCount
            <*> genSet0TermC (depth - 1) (bindersCount + 1)
        ]
    | otherwise = []

{-
instance Arbitrary Name where arbitrary = Global <$> arbitrary
instance CoArbitrary Name where coarbitrary = genericCoarbitrary

instance Arbitrary Neutral where arbitrary = genNeutral 10
instance CoArbitrary Neutral where coarbitrary = genericCoarbitrary

instance Arbitrary Value where arbitrary = genValue 10
instance CoArbitrary Value where coarbitrary = genericCoarbitrary

genNeutral :: Int -> Gen Neutral
genNeutral depth
  | 0 < depth = oneof [genNFree, NApp <$> genNeutral (depth - 1) <*> genValue (depth - 1)]
  | otherwise = genNFree
 where
  genNFree = NFree . Global <$> arbitrary

genValue :: Int -> Gen Value
genValue depth =
  oneof
    $ (VSet . abs <$> arbitrary)
      : (VNeutral <$> genNeutral depth)
      : nested
 where
  nested
    | 0 < depth = [VLam <$> arbitrary <*> arbitrary]
    | otherwise = []
-}

instance Arbitrary URI where
  arbitrary =
    oneof
      $ map
        (pure . fromJust . parseURI)
        [ "file:///simple"
        , "file://host/simple"
        , "file:///C:/Users/Me/file.xlsx"
        , "file:///home/user/file.xlsx"
        , "file:///file%20with%20spaces.xlsx"
        , "ftp://ftp.example.com/file"
        , "http://example.com/file.xlsx"
        , "https://example.com/path/to/file"
        , "https://example.com/a-b_c~d/file.xlsx"
        , "https://example.com/query?name=value&other=value"
        , "https://example.com/#fragment"
        , "https://user:pass@example.com/"
        , "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ=="
        , "urn:isbn:0451450523"
        ]

newtype SheetName = SheetName Text
  deriving (Show) via SheetName

instance Arbitrary SheetName where
  arbitrary =
    oneof
      $ map
        (pure . SheetName)
        [ "sheet"
        , "Sheet 1"
        , "uh,which,chars,are,allowed"
        , "öh more"
        ]

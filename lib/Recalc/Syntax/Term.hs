{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Recalc.Syntax.Term
Description : Terms and Values.

Types definitions for a very basic dependently typed lambda calculus based on
"A tutorial implementation of a dependently typed lambda calculus".
-}
module Recalc.Syntax.Term where

import Data.Array.Dynamic (Array)
import Data.Array.Dynamic qualified as Array
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.URI
import Numeric
import Prettyprinter

import Recalc.Engine

-- * Terms

data Mode = Check | Infer deriving (Eq, Show)

-- ** Names

newtype CaseInsensitive = CaseInsensitive {originalText :: Text}

caseInsensitive :: CaseInsensitive -> Text
caseInsensitive = Text.toLower . originalText

instance Eq CaseInsensitive where
  (==) = (==) `on` caseInsensitive

instance Ord CaseInsensitive where
  compare = compare `on` caseInsensitive

instance Show CaseInsensitive where
  show = show . originalText

data Name
  = Global !CaseInsensitive
  | -- | when passing binder, bound names are converted to locally free ones
    Local !(Maybe CaseInsensitive) !Int
  | -- | same but during quoting
    Quote !Int
  deriving (Generic, Show)

instance Eq Name where
  Global n == Global n' = n == n'
  Local _ i == Local _ j = i == j
  Quote i == Quote j = i == j
  _ == _ = False

instance Ord Name where
  compare (Global n) (Global n') = compare n n'
  compare Global{} _ = LT
  compare Local{} Global{} = GT
  compare (Local _ i) (Local _ j) = compare i j
  compare Local{} _ = LT
  compare (Quote i) (Quote j) = compare i j
  compare Quote{} _ = GT

instance IsString Name where
  fromString :: String -> Name
  fromString = Global . CaseInsensitive . Text.pack

pat :: Text -> Maybe CaseInsensitive
pat = Just . CaseInsensitive

-- | cell references may be given @[file_name.rc]sheet!C3@, @sheet!C3@,
-- or @C3@; corresponding to @FullySpecified@, @SheetOnly@, and
-- @Unspecified@ respectively
data RefInfo = FullySpecified | SheetOnly | Unspecified
  deriving (Show)

-- | types of literals
data Lit = Bool | Int
  deriving (Eq, Show)

-- | values of literals
data LitOf
  = BoolOf !Bool
  | IntOf !Int
  deriving (Eq, Show)

-- | a tensor (type) is described by the number of dimensions
-- of each component
newtype TensorDescriptor = TensorDescriptor
  {dims :: [Term Check]}
  deriving (Eq, Show)

data Term (m :: Mode) where
  -- | inferrable terms are checkable
  Inf :: !(Term Infer) -> Term Check
  -- | lambda abstraction
  Lam :: !(Maybe CaseInsensitive) -> !(Term Check) -> Term Check
  -- | annotated terms @e: t@
  Ann :: !(Term Check) -> !(Term Infer) -> Term Infer
  -- | hierarchy of type universes
  Set :: !Int -> Term Infer
  -- | dependent function (Π)
  Pi :: !(Maybe CaseInsensitive) -> !(Term Check) -> !(Term Check) -> Term Infer
  -- | bound variable
  Bound :: !Int -> Term Infer
  -- | free variable
  Free :: !Name -> Term Infer
  -- | cell references
  Ref :: !(URI, Text) -> RefInfo -> CellRange -> Term Infer
  -- | literal types
  Lit :: !Lit -> Term Infer
  -- | literal terms
  LitOf :: !LitOf -> Term Infer
  -- | tensor type
  Tensor :: !TensorDescriptor -> Term Infer
  -- | tensor value (represented as a multi-dimensional array)
  TensorOf :: !TensorDescriptor -> !(Array (Term Check)) -> Term Infer
  -- | application
  (:$) :: !(Term Infer) -> !(Term Check) -> Term Infer

deriving instance Show (Term m)

instance Eq (Term m) where
  Inf x == Inf y = x == y
  Lam _ x == Lam _ y = x == y
  Ann x t == Ann y v = x == y && t == v
  Set m == Set n = n == m
  Pi _ dom range == Pi _ dom' range' = dom == dom' && range == range'
  Bound i == Bound j = i == j
  Free name == Free name' = name == name'
  Ref sheetId _ cr == Ref sheetId' _ cr' = sheetId == sheetId' && cr == cr'
  Lit lit == Lit lit' = lit == lit'
  LitOf val == LitOf val' = val == val'
  Tensor td == Tensor td' = td == td'
  TensorOf td arr == TensorOf td' arr' = td == td' && arr == arr'
  f :$ x == g :$ y = f == g && x == y
  _ == _ = False

boolOf :: Bool -> Term Infer
boolOf = LitOf . BoolOf

intOf :: Int -> Term Infer
intOf = LitOf . IntOf

lam :: CaseInsensitive -> Term Check -> Term Check
lam = Lam . Just

splitApp :: Term Infer -> (Term Infer, [Term Check])
splitApp = go []
 where
  go :: [Term Check] -> Term Infer -> (Term Infer, [Term Check])
  go acc (x :$ y) = go (y : acc) x
  go acc x = (x, acc)

-- ** Substitutions

subst :: Int -> Term Infer -> Term m -> Term m
subst i r = \case
  Inf e -> Inf (subst i r e)
  Lam n x -> Lam n (subst (i + 1) r x)
  Ann e t -> Ann (subst i r e) (subst i r t)
  Set k -> Set k
  Pi xn x y -> Pi xn (subst i r x) (subst (i + 1) r y)
  Bound j
    | i == j -> r
    | otherwise -> Bound j
  Free n -> Free n
  Ref sheetId refInfo cr -> Ref sheetId refInfo cr
  Lit lit -> Lit lit
  LitOf val -> LitOf val
  Tensor td -> Tensor (substTensor i r td)
  TensorOf td arr -> TensorOf (substTensor i r td) (subst i r <$> arr)
  x :$ y -> subst i r x :$ subst i r y

substTensor :: Int -> Term Infer -> TensorDescriptor -> TensorDescriptor
substTensor i r (TensorDescriptor dims) =
  TensorDescriptor (subst i r `map` dims)

instance Pretty CaseInsensitive where
  pretty = pretty . originalText

instance Pretty Name where
  pretty = \case
    Global n -> pretty n
    x -> "{" <> viaShow x <> "}"

instance Pretty TensorDescriptor where
  pretty (TensorDescriptor dims) =
    align
      $ encloseSep "⟨" "⟩" comma (map pretty dims)

instance Pretty Lit where
  pretty = \case
    Bool -> "bool"
    Int -> "int"

instance Pretty LitOf where
  pretty = \case
    BoolOf b -> pretty b
    IntOf i -> pretty i

braced :: [Doc ann] -> Doc ann
braced =
  group
    . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

showExcel26 :: CellAddr -> String
showExcel26 (r, c) = row <> show (r + 1)
 where
  row = concatMap sequence (tail $ iterate (['A' .. 'Z'] :) []) !! c

instance Pretty (Term m) where
  pretty = pr [] 0
   where
    pr :: forall m' ann. [CaseInsensitive] -> Int -> Term m' -> Doc ann
    pr env i = \case
      Inf e -> pr env i e
      Lam xn x ->
        let
          v = fromMaybe (genFresh env) xn
        in
          parensIf (i > lamP) ("\\" <> pretty v <+> "->" <+> pr (v : env) i x)
      Ann e t -> parensIf (i > annP) (pr env annP e <> ":" <+> pr env 0 t)
      Set k
        | k == 0 -> "*"
        | otherwise -> "Type" <> showSubscript k
      Pi xn a b ->
        let
          v = fromMaybe (genFresh env) xn
        in
          parensIf (i > funP)
            $ annotateArg xn (pr env (funP + 1) a) <+> "->" <+> pr (v : env) funP b
      Bound j -> pretty (env !! j)
      Free (Quote n) -> pretty (reverse env !! n)
      Free n -> pretty n
      Lit lit -> pretty lit
      LitOf val -> pretty val
      Ref (uri, sheetName) refInfo cr -> case refInfo of
        FullySpecified -> quoteSheetRef True (brackets (prettyURI uri) <> escapeUri sheetName) <> "!" <> prettyCellRef cr
        SheetOnly -> quoteSheetRef False (escapeUri sheetName) <> "!" <> prettyCellRef cr
        Unspecified -> prettyCellRef cr
       where
        quoteSheetRef showUri
          | (showUri && requireQuotes (prettyURI' uri))
              || requireQuotes sheetName =
              enclose "'" "'"
          | otherwise = id
      Tensor td -> pretty td
      TensorOf td arr -> pretty td <+> pretty (Array.toList arr)
      app@(:$){} ->
        let (y, ys) = splitApp app
        in  hang 2 $ pr env 0 y <> softline' <> align (tupled $ map (pr env 0) ys)

    prettyCellRef (start, end)
      | start == end = prettyCellAddr start
      | otherwise = prettyCellAddr start <> ":" <> prettyCellAddr end

    prettyCellAddr = pretty . Text.pack . showExcel26

    requireQuotes =
      Text.any
        (\c -> not (isAlphaNum c) && c `notElem` ("._~" :: String))

    parensIf b doc
      | b = "(" <> doc <> ")"
      | otherwise = doc

    annotateArg (Just n) x = "(" <> pretty n <> ":" <> x <> ")"
    annotateArg _ x = x

    showSubscript k = pretty (showIntAtBase 10 ("₀₁₂₃₄₅₆₇₈₉" !!) k "")

    lamP = 0 :: Int
    funP = 1 :: Int
    annP = 2 :: Int

prettyURI :: URI -> Doc ann
prettyURI = escapeUri . prettyURI'

prettyURI' :: URI -> Text
prettyURI' = stripPrefix "file://" . Text.pack . unEscapeString . show
 where
  stripPrefix prefix str
    | Just trimmed <- Text.stripPrefix prefix str = trimmed
    | otherwise = str

escapeUri :: Text -> Doc ann
escapeUri =
  pretty
    . Text.replace "'" "\\'"
    . Text.replace "[" "\\["
    . Text.replace "]" "\\]"
    . Text.replace "\\" "\\\\"

-- FIXME: the binders for which we generated a variable are not re-labelled!
genFresh :: [CaseInsensitive] -> CaseInsensitive
genFresh env =
  head . filter (`notElem` env)
    $ CaseInsensitive "x" : map (CaseInsensitive . ("x" <>) . Text.pack . show) [1 :: Int ..]

splitFun :: Term Infer -> ([Term Check], Term Check)
splitFun = go []
 where
  go acc (Pi Nothing x (Inf y)) = go (x : acc) y
  go acc y = (reverse acc, Inf y)

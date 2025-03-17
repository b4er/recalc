{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.URI
import Numeric
import Prettyprinter

import Recalc.Engine (CellAddr, CellRange)
import Recalc.Univer.Protocol (quotientOn)

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
  | -- | bound variables during quoting
    Quote !Int
  | -- | implicit variables inserted (unification variables)
    Implicit !Int
  deriving (Generic, Eq, Ord, Show)

instance IsString Name where
  fromString :: String -> Name
  fromString = Global . CaseInsensitive . Text.pack

-- | whether a binder is implicit or not
data Arg = EArg | IArg
  deriving (Eq, Ord, Show)

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
data TensorDescriptor = TensorDescriptor
  { base :: Term Infer
  , dims :: [Term Check]
  }
  deriving (Eq, Show)

data Term (m :: Mode) where
  -- | inferrable terms are checkable
  Inf :: !(Term Infer) -> Term Check
  -- | lambda abstraction
  Lam :: !Arg -> !(Maybe CaseInsensitive) -> !(Term Check) -> Term Check
  -- | annotated terms @e: t@
  Ann :: !(Term Check) -> !(Term Infer) -> Term Infer
  -- | hierarchy of type universes
  Set :: !Int -> Term Infer
  -- | dependent function (Π)
  Pi :: !Arg -> !(Maybe CaseInsensitive) -> !(Term Check) -> !(Term Check) -> Term Infer
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
  -- | tensor type (only for quoting)
  Tensor :: !TensorDescriptor -> Term Infer
  -- | tensor value (represented as a multi-dimensional array, only for quoting)
  TensorOf :: !TensorDescriptor -> !(Array (Term Check)) -> Term Infer
  -- | application
  App :: !(Term Infer) -> !(Arg, Term Check) -> Term Infer

pattern (:$) :: Term Infer -> Term Check -> Term Infer
pattern f :$ x = f `App` (EArg, x)

deriving instance Show (Term m)

instance Eq (Term m) where
  Inf x == Inf y = x == y
  Lam arg _ x == Lam arg' _ y = arg == arg' && x == y
  Ann x t == Ann y v = x == y && t == v
  Set m == Set n = n == m
  Pi arg _ dom range == Pi arg' _ dom' range' = arg == arg' && dom == dom' && range == range'
  Bound i == Bound j = i == j
  Free name == Free name' = name == name'
  Ref sheetId _ cr == Ref sheetId' _ cr' = sheetId == sheetId' && cr == cr'
  Lit lit == Lit lit' = lit == lit'
  LitOf val == LitOf val' = val == val'
  Tensor td == Tensor td' = td == td'
  TensorOf td arr == TensorOf td' arr' = td == td' && arr == arr'
  App f (EArg, x) == App g (EArg, y) = f == g && x == y
  App f (IArg, _x) == App g (IArg, _y) = f == g
  App{} == App{} = False
  _ == _ = False

boolOf :: Bool -> Term Infer
boolOf = LitOf . BoolOf

intOf :: Int -> Term Infer
intOf = LitOf . IntOf

lam :: Arg -> CaseInsensitive -> Term Check -> Term Check
lam arg = Lam arg . Just

splitApp :: Term Infer -> (Term Infer, [(Arg, Term Check)])
splitApp = go []
 where
  go :: [(Arg, Term Check)] -> Term Infer -> (Term Infer, [(Arg, Term Check)])
  go acc (App x y) = go (y : acc) x
  go acc x = (x, acc)

-- ** Substitutions

subst :: Int -> Term Infer -> Term m -> Term m
subst i r = \case
  Inf e -> Inf (subst i r e)
  Lam arg n x -> Lam arg n (subst (i + 1) r x)
  Ann e t -> Ann (subst i r e) (subst i r t)
  Set k -> Set k
  Pi arg xn x y -> Pi arg xn (subst i r x) (subst (i + 1) r y)
  Bound j
    | i == j -> r
    | otherwise -> Bound j
  Free n -> Free n
  Ref sheetId refInfo cr -> Ref sheetId refInfo cr
  Lit lit -> Lit lit
  LitOf val -> LitOf val
  Tensor td -> Tensor (substTensor i r td)
  TensorOf td arr -> TensorOf (substTensor i r td) (subst i r <$> arr)
  x `App` (arg, y) -> subst i r x `App` (arg, subst i r y)

substTensor :: Int -> Term Infer -> TensorDescriptor -> TensorDescriptor
substTensor i r (TensorDescriptor base dims) =
  TensorDescriptor (subst i r base) (map (subst i r) dims)

instance Pretty CaseInsensitive where
  pretty = pretty . originalText

instance Pretty Name where
  pretty = \case
    Global n -> pretty n
    x -> "{" <> viaShow x <> "}"

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
      Lam arg xn x ->
        let
          v = fromMaybe (genFresh env) xn
          rel = if arg == IArg then braces else id
        in
          parensIf (i > lamP) ("\\" <> rel (pretty v) <+> "->" <+> pr (v : env) i x)
      Ann e t -> parensIf (i > annP) (pr env annP e <> ":" <+> pr env 0 t)
      Set k
        | k == 0 -> "*"
        | otherwise -> "Type" <> showSubscript k
      Pi arg xn a b ->
        let
          v = fromMaybe (genFresh env) xn
          prec = if arg == IArg then 0 else funP + 1
        in
          parensIf (i > funP)
            $ annotateArg arg xn (pr env prec a) <+> "->" <+> pr (v : env) funP b
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
      Tensor td -> prTensor env td
      TensorOf _ arr -> list (map (pr env 0) (Array.toList arr))
      app@App{} ->
        let
          (y, ys) = splitApp app
        in
          hang 2
            $ pr env 0 y
              <> cat
                [ (softline' <>)
                  . (case arg of EArg -> tupled; IArg -> braced)
                  $ map (pr env 0 . snd) ys'
                | (arg, ys') <- quotientOn fst ys
                ]

    prTensor env (TensorDescriptor base dims) =
      align
        $ encloseSep "⟨" "⟩" comma (map (pr env 0) dims)
          <> "["
          <> pr env 0 base
          <> "]"

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

    annotateArg arg (Just n) x = open <> pretty n <> ":" <> x <> close
     where
      (open, close) = if arg == IArg then ("{", "}") else ("(", ")")
    annotateArg IArg _ x = "{" <> x <> "}"
    annotateArg _ _ x = x

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

-- substitute @Implicit k@ with @Bound 0@ and shift lift indices (since we
-- introduce a binder)
sub :: Int -> Int -> Term m -> Term m
sub k i = \case
  Inf e -> Inf (sub k i e)
  Lam arg n x -> Lam arg n (sub k (i + 1) x)
  Ann e t -> Ann (sub k i e) (sub k i t)
  Set k' -> Set k'
  Pi arg xn x y -> Pi arg xn (sub k i x) (sub k (i + 1) y)
  -- shift the variables by +1 (lift)
  Bound j
    | i <= j -> Bound (j + 1)
    | otherwise -> Bound j
  -- replace @Implicit k@ with newly bound variable
  Free (Implicit k') | k == k' -> Bound i
  Free n -> Free n
  Ref sheetId refInfo cr -> Ref sheetId refInfo cr
  Lit lit -> Lit lit
  LitOf val -> LitOf val
  Tensor td -> Tensor (subTensor k i td)
  TensorOf td arr -> TensorOf (subTensor k i td) (sub k i <$> arr)
  x `App` (arg, y) -> sub k i x `App` (arg, sub k i y)

subTensor :: Int -> Int -> TensorDescriptor -> TensorDescriptor
subTensor k i (TensorDescriptor base dims) =
  TensorDescriptor (sub k i base) (map (sub k i) dims)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Recalc.Syntax.Term
Description : Terms and Values.

Type definitions for the typed lambda calculus implemented in "Recalc.Language".
-}
module Recalc.Syntax.Term where

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
import Recalc.Syntax.Fixity
import Recalc.Univer.Protocol (quotientOn)

-- * Terms

-- | modes for typing: bi-directional type checking
data Mode
  = -- | checkable terms (and elaborated terms)
    Check
  | -- | inferrable terms
    Infer
  deriving (Eq, Show)

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

-- | unary operators
data Op1 = Negate | LogicalNegate
  deriving (Eq, Bounded, Enum, Show)

-- | binary operators
data Op = Mult | Plus | Minus
  deriving (Eq, Bounded, Enum, Show)

-- | overloaded operators
data PrimOp = PrimPlus | PrimMult | PrimMinus
  deriving (Eq, Show)

operators :: [[Fixity Op1 Op]]
operators =
  [ [Prefix "-" Negate, Prefix "~" LogicalNegate]
  , [Infixl "*" Mult]
  , [Infixl "+" Plus, Infixl "-" Minus]
  ]

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
  -- | empty product type
  Empty :: Term Infer
  -- | dependent product (Σv:t. _)
  Sigma :: !(Maybe CaseInsensitive) -> !(Term Check) -> !(Term Check) -> Term Infer
  -- | empty product value
  Nil :: Term Infer
  -- | dependent product intro
  Pair :: !(Maybe CaseInsensitive) -> !(Term Check) -> !(Term Check) -> Term Check
  -- | projection
  Proj :: !CaseInsensitive -> !(Term Infer) -> Term Infer
  -- | bound variable
  Bound :: !Int -> Term Infer
  -- | unary operators (prefix or postfix)
  Op1 :: Op1 -> Term Infer -> Term Infer
  -- | binary operators
  Op :: Op -> Term Infer -> Term Infer -> Term Infer
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
  TensorOf :: !TensorDescriptor -> ![Term Check] -> Term Infer
  -- | application
  App :: !(Term Infer) -> !(Arg, Term Check) -> Term Infer
  -- | elaborated terms (do not use before typechecking!)
  Elaborate :: !(Term Check) -> Term Infer
  -- | elaborated, monomorph prim-ops
  PrimOp :: PrimOp -> Term Infer

pattern (:$) :: Term Infer -> Term Check -> Term Infer
pattern f :$ x = f `App` (EArg, x)

-- | smart constructor (@x@ when passing @Elaborate (Inf x)@)
elaborate :: Term Check -> Term Infer
elaborate (Inf x) = x
elaborate f = Elaborate f

deriving instance Show (Term m)

instance Eq (Term m) where
  Inf x == Inf y = x == y
  Lam arg _ x == Lam arg' _ y = arg == arg' && x == y
  Ann x t == Ann y v = x == y && t == v
  Set m == Set n = n == m
  Pi arg _ dom range == Pi arg' _ dom' range' = arg == arg' && dom == dom' && range == range'
  Empty == Empty = True
  Sigma n x y == Sigma n' x' y' = n == n' && x == x' && y == y'
  Nil == Nil = True
  Pair n x y == Pair n' x' y' = n == n' && x == x' && y == y'
  Bound i == Bound j = i == j
  Op1 op1 x == Op1 op1' x' = op1 == op1' && x == x'
  Op op x y == Op op' x' y' = op == op' && x == x' && y == y'
  Free name == Free name' = name == name'
  Ref sheetId _ cr == Ref sheetId' _ cr' = sheetId == sheetId' && cr == cr'
  Lit lit == Lit lit' = lit == lit'
  LitOf val == LitOf val' = val == val'
  Tensor td == Tensor td' = td == td'
  TensorOf td arr == TensorOf td' arr' = td == td' && arr == arr'
  App f (EArg, x) == App g (EArg, y) = f == g && x == y
  App f (IArg, _x) == App g (IArg, _y) = f == g
  App{} == App{} = False
  Elaborate x == Elaborate y = x == y
  PrimOp prim == PrimOp prim' = prim == prim'
  _ == _ = False

boolOf :: Bool -> Term Infer
boolOf = LitOf . BoolOf

intOf :: Int -> Term Infer
intOf = LitOf . IntOf

lam :: Arg -> CaseInsensitive -> Term Check -> Term Check
lam arg = Lam arg . Just

record :: [(Text, Term Check)] -> Term Check
record = foldr (\(n, x) -> Pair (Just (CaseInsensitive n)) x) (Inf Nil)

splitApp :: Term Infer -> (Term Infer, [(Arg, Term Check)])
splitApp = go []
 where
  go :: [(Arg, Term Check)] -> Term Infer -> (Term Infer, [(Arg, Term Check)])
  go acc (App x y) = go (y : acc) x
  go acc x = (x, acc)

splitPair
  :: [(Maybe CaseInsensitive, Term Check)]
  -> Term Check
  -> ([(Maybe CaseInsensitive, Term Check)], Term Check)
splitPair acc (Pair n x y) = splitPair ((n, x) : acc) y
splitPair acc y = (reverse acc, y)

splitSigma :: Term Infer -> ([(Maybe CaseInsensitive, Term Check)], Term Infer)
splitSigma = go []
 where
  go
    :: [(Maybe CaseInsensitive, Term Check)]
    -> Term Infer
    -> ([(Maybe CaseInsensitive, Term Check)], Term Infer)
  go acc (Sigma n x (Inf y)) = go ((n, x) : acc) y
  go acc y = (reverse acc, y)

-- ** Substitutions

subst :: Int -> Term Infer -> Term m -> Term m
subst i r = \case
  Inf e -> Inf (subst i r e)
  Lam arg n x -> Lam arg n (subst (i + 1) r x)
  Ann e t -> Ann (subst i r e) (subst i r t)
  Set k -> Set k
  Pi arg xn x y -> Pi arg xn (subst i r x) (subst (i + 1) r y)
  Empty -> Empty
  Sigma n x y -> Sigma n (subst i r x) (subst (i + 1) r y)
  Nil -> Nil
  Pair n x y -> Pair n (subst i r x) (subst i r y)
  Proj l x -> Proj l (subst i r x)
  Bound j
    | i == j -> r
    | otherwise -> Bound j
  Op1 op1 x -> Op1 op1 (subst i r x)
  Op op x y -> Op op (subst i r x) (subst i r y)
  Free n -> Free n
  Ref sheetId refInfo cr -> Ref sheetId refInfo cr
  Lit lit -> Lit lit
  LitOf val -> LitOf val
  Tensor td -> Tensor (substTensor i r td)
  TensorOf td arr -> TensorOf (substTensor i r td) (subst i r <$> arr)
  x `App` (arg, y) -> subst i r x `App` (arg, subst i r y)
  Elaborate x -> Elaborate (subst i r x)
  PrimOp prim -> PrimOp prim

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

-- instance Pretty Op1 where
--  pretty = \case
--    Neg -> "-"
--
-- instance Pretty Op where
--  pretty = \case
--    Add -> "+"

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
      Empty -> "record {}"
      sigma@Sigma{} -> prettySigma env sigma
      Nil -> "{}"
      pair@Pair{} -> prettyPair env pair
      Proj l x -> pr env 0 x <> "." <> pretty l
      Bound j -> pretty (env !! j)
      Op1 op1 x -> ppOp1 (pr env) op1 x
      Op op x y@(Op op' _ _)
        | op == op', right op -> ppRightOp2 (pr env) op x y
      Op op x y
        | left op -> ppLeftOp2 (pr env) op x y
        | otherwise -> ppOp2 (pr env) op x y
      Free (Quote n) -> pretty (env !! n) -- not necessary?
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
      TensorOf _ xs -> list (map (pr env 0) xs)
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
      Elaborate x -> pretty x
      PrimOp prim -> "#" <> viaShow prim
     where
      (left, right, ppOp1, ppRightOp2, ppLeftOp2, ppOp2) =
        ppHelpers
          @Op1
          @Op
          @(Term Infer)
          @ann
          @(Term Infer)
          @(Term Infer)
          @(Term Infer)
          operators
          i

    prettySigma env sigma = case splitSigma sigma of
      (fs, Empty) -> align (braced $ map prettyField fs)
      (fs, t) -> align (braced $ map prettyField (fs ++ [(Nothing, Inf t)]))
     where
      prettyField (f, t) = pretty f <> ":" <+> pr env 0 t

    prettyPair env pair = case splitPair [] pair of
      (fs, Inf Nil) -> align (braced $ map prettyField fs)
      (fs, t) -> align (braced $ map prettyField (fs ++ [(Nothing, t)]))
     where
      prettyField (f, t) = pretty f <+> "=" <+> pr env 0 t

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

    -- auxiliary functions

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
  Empty -> Empty
  Sigma n x y -> Sigma n (sub k i x) (sub k (i + 1) y)
  Nil -> Nil
  Pair n x y -> Pair n (sub k i x) (sub k i y)
  Proj l x -> Proj l (sub k i x)
  -- shift the variables by +1 (lift)
  Bound j
    | i <= j -> Bound (j + 1)
    | otherwise -> Bound j
  Op1 op1 x -> Op1 op1 (sub k i x)
  Op op1 x y -> Op op1 (sub k i x) (sub k i y)
  -- replace @Implicit k@ with newly bound variable
  Free (Implicit k') | k == k' -> Bound i
  Free n -> Free n
  Ref sheetId refInfo cr -> Ref sheetId refInfo cr
  Lit lit -> Lit lit
  LitOf val -> LitOf val
  Tensor td -> Tensor (subTensor k i td)
  TensorOf td arr -> TensorOf (subTensor k i td) (sub k i <$> arr)
  x `App` (arg, y) -> sub k i x `App` (arg, sub k i y)
  Elaborate x -> Elaborate (sub k i x)
  PrimOp prim -> PrimOp prim

subTensor :: Int -> Int -> TensorDescriptor -> TensorDescriptor
subTensor k i (TensorDescriptor base dims) =
  TensorDescriptor (sub k i base) (map (sub k i) dims)

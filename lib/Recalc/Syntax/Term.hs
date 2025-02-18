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

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text, pack, toLower)
import GHC.Generics (Generic)
import Numeric
import Prettyprinter

-- * Terms

data Mode = Check | Infer deriving (Eq, Show)

-- ** Names

newtype CaseInsensitive = CaseInsensitive {originalText :: Text}

caseInsensitive :: CaseInsensitive -> Text
caseInsensitive = toLower . originalText

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
  fromString = Global . CaseInsensitive . pack

pat :: Text -> Maybe CaseInsensitive
pat = Just . CaseInsensitive

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
  f :$ x == g :$ y = f == g && x == y
  _ == _ = False

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
  x :$ y -> subst i r x :$ subst i r y

-- * Evaluation

-- ** Normal Form

data Neutral
  = NFree !Name
  | NApp !Neutral !Value
  deriving (Generic)

data Value
  = VLam !(Maybe CaseInsensitive) !(Value -> Value)
  | VSet !Int
  | VPi !(Maybe CaseInsensitive) !Value !(Value -> Value)
  | VNeutral !Neutral
  deriving (Generic)

instance Eq Value where
  (==) = (==) `on` quote

vfree :: Name -> Value
vfree = VNeutral . NFree

infixr 9 `vfun`

vfun :: Value -> Value -> Value
vfun dom range = VPi Nothing dom (const range)

vapp :: Value -> Value -> Value
vapp (VLam _n f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
vapp a b = error ("vapp: " ++ show (a, b))

instance Show Value where show = show . pretty

-- ** Quoting

quote :: Value -> Term Check
quote = go 0
 where
  go i = \case
    VLam n f -> Lam n $ go (i + 1) $ f (vfree (Quote i))
    VSet k -> Inf (Set k)
    VPi n v f -> Inf $ Pi n (go i v) (go (i + 1) (f (vfree (Quote i))))
    VNeutral n -> Inf (goNeutral i n)

  goNeutral i = \case
    NFree n -> Free n
    NApp n v -> goNeutral i n :$ go i v

instance Pretty CaseInsensitive where
  pretty = pretty . originalText

instance Pretty Name where
  pretty = \case
    Global n -> pretty n
    x -> "{" <> viaShow x <> "}"

braced :: [Doc ann] -> Doc ann
braced =
  group
    . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

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
      app@(:$){} ->
        let (y, ys) = splitApp app
        in  hang 2 $ pr env 0 y <> softline' <> align (tupled $ map (pr env 0) ys)

    parensIf b doc
      | b = "(" <> doc <> ")"
      | otherwise = doc

    annotateArg (Just n) x = "(" <> pretty n <> ":" <> x <> ")"
    annotateArg _ x = x

    showSubscript k = pretty (showIntAtBase 10 ("₀₁₂₃₄₅₆₇₈₉" !!) k "")

    lamP = 0 :: Int
    funP = 1 :: Int
    annP = 2 :: Int

genFresh :: [CaseInsensitive] -> CaseInsensitive
genFresh env =
  head . filter (`notElem` env)
    $ CaseInsensitive "x" : map (CaseInsensitive . ("x" <>) . pack . show) [1 :: Int ..]

splitFun :: Term Infer -> ([Term Check], Term Check)
splitFun = go []
 where
  go acc (Pi Nothing x (Inf y)) = go (x : acc) y
  go acc y = (reverse acc, Inf y)

instance Pretty Value where
  pretty = pretty . quote

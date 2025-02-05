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

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text, pack)
import Numeric
import Prettyprinter

-- * Terms

data Mode = Check | Infer deriving (Eq, Show)

-- ** Names

data Name
  = Global !Text
  | -- | when passing binder, bound names are converted to locally free ones
    Local !(Maybe Text) !Int
  | -- | same but during quoting
    Quote !Int
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString :: String -> Name
  fromString = Global . pack

data Term (m :: Mode) where
    -- | inferrable terms are checkable
    Inf :: !(Term Infer) -> Term Check
    -- | lambda abstraction
    Lam :: !(Maybe Text) -> !(Term Check) -> Term Check
    -- | annotated terms @e: t@
    Ann :: !(Term Check) -> !(Term Infer) -> Term Infer
    -- | hierarchy of type universes
    Univ :: !Int -> Term Infer
    -- | dependent function (Π)
    Pi :: !(Maybe Text) -> !(Term Check) -> !(Term Check) -> Term Infer
    -- | bound variable
    Bound :: !Int -> Term Infer
    -- | free variable
    Free :: !Name -> Term Infer
    -- | application
    (:$) :: !(Term Infer) -> !(Term Check) -> Term Infer

deriving instance Eq (Term m)
deriving instance Show (Term m)

lam :: Text -> Term Check -> Term Check
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
  Lam n x -> Lam n (subst (i+1) r x)
  Ann e t -> Ann (subst i r e) t
  Univ k -> Univ k
  Pi xn x y -> Pi xn (subst i r x) (subst (i+1) r y)
  Bound j
    | i == j    -> r
    | otherwise -> Bound j
  Free n -> Free n
  x :$ y -> subst i r x :$ subst i r y


-- * Evaluation

-- ** Normal Form

data Neutral
  = NFree !Name
  | NApp !Neutral !Value

data Value
  = VLam !(Maybe Text) !(Value -> Value)
  | VSet !Int
  | VPi !(Maybe Text) !Value !(Value -> Value)
  | VNeutral !Neutral

vfree :: Name -> Value
vfree = VNeutral . NFree

vapp :: Value -> Value -> Value
vapp (VLam _n f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
vapp a b = error ("vapp: " ++ show (a,b))

instance Show Value where show = show . pretty

-- ** Quoting

quote :: Value -> Term Check
quote = go 0
  where
    go i = \case
      VLam n f -> Lam n $ go (i + 1) $ f (vfree (Quote i))
      VSet k -> Inf (Univ k)
      VPi n v f -> Inf $ Pi n (go i v) (go (i+1) (f (vfree (Quote i))))
      VNeutral n -> Inf (goNeutral i n)

    goNeutral i = \case
      NFree n -> Free n
      NApp n v -> goNeutral i n :$ go i v

instance Pretty Name where
  pretty = \case
    Global n -> pretty n
    x -> "{" <> viaShow x <> "}"

braced :: [Doc ann] -> Doc ann
braced = group
  . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

instance Pretty (Term m) where
  pretty = pr [] 0 where

    pr :: forall m' ann. [Text] -> Int -> Term m' -> Doc ann
    pr env i = \case
        Inf e -> pr env i e
        Lam xn x -> parensIf (i > lamP) ("\\" <> pretty xn <+> "->" <+> pr (fromMaybe "_" xn : env) i x)
        Ann e t -> parensIf (i > annP) (pr env annP e <> ":" <+> pr env 0 t)
        Univ k
          | k == 0    -> "*"
          | otherwise -> "Type" <> showSubscript k
        Pi xn a b -> parensIf (i > funP) $
          annotateArg xn (pr env (funP+1) a) <+> "->" <+> pr (fromMaybe "_" xn : env) funP b
        Bound j -> pretty (env !! j)
        Free (Quote n) -> pretty (reverse env !! n)
        Free n -> pretty n
        app@(:$){} ->
          let (y, ys) = splitApp app
          in hang 2 $ pr env 0 y <> softline' <> align (tupled $ map (pr env 0) ys)

    parensIf b doc
      | b         = "("<> doc <>")"
      | otherwise = doc

    annotateArg (Just n) x = "(" <> pretty n <> ":"<> x <> ")"
    annotateArg _ x = x

    showSubscript k = pretty (showIntAtBase 10 ("₀₁₂₃₄₅₆₇₈₉" !!) k "")

    lamP = 0::Int
    funP = 1::Int
    annP = 2::Int

splitFun :: Term Infer -> ([Term Check], Term Check)
splitFun = go []
  where
    go acc (Pi Nothing x (Inf y)) = go (x : acc) y
    go acc y = (reverse acc, Inf y)

instance Pretty Value where
  pretty = pretty . quote

prelude :: [(Name, (Value, Maybe Value))]
prelude = []

builtIns :: Map Name Value
builtIns = Map.fromList [ (n, v) | (n, (_, Just v)) <- prelude ]

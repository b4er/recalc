{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Recalc.Syntax.Unify
Description : A simple unification algorithm for terms.
-}
module Recalc.Syntax.Unify
  ( -- ** Substitutions
    Subst
  , apply

    -- ** Unification
  , unify

    -- ** Errors
  , UnificationError (..)
  , unificationErrorAnnotation
  ) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Array.Dynamic qualified as Array
import Data.Function (on)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Recalc.Syntax.Term
import Recalc.Univer (Annotation (Annotation))

data UnificationError
  = UnifyMismatchI (Term Infer, Term Infer)
  | UnifyMismatchC (Term Check, Term Check)
  | OccursCheck Int (Term Infer)
  deriving (Eq, Show)

type Subst = Map Int (Term Infer)

-- | unify two terms
unify :: MonadError UnificationError f => (Term Check, Term Check) -> f Subst
unify = (`unifyC` mempty)

-- | checkable term unification
unifyC :: MonadError UnificationError f => (Term Check, Term Check) -> Subst -> f Subst
unifyC (lhs, rhs) s = case (apply s lhs, apply s rhs) of
  (Inf x, Inf y) -> unifyI (x, y) s
  (Lam eps _pat body, Lam eps' _pat' body')
    | eps == eps' -> unifyC (body, body') s
  eq -> throwError (UnifyMismatchC eq)

-- | simple structural unification for inferrable terms
unifyI :: MonadError UnificationError f => (Term Infer, Term Infer) -> Subst -> f Subst
unifyI (lhs, rhs) s = case (apply s lhs, apply s rhs) of
  (Ann x ty, Ann x' ty') -> unifyC (x, x') =<< unifyI (ty, ty') s
  (Pi eps _n dom range, Pi eps' _n' dom' range')
    | eps == eps' -> unifyC (range, range') =<< unifyC (dom, dom') s
  (Bound i, Bound j) | i == j -> pure s
  (Ref ref _ cr, Ref ref' _ cr') | ref == ref', cr == cr' -> pure s
  (Tensor td, Tensor td') -> unifyTensor (td, td') s
  (TensorOf td arr, TensorOf td' arr')
    | ((==) `on` Array.shapeL) arr arr' ->
        Array.foldrA ((=<<) . unifyC) (unifyTensor (td, td') s)
          $ Array.zipWithA (,) arr arr'
  (f `App` (eps, x), g `App` (eps', y))
    | eps == eps' -> do
        unifyC (x, y) =<< unifyI (f, g) s
  (Free (Implicit k), ty) -> unifyImplicit k ty s
  (ty, var@(Free Implicit{})) -> unifyI (var, ty) s
  (Free n, Free n') | n == n' -> pure s
  (x, y)
    | x == y -> pure s
    | otherwise -> throwError (UnifyMismatchI (x, y))

unifyTensor
  :: MonadError UnificationError f => (TensorDescriptor, TensorDescriptor) -> Subst -> f Subst
unifyTensor (TensorDescriptor base dims, TensorDescriptor base' dims') s =
  foldr ((=<<) . unifyC) (unifyI (base, base') s) (zip dims dims')

unifyImplicit :: MonadError UnificationError f => Int -> Term Infer -> Subst -> f Subst
unifyImplicit k ty s
  | Free (Implicit k) == ty = pure s
  | k `elem` implicitVars ty = throwError (OccursCheck k ty)
  | otherwise =
      -- insert new unification var into substitution
      pure . Map.insert k ty
        -- update substitution with new mapping
        $ Map.map (apply (Map.fromList [(k, ty)])) s

implicitVars :: Term Infer -> Set Int
implicitVars = go mempty
 where
  go :: Set Int -> Term m -> Set Int
  go vars = \case
    Inf e -> go vars e
    Lam _ _ x -> go vars x
    Ann e t -> go (go vars e) t
    Pi _ _ x y -> go (go vars x) y
    Tensor td -> goTensor vars td
    TensorOf td arr -> foldl' go (goTensor vars td) arr
    f `App` (_, y) -> go (go vars f) y
    Free (Implicit k) -> Set.insert k vars
    Elaborate x -> go vars x -- should not be used
    _ -> vars

  goTensor vars (TensorDescriptor base dims) =
    foldl' go (go vars base) dims

apply :: Subst -> Term m -> Term m
apply = (`go` 0)
 where
  go :: Subst -> Int -> Term m -> Term m
  go s i = \case
    Inf e -> Inf (go s i e)
    Lam eps n x -> Lam eps n (go s (i + 1) x)
    Ann e t -> Ann (go s i e) (go s i t)
    Pi eps xn x y -> Pi eps xn (go s i x) (go s (i + 1) y)
    Tensor td -> Tensor (goTensor s i td)
    TensorOf td arr -> TensorOf (goTensor s i td) (go s i <$> arr)
    x `App` (eps, y) -> go s i x `App` (eps, go s i y)
    Elaborate x -> Elaborate (go s i x)
    Free (Implicit k)
      | Just x <- Map.lookup k s ->
          x
    -- no-ops
    Free n -> Free n
    Set k -> Set k
    Bound j -> Bound j
    Ref sheetId refInfo cr -> Ref sheetId refInfo cr
    Lit lit -> Lit lit
    LitOf val -> LitOf val

  goTensor s i (TensorDescriptor base dims) =
    TensorDescriptor (go s i base) (map (go s i) dims)

-- | render unification errors
unificationErrorAnnotation :: UnificationError -> Annotation
unificationErrorAnnotation = \case
  UnifyMismatchI tt -> mismatch tt
  UnifyMismatchC tt -> mismatch tt
  OccursCheck k ty ->
    ann
      "Unification Error"
      ["Implicit variable ‘", pretty k, "’ occurs in ‘", pretty ty, "’"]
 where
  mismatch :: (Term m, Term m) -> Annotation
  mismatch (t, t') =
    ann
      "Unification Error"
      ["Cannot match type ‘", pretty t, "’ with ‘", pretty t', "’"]

ann :: Text -> [Doc ann] -> Annotation
ann title = Annotation title . renderStrict . layoutPretty defaultLayoutOptions . cat

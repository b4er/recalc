{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.Syntax.Term
Description : "Language" interface for a dependently typed language

Implementation of a very simple calculus based on
"A tutorial implementation of a dependently typed lambda calculus".

This implementation implements the type level hierarchy soundly. It
adds type assumptions and type/value assumptions for Boolean values
and a few declarations to interact with them.
-}
module Recalc.Semantics where

import Control.Monad (void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Prettyprinter

import Control.Monad.Error.Class (MonadError (throwError))
import Recalc.Engine (Fetch, FetchError (..), Language (..), SheetId, getEnv, localEnv)
import Recalc.Syntax.Term

data Decl = Decl
  { declType :: Value
  , declValue :: Maybe Value
  }

type Globals = Map Name Decl

data Env = Env
  { sheetId :: SheetId
  , globals :: Globals
  }

-- | treating Boolean values as globals
prelude :: Globals
prelude =
  Map.fromList
    [ ("Bool", Decl (VSet 0) Nothing)
    , ("False", Decl (vfree "Bool") Nothing)
    , ("True", Decl (vfree "Bool") Nothing)
    , ("not", Decl (vfree "Bool" `vfun` vfree "Bool") (Just notImpl))
    , ("and", Decl (vfree "Bool" `vfun` vfree "Bool" `vfun` vfree "Bool") (Just andImpl))
    , ("or", Decl (vfree "Bool" `vfun` vfree "Bool" `vfun` vfree "Bool") (Just orImpl))
    ]
 where
  notImpl =
    VLam (Just (CaseInsensitive "x"))
      $ VNeutral . \case
        VNeutral (NFree "False") -> NFree "True"
        VNeutral (NFree "True") -> NFree "False"
        x -> NApp (NFree "not") x

  andImpl = VLam (Just (CaseInsensitive "x")) $ \x ->
    VLam (Just (CaseInsensitive "y")) $ \y ->
      VNeutral $ case (x, y) of
        (VNeutral (NFree "False"), VNeutral (NFree "False")) -> NFree "False"
        (VNeutral (NFree "True"), VNeutral (NFree "False")) -> NFree "False"
        (VNeutral (NFree "False"), VNeutral (NFree "True")) -> NFree "False"
        (VNeutral (NFree "True"), VNeutral (NFree "True")) -> NFree "True"
        _ -> NApp (NApp (NFree "and") x) y

  orImpl = VLam (Just (CaseInsensitive "x")) $ \x ->
    VLam (Just (CaseInsensitive "y")) $ \y ->
      VNeutral $ case (x, y) of
        (VNeutral (NFree "False"), VNeutral (NFree "False")) -> NFree "False"
        (VNeutral (NFree "True"), VNeutral (NFree "False")) -> NFree "True"
        (VNeutral (NFree "False"), VNeutral (NFree "True")) -> NFree "True"
        (VNeutral (NFree "True"), VNeutral (NFree "True")) -> NFree "True"
        _ -> NApp (NApp (NFree "or") x) y

-- | values are types
type Type = Value

data SemanticError
  = TypeMismatch Type Type
  | TypeMismatchPi (Term Check) Type
  | TypeOfBound
  | ExpectedSet (Term Check)
  | IllegalApplication Type
  | UnknownIdentifier Name
  deriving (Show)

throwSemanticError :: SemanticError -> Fetch (Term Infer) a
throwSemanticError err = throwError (OtherError err)

bindType :: Name -> Value -> Fetch (Term Infer) a -> Fetch (Term Infer) a
bindType name ty = localEnv (\env -> env{globals = Map.insert name (Decl ty Nothing) (globals env)})

{- Evaluation -}

eval' :: Term m -> Fetch (Term Infer) Value
eval' term = do
  Env{globals} <- getEnv
  let
    go :: [Value] -> Term m -> Value
    go valueEnv = \case
      Inf x -> go valueEnv x
      Lam pat body -> VLam pat (\x -> go (x : valueEnv) body)
      Ann x _ty -> go valueEnv x
      Set k -> VSet k
      Pi pat dom range ->
        let dom' = go valueEnv dom
        in  VPi pat dom' (\v -> go (v : valueEnv) range)
      Bound i -> valueEnv !! i
      Free name
        | Just Decl{declValue = Just v} <- Map.lookup name globals -> v
        | otherwise -> vfree name
      f :$ x -> vapp (go valueEnv f) (go valueEnv x)

  pure (go [] term)

{- Bi-directional Type Checking -}

check' :: Int -> Type -> Term Check -> Fetch (Term Infer) ()
check' i ty = \case
  Inf x -> do
    ty' <- infer' i x
    when (quote ty' /= quote ty)
      $ throwSemanticError (TypeMismatch ty' ty)
  x@(Lam _ body) -> case ty of
    VPi pat t t' ->
      bindType (Local pat i) t
        $ check' (i + 1) (t' (vfree (Local pat i)))
        $ subst 0 (Free (Local pat i)) body
    ty' -> throwSemanticError (TypeMismatchPi x ty')

infer' :: Int -> Term Infer -> Fetch (Term Infer) Type
infer' i = \case
  Ann e t -> do
    -- check' i ctxt (VSet 0)
    void $ inferUniverse' i (Inf t)
    t' <- eval t
    t' <$ check' i t' e
  Set k -> pure $ VSet (k + 1)
  Pi pat dom range -> do
    m <- inferUniverse' i dom
    dom' <- eval' dom
    n <-
      bindType (Local pat i) dom'
        $ inferUniverse' (i + 1)
        $ subst 0 (Free (Local pat i)) range
    pure $ VSet (max m n)
  Bound _i -> throwSemanticError TypeOfBound
  Free name -> do
    Env{globals} <- getEnv
    case Map.lookup name globals of
      Just Decl{declType} -> pure declType
      Nothing -> throwSemanticError (UnknownIdentifier name)
  f :$ x ->
    infer' i f >>= \case
      VPi _ t t' -> do
        check' i t x
        t' <$> eval' x
      t -> throwSemanticError (IllegalApplication t)

inferUniverse' :: Int -> Term Check -> Fetch (Term Infer) Int
inferUniverse' i = \case
  Inf e -> do
    t <- infer' i e
    case quote t of
      Inf (Set k) -> pure k
      Inf (Free name) -> do
        Env{globals} <- getEnv
        case Map.lookup name globals of
          Just Decl{declType = VSet k} -> pure k
          _ -> throwSemanticError (UnknownIdentifier name)
      ty -> throwSemanticError (ExpectedSet ty)
  ty -> throwSemanticError (ExpectedSet ty)

-- | interface implementation
instance Language (Term Infer) where
  deps _ = go mempty
   where
    go acc = acc -- collect all dependencies (currenty none)
    -- Inf x -> go acc x
    -- Lam _ body -> go acc body
    -- Ann x ty -> go (go acc x) ty
    -- Set{} -> acc
    -- Pi _ ty1 ty2 -> go (go acc ty1) ty2
    -- Bound{} -> acc
    -- Free{} -> acc
    -- f :$ x -> go (go acc f) x

  type EnvOf (Term Infer) = Env
  newEnv = (`Env` prelude)

  type ErrorOf (Term Infer) = SemanticError

  type ValueOf (Term Infer) = Value

  infer :: Term Infer -> Fetch (Term Infer) Type
  infer = infer' 0

  -- only called with values input by the user (cf. Parser)
  inferValue Env{globals} = \case
    VNeutral (NFree name) -> declType <$> Map.lookup name globals
    _ -> Nothing

  eval :: Term Infer -> Fetch (Term Infer) Value
  eval = eval'

{- Errors -}

semanticErrorTitle :: SemanticError -> Text
semanticErrorTitle = \case
  TypeMismatch{} -> "Type Mismatch"
  TypeMismatchPi{} -> "Type Mismatch"
  TypeOfBound{} -> "Error"
  ExpectedSet{} -> "Illegal Kind"
  IllegalApplication{} -> "Illegal Application"
  UnknownIdentifier{} -> "Uknown Identifier"

instance Pretty SemanticError where
  pretty =
    cat . \case
      TypeMismatch ty1 ty2 ->
        [ "Type mismatch, cannot match ‘"
        , pretty ty1
        , "‘ with ‘"
        , pretty ty2
        , "‘."
        ]
      TypeMismatchPi x ty ->
        [ "Lambda term ‘"
        , pretty x
        , "‘ cannot be matched with ‘"
        , pretty ty
        , "‘, expected Π-type."
        ]
      TypeOfBound -> ["Type of Bound (should not happen)."]
      ExpectedSet x ->
        [ "Expected term of type *, got ‘"
        , pretty x
        , "‘."
        ]
      IllegalApplication ty ->
        [ "Cannot apply a term to value with type ‘"
        , pretty ty
        , "’."
        ]
      UnknownIdentifier name ->
        [ "Unknown identifier ‘"
        , pretty name
        , "’."
        ]

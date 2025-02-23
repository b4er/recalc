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

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Array.Dynamic (Array)
import Data.Array.Dynamic qualified as Array
import Data.Function (on)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prettyprinter

import Recalc.Engine
  ( Fetch
  , FetchError (..)
  , Ix (Cell)
  , Language (..)
  , fetchType
  , fetchValue
  , getEnv
  , localEnv
  )
import Recalc.Engine.Core
import Recalc.Syntax.Parser (Parser, decimal, keyword)
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

-- | values are types
type Type = Value

instance Eq Value where (==) = (==) `on` quote

data SemanticError
  = TypeMismatch Type Type
  | TypeMismatchPi (Term Check) Type
  | TypeOfBound
  | ExpectedSet (Term Check)
  | IllegalApplication Type
  | UnknownIdentifier Name
  | UnknownError Text
  deriving (Eq, Show)

throwSemanticError :: MonadError (FetchError SemanticError) m => SemanticError -> m a
throwSemanticError err = throwError (OtherError err)

bindType :: Name -> Value -> Fetch (Term Infer) a -> Fetch (Term Infer) a
bindType name ty = localEnv (\env -> env{globals = Map.insert name (Decl ty Nothing) (globals env)})

{- Dependencies -}

deps' :: Term m -> Set CellRange
deps' = go mempty
 where
  go :: Set CellRange -> Term m -> Set CellRange
  go acc = \case
    Inf x -> go acc x
    Lam _ body -> go acc body
    Ann x ty -> go (go acc x) ty
    Set{} -> acc
    Pi _ ty1 ty2 -> go (go acc ty1) ty2
    Bound{} -> acc
    Free{} -> acc
    Ref _sheetId _ cr -> Set.singleton cr
    Lit{} -> acc
    LitOf{} -> acc
    Tensor td -> goTensor acc td
    TensorOf td arr -> Array.foldrA (flip go) (goTensor acc td) arr
    f :$ x -> go (go acc f) x

  goTensor arr (TensorDescriptor dims) = foldl' go arr dims

{- Evaluation -}

eval' :: Term m -> Fetch (Term Infer) Value
eval' term = do
  Env{globals} <- getEnv
  let
    go :: [Value] -> Term m -> Fetch (Term Infer) Value
    go valueEnv = \case
      Inf x -> go valueEnv x
      Lam p body -> pure $ VLam p (\x -> go (x : valueEnv) body)
      Ann x _ty -> go valueEnv x
      Set k -> pure (VSet k)
      Pi p dom range -> do
        dom' <- go valueEnv dom
        pure $ VPi p dom' (\v -> go (v : valueEnv) range)
      Bound i -> pure (valueEnv !! i)
      Ref sheetId _ (start, end)
        | start == end -> fetchValue sheetId start
        | otherwise -> fetchValue sheetId start -- FIXME
      Free name
        | Just Decl{declValue = Just v} <- Map.lookup name globals -> pure v
        | otherwise -> pure (vfree name)
      Lit lit -> pure (VLit lit)
      LitOf val -> pure (VLitOf val)
      Tensor (TensorDescriptor dims) ->
        VTensor . VTensorDescriptor <$> mapM (\x -> go valueEnv x) dims
      TensorOf td arr -> error (show (td, arr))
      f :$ x -> do
        f' <- go valueEnv f
        x' <- go valueEnv x
        vapp f' x'

  go [] term

{- Bi-directional Type Checking -}

check' :: Int -> Type -> Term Check -> Fetch (Term Infer) ()
check' i ty = \case
  Inf x -> do
    ty' <- infer' i x
    when (quote ty' /= quote ty)
      $ throwSemanticError (TypeMismatch ty' ty)
  x@(Lam _ body) -> case ty of
    VPi p t t' -> do
      t'' <- t' (vfree (Local p i))
      bindType (Local p i) t
        $ check' (i + 1) t''
        $ subst 0 (Free (Local p i)) body
    ty' -> throwSemanticError (TypeMismatchPi x ty')

infer' :: Int -> Term Infer -> Fetch (Term Infer) Type
infer' i = \case
  Ann e t -> do
    void $ inferUniverse' i (Inf t)
    t' <- eval t
    t' <$ check' i t' e
  Set k -> pure $ VSet (k + 1)
  Pi p dom range -> do
    m <- inferUniverse' i dom
    dom' <- eval' dom
    n <-
      bindType (Local p i) dom'
        $ inferUniverse' (i + 1)
        $ subst 0 (Free (Local p i)) range
    pure $ VSet (max m n)
  Bound _i -> throwSemanticError TypeOfBound
  Free name -> do
    Env{globals} <- getEnv
    case Map.lookup name globals of
      Just Decl{declType} -> pure declType
      Nothing -> throwSemanticError (UnknownIdentifier name)
  Ref sheetId _ (start, end)
    | start == end -> fetchType sheetId start
    | otherwise -> fetchType sheetId start -- FIXME
  Lit{} -> pure (VSet 0)
  LitOf val -> pure (inferLiteral val)
  Tensor (TensorDescriptor dims) -> do
    mapM_ (\x -> check' i vint x) dims
    pure (VSet 0)
  TensorOf (TensorDescriptor dims) arr -> do
    mapM_ (\x -> check' i vint x) dims
    mapM_ (\x -> check' i vint x) arr
    VTensor . VTensorDescriptor <$> mapM (\x -> eval' x) dims
  f :$ x ->
    infer' i f >>= \case
      VPi _ t t' -> do
        check' i t x
        x' <- eval' x
        t' x'
      t -> throwSemanticError (IllegalApplication t)

inferLiteral :: LitOf -> Type
inferLiteral =
  VLit . \case
    BoolOf{} -> Bool
    IntOf{} -> Int

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
      Inf Tensor{} -> pure 0
      ty -> throwSemanticError (ExpectedSet ty)
  ty -> throwSemanticError (ExpectedSet ty)

-- | interface implementation
instance Language (Term Infer) where
  deps = deps'

  type EnvOf (Term Infer) = Env
  newEnv = (`Env` prelude)

  type ErrorOf (Term Infer) = SemanticError

  type ValueOf (Term Infer) = Value

  infer :: Term Infer -> Fetch (Term Infer) Type
  infer = infer' 0

  -- only called with values input by the user (cf. Parser)
  inferValue Env{globals} = \case
    VLit{} -> Just (VSet 0)
    VLitOf val -> Just (inferLiteral val)
    VNeutral (NFree name) -> declType <$> Map.lookup name globals
    _ -> Nothing

  eval :: Term Infer -> Fetch (Term Infer) Value
  eval = eval'

{- Values -}

-- ** Normal Forms

data Neutral
  = NFree !Name
  | NRef !SheetId !CellRange
  | NApp !Neutral !Value
  deriving (Generic)

newtype VTensorDescriptor = VTensorDescriptor [Value]

data Value
  = VLam !(Maybe CaseInsensitive) !(Value -> Fetch (Term Infer) Value)
  | VSet !Int
  | VPi !(Maybe CaseInsensitive) !Value !(Value -> Fetch (Term Infer) Value)
  | VLit Lit
  | VLitOf LitOf
  | VTensor VTensorDescriptor
  | VTensorOf VTensorDescriptor (Array Value)
  | VNeutral !Neutral

vfree :: Name -> Value
vfree = VNeutral . NFree

infixr 9 `vfun`

vfun :: Value -> Value -> Value
vfun dom range = VPi Nothing dom (const $ pure range)

vapp :: Value -> Value -> Fetch (Term Infer) Value
vapp (VLam _n f) v = f v
vapp (VNeutral n) v = pure (VNeutral (NApp n v))
vapp f _ = throwSemanticError (IllegalApplication f)

vlam :: Maybe CaseInsensitive -> (Value -> Value) -> Value
vlam vname body = VLam vname $ \x -> pure (body x)

vpi :: Maybe CaseInsensitive -> Value -> (Value -> Value) -> Value
vpi vname dom range = VPi vname dom $ \x -> pure (range x)

vbool, vint :: Type
vbool = VLit Bool
vint = VLit Int

vboolOf :: Bool -> Value
vboolOf = VLitOf . BoolOf

instance Show Value where show = show . pretty

-- ** Quoting

quote :: Value -> Term Check
quote = go 0
 where
  go i = \case
    VLam vname body ->
      let
        body' = quoteAbs $ body (vfree (Quote i))
      in
        Lam vname (go (i + 1) body')
    VSet k -> Inf (Set k)
    VPi vname dom range ->
      let
        range' = quoteAbs $ range (vfree (Quote i))
      in
        Inf $ Pi vname (go i dom) (go (i + 1) range')
    VLit lit -> Inf (Lit lit)
    VLitOf val -> Inf (LitOf val)
    VTensor td -> Inf (Tensor (goTensor i td))
    VTensorOf td arr -> Inf (TensorOf (goTensor i td) (go i <$> arr))
    VNeutral n -> Inf (goNeutral i n)

  goNeutral i = \case
    NFree n -> Free n
    NRef sheetId cr -> Ref sheetId FullySpecified cr
    NApp n v -> goNeutral i n :$ go i v

  goTensor i (VTensorDescriptor vdims) =
    TensorDescriptor (map (go i) vdims)

  quoteAbs :: Fetch (Term Infer) Value -> Value
  quoteAbs v = either (error . show) id $ do
    let
      fetch = \case
        Cell _ sheetId ca -> pure (VNeutral (NRef sheetId (ca, ca)))
        _ -> throwSemanticError undefined
     in
      runExcept $ runReaderT v (fetch, Env undefined mempty)

instance Pretty Value where
  pretty = pretty . quote

valueP :: Parser Value
valueP = VLitOf . IntOf <$> decimal <|> constant
 where
  constant = do
    w <- originalText <$> keyword
    case Text.toLower w of
      "bool" -> pure (VLit Bool)
      "false" -> pure (VLitOf (BoolOf False))
      "true" -> pure (VLitOf (BoolOf True))
      "int" -> pure (VLit Int)
      _ -> fail $ "invalid keyword ‘" <> Text.unpack w <> "’"

{- Prelude -}

-- | treating Boolean values as globals
prelude :: Globals
prelude =
  Map.fromList
    [ ("not", Decl (vbool `vfun` vbool) (Just notImpl))
    , ("and", Decl (vbool `vfun` vbool `vfun` vbool) (Just andImpl))
    , ("or", Decl (vbool `vfun` vbool `vfun` vbool) (Just orImpl))
    ]
 where
  notImpl =
    vlam (Just (CaseInsensitive "x")) $ \case
      VLitOf (BoolOf b) -> VLitOf (BoolOf (not b))
      x -> VNeutral (NApp (NFree "not") x)

  andImpl = vlam (Just (CaseInsensitive "x")) $ \x ->
    vlam (Just (CaseInsensitive "y")) $ \y ->
      case (x, y) of
        (VLitOf (BoolOf a), VLitOf (BoolOf b)) -> VLitOf (BoolOf (a && b))
        _ -> VNeutral (NApp (NApp (NFree "and") x) y)

  orImpl = vlam (Just (CaseInsensitive "x")) $ \x ->
    vlam (Just (CaseInsensitive "y")) $ \y ->
      case (x, y) of
        (VLitOf (BoolOf a), VLitOf (BoolOf b)) -> VLitOf (BoolOf (a || b))
        _ -> VNeutral (NApp (NApp (NFree "or") x) y)

{- Errors -}

semanticErrorTitle :: SemanticError -> Text
semanticErrorTitle = \case
  TypeMismatch{} -> "Type Mismatch"
  TypeMismatchPi{} -> "Type Mismatch"
  TypeOfBound{} -> "Error"
  ExpectedSet{} -> "Illegal Kind"
  IllegalApplication{} -> "Illegal Application"
  UnknownIdentifier{} -> "Uknown Identifier"
  UnknownError{} -> "Error"

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
      UnknownError message ->
        [ "An unknown error occurred: "
        , pretty message
        ]

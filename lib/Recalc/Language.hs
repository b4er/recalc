{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.Language
Description : The core language implementation.

Defines the values for the lambda calculus, evaluation and bi-directional type
checking. It instantiates the classes 'Recalc' and 'UniverRecalc' such that the
language can be used by the Univer frontend.

The original implementation of the core calculus is based on
"A Tutorial Implementation of a Dependently Typed Lambda Calculus".

This extension lifts the calculus to spreadsheets and adds homogeneous tensor
types which can be constructed using cell-references.

Furthermore it extends it by the following primitives:

- sigma types (dependent products): 'Empty', 'Sigma', 'Nil', 'Pair' and 'Proj'
- implicit arguments (handled simultaneously with regular arguments): 'IArg'
- tensor types with dependent indices (not part of surface language, just for
  types of cell references): 'Tensor' and 'TensorOf'
- elaboration: 'Elaborate', 'PrimOp'

Type checking expects that the term is 'desugar'ed.

The evaluation phase expects that the term successfully type checked and the
resulting elaborated term is used.

Implicit function arguments are attempted to be resolved when an argument
is applied: first all types inferred using unification, then remaining
ones are attempted to be 'materialize'd.

This is used to implement light-weight "type classes":

> Plus: {t} -> {{plus: t -> t -> t}} -> t -> t -> t

When @Plus(1)@ is inferred @t@ is resolved by unification, the remaining
implicit argument becomes @{plus: int -> int -> int}@ and materialization
is expected to solve this constraint.

__References:__
  Andres Löh, Conor McBride, Wouter Swierstra.
  [A Tutorial Implementation of a Dependently Typed Lambda Calculus](https://dl.acm.org/doi/10.5555/1883634.1883637).
  Fundamenta Informaticae, Volume 102, Issue 2.
-}
module Recalc.Language
  ( -- * Types
    Env (..)
  , Term
  , Mode (Infer)
  , Type
  , SemanticError (..)

    -- ** Initial Global Variables
  , prelude

    -- * for test only
  , check'
  , eval'
  , infer'
  , Decl (..)
  , Value (..)
  , VTensorDescriptor (..)
  , Neutral (..)
  , Globals
  , vapp
  , vbool
  , vboolOf
  , vfree
  , vfun
  , vlam
  , vpi
  ) where

import Control.Monad (unless, void, when)
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Function (on)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Massiv.Array qualified as Array
import Data.Massiv.Array.Numeric ((!><!))
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import GHC.Float (int2Double)
import GHC.Generics (Generic)
import Prettyprinter hiding (column)
import Prettyprinter.Render.Text (renderStrict)

import Recalc.Array
import Recalc.Engine
  ( CellAddr
  , CellRange
  , CellRangeRef
  , CellType (CellFormula, CellValue)
  , FetchOf
  , Recalc (..)
  , SheetId
  , column
  , fetchType
  , fetchValue
  , row
  , runFetchWith
  , throwSemanticError
  )
import Recalc.Syntax.Parser (formulaP, valueP)
import Recalc.Syntax.Term
import Recalc.Syntax.Unify
import Recalc.Univer (Annotation (..), UniverRecalc (..))

-- | all semantic errors that can occur during type checking or evaluation
data SemanticError
  = TypeMismatch Type Type
  | TypeMismatchPi (Term Check) Type
  | TypeOfBound
  | ExpectedInt Value
  | ExpectedSet (Term Check)
  | IllegalApplication Type
  | IllegalImplicitApplication Type
  | IllegalArrayType [(CellAddr, Type)]
  | IllegalProjection CaseInsensitive Type
  | UnknownIdentifier Name
  | UnificationError UnificationError
  | LabelMismatch (Maybe CaseInsensitive) (Maybe CaseInsensitive)
  | TypeMismatchSigma (Term Check) Type
  deriving (Eq, Show)

-- | a global variable must declare its 'Type' but not necessarily its 'Value'
data Decl = Decl
  { declType :: Type
  , declValue :: Maybe Value
  }

type Globals = Map Name Decl

-- | the environment contains just the globally bound variables
newtype Env = Env {globals :: Globals}

type Spreadsheet = FetchOf (Term Infer)

{- Values -}

-- ** Normal Forms

data Neutral
  = NFree !Name
  | NRef !SheetId !CellRange
  | NApp !Arg !Neutral !Value
  | NProj !CaseInsensitive !Neutral
  deriving (Generic)

data VTensorDescriptor = VTensorDescriptor Value [Value]

-- | terms evaluate to values. values with bound (such as 'VPi') variables in them
-- are represented using a HOAS.
--
-- note that the return type is a monadic value such that those values can
-- easily resolve types/values from other cells.
data Value
  = -- | lambda abstractions
    VLam !Arg !(Maybe CaseInsensitive) !(Value -> Spreadsheet Value)
  | -- | set of k-th order (hierarchy of types)
    VSet !Int
  | -- | dependent function
    VPi !Arg !(Maybe CaseInsensitive) !Value !(Value -> Spreadsheet Value)
  | -- | empty record type
    VEmpty
  | -- | sigma type
    VSigma !(Maybe CaseInsensitive) !Value !(Value -> Spreadsheet Value)
  | -- | empty record
    VNil
  | -- | pair
    VPair !(Maybe CaseInsensitive) !Value !Value
  | -- | type literal
    VLit Lit
  | -- | value literal
    VLitOf LitOf
  | -- | builtin operations (monomorph)
    VPrimOp PrimOp
  | -- | tensor type
    VTensor VTensorDescriptor
  | -- | tensor value
    VTensorOf VTensorDescriptor (Array Value)
  | -- | stuck terms
    VNeutral !Neutral
  deriving (Generic)

instance Eq Value where
  (==) = (==) `on` quote

instance Show Value where
  show = show . quote

instance Pretty Value where
  pretty = pretty . quote

type Type = Value

-- | free variable
vfree :: Name -> Value
vfree = VNeutral . NFree

infixr 9 `vfun`

-- | function type constructor (no bound type)
vfun :: Value -> Value -> Value
vfun dom range = VPi EArg Nothing dom (const $ pure range)

vlam :: Arg -> Maybe CaseInsensitive -> (Value -> Value) -> Value
vlam arg n body = VLam arg n $ pure . body

-- | dependent function
vpi :: Arg -> Maybe CaseInsensitive -> Value -> (Value -> Value) -> Value
vpi arg vname dom range = VPi arg vname dom $ pure . range

-- | nested dependent functions
vpis :: [(Arg, Text, Value)] -> ([Value] -> Value) -> Value
vpis doms range =
  let
    go (arg, var, val) f vs =
      vpi arg (Just (CaseInsensitive var)) val $ f . (: vs)
  in
    foldr go (range . reverse) doms []

-- | non-dependent record
vrecord :: [(Text, Value)] -> Value
vrecord = foldr (\(n, t) -> VSigma (Just (CaseInsensitive n)) t . const . pure) VEmpty

-- | application (must be able to fail)
vapp :: Value -> Value -> Spreadsheet Value
vapp (VLam _arg _n f) v = f v
vapp (VNeutral n) v = pure (VNeutral (NApp EArg n v))
vapp f _ = throwSemanticError (IllegalApplication f)

-- | type literals
vbool, vint :: Type
vbool = VLit Bool
vint = VLit Int

-- | boolean values
vboolOf :: Bool -> Value
vboolOf = VLitOf . BoolOf

-- | integer values
vintOf :: Int -> Value
vintOf = VLitOf . IntOf

-- | tensor type
vtensor
  :: Value
  -- ^ the type of the elements
  -> [Value]
  -- ^ the dimensions of each component (should be non-zero, positive integers)
  -> Value
vtensor base = VTensor . VTensorDescriptor base

splitVPair :: Value -> ([(Maybe CaseInsensitive, Value)], Value)
splitVPair = go []
 where
  go acc (VPair n x y) = go ((n, x) : acc) y
  go acc y = (reverse acc, y)

vproj :: CaseInsensitive -> Value -> Value
vproj l = \case
  pair@VPair{}
    | Just v <- lookup (Just l) . fst $ splitVPair pair ->
        v
  VNeutral n -> VNeutral (NProj l n)
  _ -> error "vproj"

cellRangeAddrs :: CellRange -> [CellAddr]
cellRangeAddrs (start, end) = [(i, j) | i <- [row start .. row end], j <- [column start .. column end]]

cellRangeDescriptor' :: Value -> CellRange -> VTensorDescriptor
cellRangeDescriptor' base (start, end) = VTensorDescriptor base' dims'
 where
  (m, n) = (row end - row start + 1, column end - column start + 1)

  dim' x = if x == 1 then id else (vintOf x :)

  (base', dims') =
    dim' m . dim' n <$> case base of
      VTensor (VTensorDescriptor b ds) -> (b, ds)
      _ -> (base, [])

-- ** Dependencies

deps' :: Term m -> Set CellRangeRef
deps' = go mempty
 where
  go :: Set CellRangeRef -> Term m -> Set CellRangeRef
  go acc = \case
    Inf x -> go acc x
    Lam _ _ body -> go acc body
    Ann x ty -> go (go acc x) ty
    Set{} -> acc
    Pi _ _ ty1 ty2 -> go (go acc ty1) ty2
    Empty -> acc
    Sigma _ x y -> go (go acc x) y
    Nil -> acc
    Proj _ x -> go acc x
    Pair _ x y -> go (go acc x) y
    Bound{} -> acc
    Op1 _ x -> go acc x
    Op _ x y -> go (go acc x) y
    Free{} -> acc
    Ref sheetId _ cr -> Set.singleton (sheetId, cr)
    Lit{} -> acc
    LitOf{} -> acc
    Tensor td -> goTensor acc td
    TensorOf td arr -> foldr (flip go) (goTensor acc td) arr
    f `App` (_arg, x) -> go (go acc f) x
    -- should not be used (just to make GHC happy)
    Elaborate x -> go acc x
    PrimOp{} -> acc

  goTensor acc (TensorDescriptor base _dims) = go acc base

-- ** Quoting

quote :: Value -> Term Check
quote = go 0
 where
  go i = \case
    VLam arg vname body ->
      let
        body' = quoteAbs $ body (vfree (Quote i))
      in
        Lam arg vname (go (i + 1) body')
    VSet k -> Inf (Set k)
    VPi arg vname dom range ->
      let
        range' = quoteAbs $ range (vfree (Quote i))
      in
        Inf $ Pi arg vname (go i dom) (go (i + 1) range')
    VEmpty -> Inf Empty
    VSigma n x y ->
      let
        y' = quoteAbs $ y (vfree (Quote i))
      in
        Inf $ Sigma n (go i x) (go (i + 1) y')
    VNil -> Inf Nil
    VPair n x y -> Pair n (go i x) (go i y)
    VLit lit -> Inf (Lit lit)
    VLitOf val -> Inf (LitOf val)
    VPrimOp prim -> Inf (PrimOp prim)
    VTensor td -> Inf (Tensor (goTensor i td))
    VTensorOf td arr -> Inf (TensorOf (goTensor i td) (go i <$> toList arr))
    VNeutral n -> Inf (goNeutral i n)

  goNeutral i = \case
    NFree (Quote k) -> Bound (i - k - 1)
    NFree n -> Free n
    NRef sheetId cr -> Ref sheetId Unspecified cr
    NApp arg n v -> goNeutral i n `App` (arg, go i v)
    NProj l n -> Proj l (goNeutral i n)

  goTensor i (VTensorDescriptor base vdims) =
    TensorDescriptor (case go i base of Inf x -> x; _ -> damn) (map (go i) vdims)

  quoteAbs :: Spreadsheet Value -> Value
  quoteAbs =
    either (error . show) id
      . runFetchWith
        (Env prelude)
        ( \(sheetId, ca) ->
            pure
              ( VNeutral (NRef sheetId (ca, ca))
              , Ref sheetId Unspecified (ca, ca)
              , VNeutral (NRef sheetId (ca, ca))
              )
        )

  damn = error "quote: this shouldn't happen"

-- * Bi-directional typechecking

-- | fails if the type does not check, returns an elaborated term otherwise
check' :: [Type] -> Type -> Term Check -> Spreadsheet (Term Check)
check' ctxt ty = \case
  Inf x -> do
    (ty', x') <- infer' ctxt x
    when (quote ty' /= quote ty)
      $ throwSemanticError (TypeMismatch ty' ty)
    pure (Inf x')
  x@(Lam arg n body) -> case ty of
    VPi EArg _p t t' -> do
      t'' <- t' t
      Lam arg n <$> check' (t : ctxt) t'' body
    ty' -> throwSemanticError (TypeMismatchPi x ty')
  pair@(Pair n x y) -> case ty of
    -- normalize sigma.. this does not allow re-ordering independent fields
    VSigma n' xT yT -> do
      when (n /= n')
        $ throwSemanticError (LabelMismatch n n')
      x' <- check' ctxt xT x
      yT' <- yT xT
      y' <- check' (xT : ctxt) yT' y
      pure (Pair n x' y')
    _ -> throwSemanticError (TypeMismatchSigma pair ty)

-- . traceShowWith ("check'"::String,ctxt,ty,)

infer' :: [Type] -> Term Infer -> Spreadsheet (Type, Term Infer)
infer' ctxt = \case
  Ann e t -> do
    void $ inferUniverse' ctxt (Inf t)
    t'' <- eval' ctxt t
    e' <- check' ctxt t'' e
    pure (t'', elaborate e')
  Set k -> pure (VSet (k + 1), Set k)
  Pi arg p dom range -> do
    (m, dom') <- inferUniverse' ctxt dom
    dom'' <- eval' ctxt dom
    -- regardless of _arg, the pi-bound variable needs to be accessible in the
    -- range
    (n, range') <- inferUniverse' (dom'' : ctxt) range
    pure (VSet (max m n), Pi arg p (Inf dom') (Inf range'))
  Empty -> pure (VSet 0, Empty)
  Sigma p x y -> do
    (m, x') <- inferUniverse' ctxt x
    xT <- eval' ctxt x
    (n, y') <- inferUniverse' (xT : ctxt) y
    pure (VSet (max m n), Sigma p (Inf x') (Inf y'))
  Nil -> pure (VEmpty, Nil)
  Proj l x ->
    infer' ctxt x >>= \case
      (sigma@VSigma{}, x')
        | Inf e <- quote sigma
        , Just t <- lookup (Just l) . fst $ splitSigma e ->
            (,Proj l x') <$> eval' ctxt t
      (ty, _) -> throwSemanticError (IllegalProjection l ty)
  Bound i -> pure (ctxt !! i, Bound i)
  Op1{} -> error "op1 is syntax sugar"
  Op{} -> error "op is syntax sugar"
  Free name -> do
    Env{globals} <- ask
    case Map.lookup name globals of
      Just Decl{declType} -> pure (declType, Free name)
      Nothing -> throwSemanticError (UnknownIdentifier name)
  ref@(Ref sheetId _ cr@(start, end))
    | start == end -> (,ref) <$> fetchType @(Term Infer) (sheetId, start)
    | otherwise -> do
        ty0 <- fetchType @(Term Infer) (sheetId, start)
        -- make sure all references match
        types <- mapM (\ca -> (ca,) <$> fetchType @(Term Infer) (sheetId, ca)) (cellRangeAddrs cr)
        let errors = filter ((ty0 /=) . snd) types
        unless (null errors)
          $ throwSemanticError (IllegalArrayType errors)
        pure (VTensor (cellRangeDescriptor' ty0 cr), ref)
  lit@Lit{} -> pure (VSet 0, lit)
  lit@(LitOf val) -> pure (inferLiteral val, lit)
  -- infer for Tensor and TensorOf should never be called
  -- since it's not part of surface language..
  ---
  -- implement them anyway:
  Tensor (TensorDescriptor base dims) -> do
    (_, base') <- inferUniverse' ctxt (Inf base)
    pure (VSet 0, Tensor (TensorDescriptor base' dims))
  TensorOf (TensorDescriptor base dims) arr -> do
    (baseType, base') <- infer' ctxt base
    dims' <- mapM (check' ctxt vint) dims
    vdims <- mapM (eval' ctxt) dims
    arr' <- mapM (check' ctxt baseType) arr
    let term = TensorOf (TensorDescriptor base' dims') arr'
    pure . (,term) . VTensor $ VTensorDescriptor baseType vdims
  f `App` (IArg, x) ->
    infer' ctxt f >>= \case
      (VPi IArg _ t t', fnTerm) -> do
        xTerm <- check' ctxt t x
        x' <- eval' ctxt x
        (,fnTerm `App` (IArg, xTerm)) <$> t' x'
      (t@VPi{}, _) -> throwSemanticError (IllegalImplicitApplication t)
      (t, _) -> throwSemanticError (IllegalApplication t)
  f `App` (EArg, x) ->
    resolveImplicits ctxt x =<< infer' ctxt f
  -- we never infer elaborated terms..
  Elaborate _ -> error "the impossible happened"
  p@(PrimOp prim) ->
    (,p) <$> case prim of
      PrimPlus -> pure (vint `vfun` vint `vfun` vint)
      PrimMinus -> pure (vint `vfun` vint `vfun` vint)
      PrimMult -> pure (vint `vfun` vint `vfun` vint)

-- . traceShowWith ("infer'"::String,ctxt,)

resolveImplicits :: [Type] -> Term Check -> (Type, Term Infer) -> Spreadsheet (Type, Term Infer)
resolveImplicits ctxt x (fnTy, fnTerm) = do
  go 0 [] fnTy >>= \case
    Just (implicits, dom, range) -> do
      case x of
        -- if the argument is not a lambda and there were implicit arguments,
        -- try inferring the implicits from unification
        Inf x' | not (null implicits) -> do
          (xTy, xTerm) <- infer' ctxt x'
          case unify (quote xTy, quote dom) of
            Left err -> throwSemanticError (UnificationError err)
            Right s -> do
              let
                -- unknown (from unification) implicit arguments
                notUnified =
                  [ (k, (n, apply s (quote d)))
                  | (k, (n, d)) <- zip [0 ..] implicits
                  , isNothing (Map.lookup k s)
                  ]

              -- evaluate types and try materialize
              impls <-
                Map.fromList . mapMaybe (\(k, ty) -> (k,) <$> materialize ty)
                  <$> mapM (\(k, (_, d)) -> (k,) <$> eval' ctxt d) notUnified

              let
                -- left over implicits: neither solved by unification nor materialization
                implicits' = filter ((`Map.notMember` impls) . fst) notUnified

                -- pi over implicits that were not inferred
                piImplicits y =
                  foldr (\(k, (n, d)) -> Inf . Pi IArg n d . sub k 0) y implicits'

                -- lambda over implicits that were not inferred
                lamImplicits y =
                  foldr (\(k, (n, _)) -> Lam IArg n . sub k 0) y implicits'

                -- apply all inferred implicits to term (elaboration)
                fnTerm' =
                  lamImplicits
                    . Inf
                    . (`App` (EArg, Inf xTerm))
                    $ foldl'
                      (\fn i -> fn `App` (IArg, i))
                      fnTerm
                      [ case Map.lookup k s of
                        Just v -> Inf v
                        Nothing
                          | Just v <- Map.lookup k impls -> v
                          | otherwise -> Inf (Free (Implicit k))
                      | (k, _) <- zip [0 ..] implicits
                      ]

              -- set elaborated term
              fmap (,elaborate fnTerm')
                -- close over the uninferred ones
                . eval' ctxt
                . piImplicits
                -- substitute inferred implicits
                . apply s
                . quote
                -- the resulting type
                =<< range
                =<< eval' ctxt x
        -- else, regular bi-directional application checking
        _ -> do
          xTerm <- check' ctxt dom x
          x' <- eval' ctxt x
          (,fnTerm `App` (EArg, xTerm)) <$> range x'
    Nothing -> throwSemanticError (IllegalApplication fnTy)
 where
  go
    :: Int
    -> [(Maybe CaseInsensitive, Value)]
    -> Value
    -> Spreadsheet
        ( Maybe
            ( [(Maybe CaseInsensitive, Value)]
            , Value
            , Value -> Spreadsheet Value
            )
        )
  go i acc = \case
    VPi IArg n t t' -> go (i + 1) ((n, t) : acc) =<< t' (vfree (Implicit i))
    VPi EArg _ t t' -> pure (Just (reverse acc, t, t'))
    _ -> pure Nothing

materialize :: Value -> Maybe (Term Check)
materialize ty
  | ty == vrecord [("plus", vint `vfun` vint `vfun` vint)] =
      Just $ record [("plus", Inf (PrimOp PrimPlus))]
  | ty == vrecord [("minus", vint `vfun` vint `vfun` vint)] =
      Just $ record [("minus", Inf (PrimOp PrimMinus))]
  | ty == vrecord [("mult", vint `vfun` vint `vfun` vint)] =
      Just $ record [("mult", Inf (PrimOp PrimMult))]
  | otherwise = Nothing

inferLiteral :: LitOf -> Type
inferLiteral =
  VLit . \case
    BoolOf{} -> Bool
    IntOf{} -> Int

-- | use this to determine the level in the type-hierarchy such
-- that we do not need to type @type: type@
inferUniverse' :: [Type] -> Term Check -> Spreadsheet (Int, Term Infer)
inferUniverse' ctxt = \case
  Inf e -> do
    (t, e') <- infer' ctxt e
    case quote t of
      Inf (Set k) -> pure (k, e')
      Inf (Bound i) -> do
        let v = ctxt !! i
        (,Bound i) . fst <$> inferUniverse' ctxt (quote v)
      Inf (Free name) -> do
        Env{globals} <- ask
        case Map.lookup name globals of
          Just Decl{declType = VSet k} -> pure (k, Free name)
          _ -> throwSemanticError (UnknownIdentifier name)
      Inf tensor@Tensor{} -> pure (0, tensor)
      ty -> throwSemanticError (ExpectedSet ty)
  ty -> throwSemanticError (ExpectedSet ty)

-- * Evaluation

-- the error calls below are ruled out by the type checker
eval' :: [Value] -> Term m -> Spreadsheet Value
eval' ctxt = \case
  Inf x -> eval' ctxt x
  Lam arg p body -> pure . VLam arg p $ \v -> eval' (v : ctxt) body
  Ann x _ty -> eval' ctxt x
  Set k -> pure (VSet k)
  Pi arg p dom range -> do
    dom' <- eval' ctxt dom
    pure $ VPi arg p dom' (\v -> eval' (v : ctxt) range)
  Empty -> pure VEmpty
  Sigma n x y -> do
    x' <- eval' ctxt x
    pure . VSigma n x' $ \v -> eval' (v : ctxt) y
  Nil -> pure VNil
  Pair n x y -> VPair n <$> eval' ctxt x <*> eval' ctxt y
  Proj l x -> vproj l <$> eval' ctxt x
  Bound i -> pure (ctxt !! i)
  Op1{} -> error "op1 is syntax sugar"
  Op{} -> error "op is syntax sugar"
  Ref sheetId _ cr@(start, end)
    -- if it's a single reference: just the value there
    | start == end -> fetchValue @(Term Infer) (sheetId, start)
    | otherwise -> do
        let addrs = cellRangeAddrs cr
        -- evaluate the descriptor to get dimensions
        vtd@(VTensorDescriptor _ vdims) <-
          (`cellRangeDescriptor'` cr) <$> fetchType @(Term Infer) (sheetId, start)
        vdims' <- evalShape vdims -- make sure they are integers
        -- evaluate each position in the referenced range
        VTensorOf vtd . fromList vdims'
          <$> concatMapM (\ca -> flattenTensor <$> fetchValue @(Term Infer) (sheetId, ca)) addrs
  Free name -> do
    Env{globals} <- ask
    case Map.lookup name globals of
      Just Decl{declValue = Just v} -> pure v
      _ -> pure (vfree name)
  Lit lit -> pure (VLit lit)
  LitOf val -> pure (VLitOf val)
  -- simple (should not be called)
  Tensor td -> uncurry vtensor <$> evalTensor' ctxt td
  TensorOf td xs -> do
    (vbase, vdims) <- evalTensor' ctxt td
    idims <- mapM evalInt vdims
    vs <- mapM (eval' ctxt) xs
    let
      vtd = VTensorDescriptor vbase vdims
      arr = fromList idims vs
    pure (VTensorOf vtd arr)
  f `App` (_arg, x) -> do
    f' <- eval' ctxt f
    x' <- eval' ctxt x
    vapp f' x'
  Elaborate x -> eval' ctxt x
  PrimOp prim -> pure $ case prim of
    PrimPlus -> intOp (+)
    PrimMinus -> intOp (-)
    PrimMult -> intOp (*)

evalTensor' :: [Value] -> TensorDescriptor -> Spreadsheet (Value, [Value])
evalTensor' ctxt (TensorDescriptor base dims) = (,) <$> eval' ctxt base <*> mapM (eval' ctxt) dims

evalShape :: [Value] -> Spreadsheet [Int]
evalShape = mapM evalInt

evalInt :: Value -> Spreadsheet Int
evalInt = \case
  VLitOf (IntOf i) -> pure i
  v -> throwSemanticError (ExpectedInt v)

-- auxiliary functions
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

flattenTensor :: Value -> [Value]
flattenTensor = \case
  VTensorOf _ arr -> toList arr
  v -> [v]

{- language semantics -}

-- | instantiate the interface with the language defined above
instance Recalc (Term Infer) where
  type EnvOf (Term Infer) = Env
  type ErrorOf (Term Infer) = SemanticError
  type ElaborationOf (Term Infer) = Term Infer
  type TypeOf (Term Infer) = Value
  type ValueOf (Term Infer) = Value

  parseCell = \case
    CellValue -> valueP
    CellFormula -> fmap desugar formulaP

  depsOf = deps'

  inferElaborate = infer' []
  eval = eval' []

-- | for error annotations and sheet-defined functions (not implemented) we need
-- to instantiate the Univer-specific interface too:
instance UniverRecalc (Term Infer) where
  -- define
  --  :: Text
  --  -> Map CellAddr (Maybe ((String, CellType), Maybe (t, Maybe (ValueOf t, Maybe (ValueOf t)))))
  --  -> [(Text, CellRange)]
  --  -> CellRange
  --  -> EnvOf t
  --  -> Either (ErrorOf t) (EnvOf t)
  define _name _ _ _ = Right

  errorAnnotation = \case
    TypeMismatch ty1 ty2 ->
      annotation
        "Type Mismatch"
        [ "Type mismatch, cannot match ‘"
        , pretty ty1
        , "‘ with ‘"
        , pretty ty2
        , "‘."
        ]
    TypeMismatchPi x ty ->
      annotation
        "Type Mismatch"
        [ "Lambda term ‘"
        , pretty x
        , "‘ cannot be matched with ‘"
        , pretty ty
        , "‘, expected Π-type."
        ]
    TypeOfBound -> annotation "Error" ["Type of Bound (should not happen)."]
    ExpectedInt x ->
      annotation
        "Unexpected Value"
        [ "Unexpected value ‘"
        , pretty x
        , "‘, should be an ‘int‘."
        ]
    ExpectedSet x ->
      annotation
        "Illgeal Kind"
        [ "Expected term of type *, got ‘"
        , pretty x
        , "‘."
        ]
    IllegalApplication ty ->
      annotation
        "Illegal Application"
        [ "Cannot apply a term to value with type ‘"
        , pretty ty
        , "’."
        ]
    IllegalImplicitApplication ty ->
      annotation
        "Illegal Application"
        [ "Cannot apply an implicit term to value with type ‘"
        , pretty ty
        , "’."
        ]
    IllegalArrayType locs ->
      annotation
        "Unexpected Type in Array"
        [ "Expected ‘int’ but got "
        , hsep . punctuate comma
            $ map (\(ca, ty) -> "‘" <> pretty ty <> "’ at" <+> pretty (showExcel26 ca)) locs
        ]
    IllegalProjection l ty ->
      annotation
        "Illegal Projection"
        [ "Label ‘"
        , pretty l
        , "’ does not exist on type‘"
        , pretty ty
        , "’."
        ]
    UnknownIdentifier name ->
      annotation
        "Unknown Identifier"
        [ "Unknown identifier ‘"
        , pretty name
        , "’."
        ]
    UnificationError err -> unificationErrorAnnotation err
    LabelMismatch l l' ->
      annotation
        "Type Mismatch"
        [ "Product types cannot be matched, the labels ‘"
        , pretty l
        , "‘ and ‘"
        , pretty l'
        , "‘ differ."
        ]
    TypeMismatchSigma x ty ->
      annotation
        "Type Mismatch"
        [ "Product type ‘"
        , pretty x
        , "‘ cannot be matched with ‘"
        , pretty ty
        , "‘, expected Σ-type."
        ]
   where
    annotation title docs = Annotation title (render (cat docs))

render :: Doc ann -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

{- Syntactic Sugar -}

-- | desugaring terms: resolve operators to globals
desugar :: Term Infer -> Term Infer
desugar = go
 where
  go :: forall m. Term m -> Term m
  go = \case
    Op1 op1 x -> Free (fromString (show op1)) :$ Inf (go x)
    Op op x y -> Free (fromString (show op)) :$ Inf (go x) :$ Inf (go y)
    -- recurse
    Inf x -> Inf (go x)
    Lam arg n x -> Lam arg n (go x)
    Ann e t -> Ann (go e) (go t)
    Set k -> Set k
    Pi arg n x y -> Pi arg n (go x) (go y)
    Tensor td -> Tensor (goTensor td)
    TensorOf td arr -> TensorOf (goTensor td) (go <$> arr)
    x `App` (arg, y) -> go x `App` (arg, go y)
    -- leaves
    x -> x

  goTensor (TensorDescriptor base vdims) =
    TensorDescriptor (go base) (map go vdims)

{- Prelude -}

prelude :: Globals
prelude =
  Map.fromList
    [ ("not", Decl (vbool `vfun` vbool) (Just notImpl))
    , ("and", Decl (vbool `vfun` vbool `vfun` vbool) (Just andImpl))
    , ("or", Decl (vbool `vfun` vbool `vfun` vbool) (Just orImpl))
    , ("mmult", Decl mmultT (Just mmultImpl))
    , -- operators
      ("LogicalNegate", Decl (vbool `vfun` vbool) (Just notImpl))
    , ("Negate", Decl (vint `vfun` vint) (Just negateImpl))
    ,
      ( "Mult"
      , classDecl
          "mult"
          (\t -> t `vfun` t `vfun` t)
          (\impl -> binOp $ \x y -> (`vapp` y) =<< impl `vapp` x)
      )
    ,
      ( "Plus"
      , classDecl
          "plus"
          (\t -> t `vfun` t `vfun` t)
          (\impl -> binOp $ \x y -> (`vapp` y) =<< impl `vapp` x)
      )
    ,
      ( "Minus"
      , classDecl
          "minus"
          (\t -> t `vfun` t `vfun` t)
          (\impl -> binOp $ \x y -> (`vapp` y) =<< impl `vapp` x)
      )
    ]
 where
  notImpl =
    vlam EArg (Just (CaseInsensitive "x")) $ \case
      VLitOf (BoolOf b) -> VLitOf (BoolOf (not b))
      x -> VNeutral (NApp EArg (NFree "not") x)

  andImpl = vlam EArg (Just (CaseInsensitive "x")) $ \x ->
    vlam EArg (Just (CaseInsensitive "y")) $ \y ->
      case (x, y) of
        (VLitOf (BoolOf a), VLitOf (BoolOf b)) -> VLitOf (BoolOf (a && b))
        _ -> VNeutral (NApp EArg (NApp EArg (NFree "and") x) y)

  orImpl = vlam EArg (Just (CaseInsensitive "x")) $ \x ->
    vlam EArg (Just (CaseInsensitive "y")) $ \y ->
      case (x, y) of
        (VLitOf (BoolOf a), VLitOf (BoolOf b)) -> VLitOf (BoolOf (a || b))
        _ -> VNeutral (NApp EArg (NApp EArg (NFree "or") x) y)

  mmultT = vpis [(IArg, "m", vint), (IArg, "n", vint), (IArg, "k", vint)]
    $ \[m, n, k] -> vtensor vint [m, n] `vfun` vtensor vint [n, k] `vfun` vtensor vint [m, k]

  mmultImpl =
    vlam IArg (Just (CaseInsensitive "m")) $ \(VLitOf (IntOf m)) ->
      vlam EArg (Just (CaseInsensitive "n")) $ \(VLitOf (IntOf n)) ->
        vlam IArg (Just (CaseInsensitive "k")) $ \(VLitOf (IntOf k)) ->
          vlam EArg (Just (CaseInsensitive "u")) $ \u ->
            VLam EArg (Just (CaseInsensitive "v")) $ \v ->
              case (u, v) of
                (VTensorOf _ (Array _ arr), VTensorOf _ (Array _ arr')) -> do
                  iarr <- Array.resize' (Array.Sz2 m n) <$> mapM (fmap int2Double . evalInt) arr
                  iarr' <- Array.resize' (Array.Sz2 n k) <$> mapM (fmap int2Double . evalInt) arr'
                  let
                    multArr = Array.resize' (Array.Sz1 (m * k)) (iarr !><! iarr')
                    vtd = VTensorDescriptor vint (vintOf <$> [m, k])
                  pure (VTensorOf vtd (vintOf . round @Double <$> Array [m, k] multArr))
                _ -> pure . VNeutral $ NApp EArg (NApp EArg (NFree "mmult") u) v

  negateImpl = unOp negate

unOp :: (Int -> Int) -> Value
unOp op =
  VLam IArg (Just (CaseInsensitive "x")) $ \(VLitOf (IntOf x)) ->
    pure (VLitOf (IntOf (op x)))

binOp :: (Value -> Value -> Spreadsheet Value) -> Value
binOp f =
  vlam EArg (Just (CaseInsensitive "x"))
    $ VLam EArg (Just (CaseInsensitive "y")) . f

-- assume implementation record
assume :: Text -> (Value -> Value) -> Value
assume f t = vpi IArg (Just (CaseInsensitive "t")) (VSet 0) $ \a ->
  VPi IArg Nothing (VSigma (Just (CaseInsensitive f)) (t a) (const (pure VEmpty)))
    . const
    $ pure (t a)

-- pass implementation record
implement :: Text -> (Value -> Value) -> Value
implement f v =
  vlam IArg (Just (CaseInsensitive "t")) $ \_t ->
    vlam IArg (Just (CaseInsensitive "impl")) $ \impl ->
      v (vproj (CaseInsensitive f) impl)

-- declare a light-weigth class
classDecl :: Text -> (Value -> Value) -> (Value -> Value) -> Decl
classDecl c t = Decl (assume c t) . Just . implement c

intOp :: (Int -> Int -> Int) -> Value
intOp op =
  vlam EArg (Just (CaseInsensitive "x")) $ \(VLitOf (IntOf x)) ->
    VLam EArg (Just (CaseInsensitive "y")) $ \(VLitOf (IntOf y)) ->
      pure (VLitOf (IntOf (x `op` y)))

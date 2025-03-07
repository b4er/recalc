{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recalc.Language
  ( Env (..)
  , Term
  , Mode (Infer)
  -- , Value
  , Type
  , SemanticError (..)
  , prelude
  -- for test only
  , check'
  , eval'
  , infer'
  , Decl (..)
  , Value (..)
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
import Data.Array.Dynamic (Array, ShapeL)
import Data.Array.Dynamic qualified as Array
import Data.Function (on)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter hiding (column)
import Prettyprinter.Render.Text (renderStrict)

import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Map qualified as Map
import Recalc.Engine
  ( CellAddr
  , CellRange
  , CellRangeRef
  , CellType (CellFormula, CellValue)
  , Fetch
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
import Recalc.Univer

data SemanticError
  = TypeMismatch Type Type
  | TypeMismatchPi (Term Check) Type
  | TypeOfBound
  | ExpectedInt Value
  | ExpectedSet (Term Check)
  | IllegalApplication Type
  | IllegalArrayType [(CellAddr, Type)]
  | UnknownIdentifier Name
  | UnknownError Text
  deriving (Eq, Show)

instance Pretty Value where pretty = pretty . quote

type Spreadsheet = Fetch Env SemanticError Value

-- | a global variable must declare its "Type" but not necessarily its "Value"
data Decl = Decl
  { declType :: Type
  , declValue :: Maybe Value
  }

type Globals = Map Name Decl

newtype Env = Env {globals :: Globals}

{- Values -}

-- ** Normal Forms

data Neutral
  = NFree !Name
  | NRef !SheetId !CellRange
  | NApp !Neutral !Value
  deriving (Generic)

data VTensorDescriptor = VTensorDescriptor Value [Value]

data Value
  = VLam !(Maybe CaseInsensitive) !(Value -> Spreadsheet Value)
  | VSet !Int
  | VPi !(Maybe CaseInsensitive) !Value !(Value -> Spreadsheet Value)
  | VLit Lit
  | VLitOf LitOf
  | VTensor VTensorDescriptor
  | VTensorOf VTensorDescriptor (Array Value)
  | VNeutral !Neutral

instance Eq Value where
  (==) = (==) `on` quote

instance Show Value where
  show = show . quote

type Type = Value

vfree :: Name -> Value
vfree = VNeutral . NFree

infixr 9 `vfun`

vfun :: Value -> Value -> Value
vfun dom range = VPi Nothing dom (const $ pure range)

vlam :: Maybe CaseInsensitive -> (Value -> Value) -> Value
vlam vname body = VLam vname $ pure . body

vlams :: [Text] -> ([Value] -> Value) -> Value
vlams vars body =
  let
    go var f vs =
      vlam (Just (CaseInsensitive var)) $ f . (: vs)
  in
    foldr go (body . reverse) vars []

vpi :: Maybe CaseInsensitive -> Value -> (Value -> Value) -> Value
vpi vname dom range = VPi vname dom $ pure . range

vpis :: [(Text, Value)] -> ([Value] -> Value) -> Value
vpis doms range =
  let
    go (var, val) f vs =
      vpi (Just (CaseInsensitive var)) val $ f . (: vs)
  in
    foldr go (range . reverse) doms []

vapp :: Value -> Value -> Spreadsheet Value
vapp (VLam _n f) v = f v
vapp (VNeutral n) v = pure (VNeutral (NApp n v))
vapp f _ = throwSemanticError (IllegalApplication f)

vbool, vint :: Type
vbool = VLit Bool
vint = VLit Int

vboolOf :: Bool -> Value
vboolOf = VLitOf . BoolOf

vintOf :: Int -> Value
vintOf = VLitOf . IntOf

vtensor :: Value -> [Value] -> Value
vtensor base = VTensor . VTensorDescriptor base

vtensorOf :: Value -> [Value] -> Array Value -> Value
vtensorOf value = VTensorOf . VTensorDescriptor value

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
    Lam _ body -> go acc body
    Ann x ty -> go (go acc x) ty
    Set{} -> acc
    Pi _ ty1 ty2 -> go (go acc ty1) ty2
    Bound{} -> acc
    Free{} -> acc
    Ref sheetId _ cr -> Set.singleton (sheetId, cr)
    Lit{} -> acc
    LitOf{} -> acc
    Tensor td -> goTensor acc td
    TensorOf td arr -> Array.foldrA (flip go) (goTensor acc td) arr
    f :$ x -> go (go acc f) x

  goTensor acc (TensorDescriptor base _dims) = go acc base

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

  goTensor i (VTensorDescriptor base vdims) =
    TensorDescriptor (case go i base of Inf x -> x; _ -> damn) (map (go i) vdims)

  quoteAbs :: Spreadsheet Value -> Value
  quoteAbs =
    either (error . show) id
      . runFetchWith
        (Env prelude)
        (\_ (sheetId, ca) -> pure (VNeutral (NRef sheetId (ca, ca))))

  damn = error "quote: this shouldn't happen"

-- * Bi-directional typechecking

check' :: [Type] -> Type -> Term Check -> Spreadsheet ()
check' ctxt ty = \case
  Inf x -> do
    ty' <- infer' ctxt x
    when (quote ty' /= quote ty)
      $ throwSemanticError (TypeMismatch ty' ty)
  x@(Lam _ body) -> case ty of
    VPi _p t t' -> do
      t'' <- t' t
      check' (t : ctxt) t'' body
    ty' -> throwSemanticError (TypeMismatchPi x ty')

-- . traceShowWith ("check'"::String,ctxt,ty,)

infer' :: [Type] -> Term Infer -> Spreadsheet Type
infer' ctxt = \case
  Ann e t -> do
    void $ inferUniverse' ctxt (Inf t)
    t' <- eval' ctxt t
    t' <$ check' ctxt t' e
  Set k -> pure $ VSet (k + 1)
  Pi _p dom range -> do
    m <- inferUniverse' ctxt dom
    dom' <- eval' ctxt dom
    n <- inferUniverse' (dom' : ctxt) range
    pure $ VSet (max m n)
  Bound i -> pure (ctxt !! i)
  Free name -> do
    Env{globals} <- ask
    case Map.lookup name globals of
      Just Decl{declType} -> pure declType
      Nothing -> throwSemanticError (UnknownIdentifier name)
  Ref sheetId _ cr@(start, end)
    | start == end -> fetchType (sheetId, start)
    | otherwise -> do
        ty0 <- fetchType (sheetId, start)
        -- make sure all references match
        types <- mapM (\ca -> (ca,) <$> fetchType (sheetId, ca)) (cellRangeAddrs cr)
        let errors = filter ((ty0 /=) . snd) types
        unless (null errors)
          $ throwSemanticError (IllegalArrayType errors)
        pure (VTensor (cellRangeDescriptor' ty0 cr))
  Lit{} -> pure (VSet 0)
  LitOf val -> pure (inferLiteral val)
  -- infer for Tensor and TensorOf should never be called
  -- since it's not part of surface language..
  ---
  -- implement them anyway:
  Tensor (TensorDescriptor base _dims) -> do
    void $ inferUniverse' ctxt (Inf base)
    pure (VSet 0)
  TensorOf (TensorDescriptor base dims) arr -> do
    baseType <- infer' ctxt base
    mapM_ (check' ctxt vint) dims
    vdims <- mapM (eval' ctxt) dims
    mapM_ (check' ctxt baseType) arr
    pure . VTensor $ VTensorDescriptor baseType vdims
  f :$ x ->
    infer' ctxt f >>= \case
      VPi _ t t' -> do
        check' ctxt t x
        x' <- eval' ctxt x
        t' x'
      t -> throwSemanticError (IllegalApplication t)

-- . traceShowWith ("infer'"::String,ctxt,)

inferLiteral :: LitOf -> Type
inferLiteral =
  VLit . \case
    BoolOf{} -> Bool
    IntOf{} -> Int

inferUniverse' :: [Type] -> Term Check -> Spreadsheet Int
inferUniverse' ctxt = \case
  Inf e -> do
    t <- infer' ctxt e
    case quote t of
      Inf (Set k) -> pure k
      Inf (Bound i) -> inferUniverse' ctxt (quote (ctxt !! i))
      Inf (Free name) -> do
        Env{globals} <- ask
        case Map.lookup name globals of
          Just Decl{declType = VSet k} -> pure k
          _ -> throwSemanticError (UnknownIdentifier name)
      Inf Tensor{} -> pure 0
      ty -> throwSemanticError (ExpectedSet ty)
  ty -> throwSemanticError (ExpectedSet ty)

-- * Evaluation

eval' :: [Value] -> Term m -> Spreadsheet Value
eval' ctxt = \case
  Inf x -> eval' ctxt x
  Lam p body -> pure . VLam p $ \v -> eval' (v : ctxt) body
  Ann x _ty -> eval' ctxt x
  Set k -> pure (VSet k)
  Pi p dom range -> do
    dom' <- eval' ctxt dom
    pure $ VPi p dom' (\v -> eval' (v : ctxt) range)
  Bound i -> pure (ctxt !! i)
  Ref sheetId _ cr@(start, end)
    | start == end -> fetchValue (sheetId, start)
    | otherwise -> do
        let addrs = cellRangeAddrs cr
        vtd@(VTensorDescriptor _ vdims) <- (`cellRangeDescriptor'` cr) <$> fetchType (sheetId, start)
        vdims' <- evalShape vdims
        VTensorOf vtd . Array.fromList vdims'
          <$> concatMapM (\ca -> flattenTensor <$> fetchValue (sheetId, ca)) addrs
  Free name -> do
    Env{globals} <- ask
    case Map.lookup name globals of
      Just Decl{declValue = Just v} -> pure v
      _ -> pure (vfree name)
  Lit lit -> pure (VLit lit)
  LitOf val -> pure (VLitOf val)
  Tensor td -> uncurry vtensor <$> evalTensor' ctxt td
  TensorOf td arr ->
    uncurry vtensorOf <$> evalTensor' ctxt td <*> mapM (eval' ctxt) arr
  f :$ x -> do
    f' <- eval' ctxt f
    x' <- eval' ctxt x
    vapp f' x'

evalTensor' :: [Value] -> TensorDescriptor -> Spreadsheet (Value, [Value])
evalTensor' ctxt (TensorDescriptor base dims) = (,) <$> eval' ctxt base <*> mapM (eval' ctxt) dims

evalShape :: [Value] -> Spreadsheet ShapeL
evalShape = mapM evalInt
 where
  evalInt = \case
    VLitOf (IntOf i) -> pure i
    v -> throwSemanticError (ExpectedInt v)

-- auxiliary functions
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

flattenTensor :: Value -> [Value]
flattenTensor = \case
  VTensorOf _ arr -> Array.toList arr
  v -> [v]

{- language semantics -}

instance Recalc (Term Infer) where
  type EnvOf (Term Infer) = Env
  type ErrorOf (Term Infer) = SemanticError
  type ValueOf (Term Infer) = Value

  parseCell = \case
    CellValue -> valueP
    CellFormula -> formulaP

  depsOf = deps'

  infer = infer' []
  eval = eval' []

{- prettier errors & sheet-defined functions -}

render :: Doc ann -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

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
    IllegalArrayType locs ->
      annotation
        "Unexpected Type in Array"
        [ "Expected ‘int’ but got "
        , hsep . punctuate comma
            $ map (\(ca, ty) -> "‘" <> pretty ty <> "’ at" <+> pretty (showExcel26 ca)) locs
        ]
    UnknownIdentifier name ->
      annotation
        "Unknown Identifier"
        [ "Unknown identifier ‘"
        , pretty name
        , "’."
        ]
    UnknownError message ->
      annotation
        "Error"
        [ "An unknown error occurred: "
        , pretty message
        ]
   where
    annotation title docs = Annotation title (render (cat docs))

{- Prelude -}

prelude :: Globals
prelude =
  Map.fromList
    [ ("not", Decl (vbool `vfun` vbool) (Just notImpl))
    , ("and", Decl (vbool `vfun` vbool `vfun` vbool) (Just andImpl))
    , ("or", Decl (vbool `vfun` vbool `vfun` vbool) (Just orImpl))
    , ("mmult", Decl mmultT (Just mmultImpl))
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

  mmultT = vpis [("m", vint), ("n", vint), ("k", vint)] $ \[m, n, k] ->
    vtensor vint [m, n] `vfun` vtensor vint [n, k] `vfun` vtensor vint [m, k]

  mmultImpl = vlams ["m", "n", "k", "u", "v"] $ \[m, n, k, u, v] ->
    -- case (u, v) of
    --   (VTensorOf{}, VTensorOf{}) -> error "see static building issues on branch dev/hmatrix"
    VNeutral $ NApp (NApp (NApp (NApp (NApp (NFree "mmult") m) n) k) u) v

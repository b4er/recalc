{-|
Module      : Recalc.Syntax.Fixity
Description : Fixity declarations for operators (prefix, infix, and postfix)
              that combine pretty-printing and parsing.

This module provides builds pretty-printing utility functions and a parser
for terms with operators.

=== Example Usage

@
import Recalc.Syntax.Fixity

data Op2 = Mul | Div | Add

data Term = Num Int | Op Op2 Term Term | ...

myOps :: [[Fixity Void Op2]]
myOps =
  [ [Infixl "*" Mul, Infixl "/" Div]
  , [Infixl "+" Add]
  ]

prettyTerm = pp 0
  where
    pp prec =
      let (left, right, ppOp1, ppRightOp2, ppLeftOp2, ppOp2) = ppHelpers myOps prec
       in \case
        Num i -> pretty i
        Op op x y@(OP op' _ _)
          | op == op', right op -> ppRightOp2 pp op x y
        Op op x y
          | left op -> ppLeftOp2 pp op x y
          | otherwise -> ppOp2 pp op x y
        ...

termP :: Parser Term
termP = makeOpsParser
  (void . symbol)              -- parse operator from operator symbol
  (numP <|> parens termP)      -- parse atomic terms and parenthesized one recursively
  (error "no unary operators") -- skip (since there are no Prefix, or Postfix operators)
  Op                           -- build expression from binary operator
@
-}
module Recalc.Syntax.Fixity
  ( Fixity (Prefix, Postfix, Infixl, Infixr, Infix)
  , makeOpsParser
  , ppHelpers
  , sym
  ) where

import Control.Monad (MonadPlus)
import Control.Monad.Combinators.Expr (makeExprParser)
import Control.Monad.Combinators.Expr qualified as Expr
import Data.List qualified as List
import Data.Text qualified as Text
import Prettyprinter hiding (parens)
import Prettyprinter qualified as PP

-- | Fixity declaration (unary prefix and postfix operators, as well as
-- left-associative, right-associative and non-associative infix operators).
data Fixity op1 op2
  = Prefix {sym :: String, op1 :: op1}
  | Postfix {sym :: String, op1 :: op1}
  | Infixl {sym :: String, op2 :: op2}
  | Infixr {sym :: String, op2 :: op2}
  | Infix {sym :: String, op2 :: op2}

isUnary, isBinary :: Fixity op1 op2 -> Bool
isUnary = \case Prefix{} -> True; Postfix{} -> True; _ -> False
isBinary = not . isUnary

-- | construct a parser from operator table
makeOpsParser
  :: MonadPlus f
  => (String -> f ())
  -- ^ operator symbol parser
  -> f a
  -- ^ "atomic" term parser (must not accept ε)
  -> (op1 -> a -> a)
  -- ^ constructor for unary operators
  -> (op2 -> a -> a -> a)
  -- ^ constructor for binary operators
  -> [[Fixity op1 op2]]
  -- ^ fixity declaration (precedence list)
  -> f a
makeOpsParser symbol p unary binary opss =
  makeExprParser p $ (`map` opss) $ \ops -> (`map` ops) $ \case
    Prefix s op -> Expr.Prefix (unary op <$ symbol s)
    Postfix s op -> Expr.Postfix (unary op <$ symbol s)
    Infixl s op -> Expr.InfixL (binary op <$ symbol s)
    Infixr s op -> Expr.InfixR (binary op <$ symbol s)
    Infix s op -> Expr.InfixN (binary op <$ symbol s)

-- | pretty-print terms minimally respecting specified operator precedences and
-- associativities
ppHelpers
  :: (Eq op1, Eq op2)
  => [[Fixity op1 op2]]
  -> Int
  -> ( op2 -> Bool
     , op2 -> Bool
     , (Int -> t1 -> Doc ann) -> op1 -> t1 -> Doc ann
     , (Int -> t2 -> Doc ann) -> op2 -> t2 -> t2 -> Doc ann
     , (Int -> t3 -> Doc ann) -> op2 -> t3 -> t3 -> Doc ann
     , (Int -> t4 -> Doc ann) -> op2 -> t4 -> t4 -> Doc ann
     )
ppHelpers opss prec = (leftAssoc, rightAssoc, ppOp1, ppRightOp2, ppLeftOp2, ppOp2)
 where
  -- fixme: build Map op (sym, l, r ...) instead
  symOf1 op = maybe mempty (pretty . Text.pack . sym)
    $ (`List.find` concat opss)
    $ \f -> isUnary f && op1 f == op
  symOf2 op = maybe mempty (pretty . Text.pack . sym)
    $ (`List.find` concat opss)
    $ \f -> isBinary f && op2 f == op

  leftAssoc op = any (any (\case Infixl _ op' -> op == op'; _ -> False)) opss
  rightAssoc op = any (any (\case Infixr _ op' -> op == op'; _ -> False)) opss

  parens1 op = if prec >= precOf1 op then PP.parens else id
  parens2 op = if prec > precOf2 op then PP.parens else id

  precOf1 op =
    maybe 0 succ
      $ (`List.findIndex` reverse opss)
      $ any ((op ==) . op1) . filter isUnary
  precOf2 op =
    maybe 0 succ
      $ (`List.findIndex` reverse opss)
      $ any ((op ==) . op2) . filter isBinary

  ppOp1 pp op x = parens1 op . prefixOrSuffix op $ pp (precOf1 op) x

  prefixOrSuffix op
    | any (any (\case Postfix _ op' -> op == op'; _ -> False)) opss = (<> symOf1 op)
    | otherwise = (symOf1 op <>)

  ppRightOp2 pp op x y = parens2 op $ pp (precOf2 op) x <+> symOf2 op <+> pp (precOf2 op) y
  ppLeftOp2 pp op x y = parens2 op $ pp (precOf2 op) x <+> symOf2 op <+> pp (precOf2 op + 1) y
  ppOp2 pp op x y = parens2 op $ pp (precOf2 op + 1) x <+> symOf2 op <+> pp (precOf2 op + 1) y

{-# LANGUAGE OverloadedStrings #-}

module Recalc.Syntax.FixitySpec where

import Control.Monad (void, when, zipWithM_)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
import Test.Hspec (Expectation, Spec, describe, it)
import Text.Megaparsec ((<|>))

import Recalc.Engine (SheetId)
import Recalc.Syntax.Fixity
import Recalc.Syntax.Parser
import Recalc.Syntax.Test

data OP1 = TR | HMM | NEG | NOT deriving (Eq, Show)

data OP = DOT | ADD | MUL | DIV | EXP
  deriving (Eq, Show)

data E = N Int | OP1 OP1 E | OP OP E E
  deriving (Eq, Show)

myOps :: [[Fixity OP1 OP]]
myOps =
  [ [Prefix "~" NOT, Prefix "-" NEG]
  , [Prefix "?" HMM]
  , [Infixr "^" EXP]
  , [Infixl "*" MUL, Infixl "/" DIV]
  , [Infixl "+" ADD]
  , [Infix "." DOT]
  , [Postfix "'" TR]
  ]

renderE :: E -> String
renderE = renderString . layoutPretty defaultLayoutOptions . pp 0
 where
  pp prec =
    let (left, right, ppOp1, ppRightOp2, ppLeftOp2, ppOp2) = ppHelpers myOps prec
    in  \case
          N i -> pretty i
          OP1 op x -> ppOp1 pp op x
          OP op x y@(OP op' _ _)
            | op == op', right op -> ppRightOp2 pp op x y
          OP op x y
            | left op -> ppLeftOp2 pp op x y
            | otherwise -> ppOp2 pp op x y

e :: Parser E
e =
  makeOpsParser
    (void . symbol)
    ((N <$> decimal) <|> parens e)
    OP1
    OP
    myOps

spec :: Spec
spec = do
  describe "makeOpsParser"
    . manyIt "parses" succeeds
    $ \(input, x, _) -> parseTest sheetId e input x

  describe "makeOpsParser/ppHelpers"
    . manyIt "re-encodes" succeeds
    $ \(input, _, check) ->
      when check $ parseTest sheetId (renderE <$> e) input input

  describe "makeOpsParser" . manyIt "rejects" rejects $ failTest sheetId e
 where
  -- [(string input, expected, True <--> test render(parse(input)) == input)]
  succeeds =
    -- prefix
    [ ("-1", OP1 NEG (N 1), True)
    , ("- 1", OP1 NEG (N 1), False)
    , ("~-1", OP1 NOT (N (-1)), True)
    , ("-(1 + 2)", OP1 NEG (OP ADD (N 1) (N 2)), True)
    , ("-(-1)", OP1 NEG (OP1 NEG (N 1)), True)
    , ("-(~1)", OP1 NEG (OP1 NOT (N 1)), True)
    , ("?-1", OP1 HMM (OP1 NEG (N 1)), True)
    , -- postfix
      ("1'", OP1 TR (N 1), True)
    , ("(1)'", OP1 TR (N 1), False)
    , ("(1')'", OP1 TR (OP1 TR (N 1)), True)
    , ("1 . 2'", OP1 TR (OP DOT (N 1) (N 2)), True)
    , -- binary
      ("1 + 2", OP ADD (N 1) (N 2), True)
    , ("1 + 2 * 3", OP ADD (N 1) (OP MUL (N 2) (N 3)), True)
    , ("1 / 2 / 3", OP DIV (OP DIV (N 1) (N 2)) (N 3), True)
    , ("(1 + 2) + 3", OP ADD (OP ADD (N 1) (N 2)) (N 3), False) -- parens are superfluous
    , ("1 + 2 + 3", OP ADD (OP ADD (N 1) (N 2)) (N 3), True)
    , ("1 + (2 + 3)", OP ADD (N 1) (OP ADD (N 2) (N 3)), True)
    , ("1 ^ (2 ^ 3)", OP EXP (N 1) (OP EXP (N 2) (N 3)), False) -- parens are superfluous
    , ("1 ^ 2 ^ 3", OP EXP (N 1) (OP EXP (N 2) (N 3)), True)
    , ("(1 ^ 2) ^ 3", OP EXP (OP EXP (N 1) (N 2)) (N 3), True)
    , ("1 * 2 / 3", OP DIV (OP MUL (N 1) (N 2)) (N 3), True)
    , ("(1 * 2) / 3", OP DIV (OP MUL (N 1) (N 2)) (N 3), False)
    , ("1 . (2 . 3)", OP DOT (N 1) (OP DOT (N 2) (N 3)), True)
    , ("(1 . 2) . 3", OP DOT (OP DOT (N 1) (N 2)) (N 3), True)
    , -- nested

      ( "1 ^ ((2 ^ 3) ^ 4) ^ 5"
      , OP EXP (N 1) (OP EXP (OP EXP (OP EXP (N 2) (N 3)) (N 4)) (N 5))
      , True
      )
    ]

  rejects =
    [ ""
    , "-~1"
    , "-?1"
    , "1''"
    , "1 . 2 . 3"
    ]

-- | test a list of examples, enumerating them 1, 2, ..
manyIt :: String -> [a] -> (a -> Expectation) -> Spec
manyIt verb cases prop =
  zipWithM_ (\ex i -> it (message i) (prop ex)) cases [1 :: Int ..]
 where
  message i = verb <> " example-" <> show i

sheetId :: SheetId
sheetId = undefined

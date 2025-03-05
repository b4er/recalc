{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recalc.EngineSpec where

import Control.Monad (join, void)
import Control.Monad.Reader
import Data.Char (isAlphaNum)
import Data.List (foldl', sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Void (Void)
import Network.URI (parseURI)
import Prettyprinter hiding (column)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Recalc.Engine hiding (newEngineState, recalc)
import Recalc.Repl (newEngineState, recalc)

-- | run all inputs in sequence
run' :: [[(CellAddr, String)]] -> Results Void Term (Maybe Int)
run' = fst . foldl' alg (error "no inputs", newEngineState ())
 where
  alg (_, st) inputs = recalc @Term inputs st

-- | run inputs in sequence and prepare "Results" for hspec
run :: [[(CellAddr, String)]] -> [(CellAddr, Int)]
run =
  -- make sure the results are sorted
  sortOn fst
    -- only keep (ca, val), drop all errors
    . mapMaybe (\((_, ca), x) -> (ca,) <$> join (snd =<< snd =<< snd =<< cell x))
    . run'

-- | the specification checks a few (single-document, single-sheet) examples
-- to make sure results are expected (incl. making sure dependencies are resolved)
spec :: Spec
spec = do
  describe "sheet arithmetics" $ do
    it "reacts on new values entered"
      $ run [[((2, 2), "12"), ((0, 0), "=1+2"), ((1, 1), "=0")]]
      `shouldBe` [ ((0, 0), 3)
                 , ((1, 1), 0)
                 , ((2, 2), 12)
                 ]

    it "computes handles simple refs" $ do
      run [[((1, 0), "1")], [((2, 2), "=A2")]] `shouldBe` [((2, 2), 1)]
      run [[((0, 0), "=1+1")], [((2, 2), "=A1")]] `shouldBe` [((2, 2), 2)]
      run [[((5, 3), "8")], [((2, 2), "=D6")]] `shouldBe` [((2, 2), 8)]

    it "computes sum of previously entered values"
      $ run
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        ]
      `shouldBe` [((2, 2), 18)]

    it "updates sum when value is changed (1)"
      $ run
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 1), "0")]
        ]
      `shouldBe` [((0, 1), 0), ((2, 2), 6)]

    it "updates sum when value is changed (2)"
      $ run
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 0), "101")]
        , [((0, 1), "0")]
        , [((1, 0), "0")]
        , [((0, 0), "0")]
        , [((1, 1), "0")]
        ]
      `shouldBe` [((1, 1), 0), ((2, 2), 0)]

    it "sum behaves with negative numbers"
      $ run
        [ [((0, 1), "12"), ((1, 0), "-2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 1), "0")]
        ]
      `shouldBe` [((0, 1), 0), ((2, 2), 2)]

{- AST for a simple spreadsheet language (numbers, addition, references, sums of ranges) -}

-- | when referencing a cell, the uri and sheetName can be omited
-- (defaults to "current" sheet)
data SheetRef
  = Explicit {sheetId :: SheetId}
  | Implicit {sheetId :: SheetId}
  deriving (Show)

instance Pretty SheetRef where
  pretty = \case
    Explicit (uri, sheetName) -> pretty (show uri ++ "#" ++ Text.unpack sheetName)
    _ -> ""

-- | a simple term language
data Term
  = Num Int
  | Add Term Term
  | Ref SheetRef CellAddr
  | Sum SheetRef CellRange
  deriving (Show)

instance Pretty Term where
  pretty = \case
    Num i -> pretty i
    Add x y -> pretty x <+> "+" <+> pretty y
    Ref _ ca -> pretty (showExcel26 ca)
    Sum _ (begin, end) ->
      "sum"
        <> parens (pretty $ showExcel26 begin <> ":" <> showExcel26 end)

{- Parsing -}

type Parser = ReaderT SheetId (Parsec Void String)

-- | simple parser, does not deal with lexing (whitespaces might break things)
formulaP :: Parser Term
formulaP = char '=' *> termP
 where
  termP = do
    t <- choice [valueP, try sumP, refP]
    option t (Add t <$> (char '+' *> termP))

  refP = Ref <$> sheetIdP <*> cellAddrP
  sumP = do
    void (string "SUM")
    between (char '(') (char ')')
      $ Sum <$> sheetIdP <*> cellRangeP

  sheetIdP =
    choice
      [ (<* char '!') . between (char '[') (char ']') $ do
          let legalChar = satisfy (\c -> isAlphaNum c || c `elem` ("-./~" :: String))
          path <- many1 legalChar
          uri <- maybe (fail "invalid URI") pure (parseURI ("file://" ++ path))
          sheetName <- char '#' *> many1 legalChar
          pure $ Explicit (uri, Text.pack sheetName)
      , asks Implicit
      ]

  cellAddrP = do
    colStr <- many1 upperChar
    rowDigits <- decimal
    maybe (fail "Invalid cell reference") pure
      . readExcel
      $ Text.pack (colStr <> show @Int rowDigits)

  cellRangeP = (,) <$> cellAddrP <*> (char ':' *> cellAddrP)

  many1 p = (:) <$> p <*> many p

valueP :: Parser Term
valueP = Num <$> (option id (negate <$ char '-') <*> decimal)

{- Language Semantics -}

instance Recalc Term where
  -- there is no additional context for typing/evaluation
  type EnvOf Term = ()

  -- there will not be errors
  type ErrorOf Term = Void

  -- Nothing as type, Just as values
  type ValueOf Term = Maybe Int

  parseCell :: CellType -> Parser Term
  parseCell = \case
    CellFormula -> formulaP
    CellValue -> valueP

  -- dependencies of a term
  depsOf :: Term -> Set CellRangeRef
  depsOf = \case
    Add x y -> foldMap depsOf [x, y]
    Ref sheetRef ca -> Set.singleton (sheetId sheetRef, (ca, ca))
    Sum sheetRef cr -> Set.singleton (sheetId sheetRef, cr)
    _ -> Set.empty

  -- everything is untyped
  infer :: Term -> Fetch () Void (Maybe Int) (Maybe Int)
  infer _ = pure Nothing

  -- evaluation is straight-forward
  eval :: Term -> Fetch () Void (Maybe Int) (Maybe Int)
  eval = \case
    Num n -> pure (Just n)
    Add x y -> liftA2 (+) <$> eval x <*> eval y
    Ref sheetRef ref -> fetchValue (sheetId sheetRef, ref)
    Sum sheetRef (start, end) -> do
      Just . sum . catMaybes
        <$> sequence
          [ fetchValue (sheetId sheetRef, (i, j))
          | i <- [row start .. row end]
          , j <- [column start .. column end]
          ]

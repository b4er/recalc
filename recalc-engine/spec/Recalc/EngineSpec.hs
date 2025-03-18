{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.EngineSpec
Description : Tests for the recalculation engine (only single-sheet so far).

Implements a simple calculator language and uses it instantiate
the 'Recalc' interface. Use this language to validate the
spreadsheet engine.
-}
module Recalc.EngineSpec where

import Control.Monad (void)
import Control.Monad.Reader
import Data.Char (isAlphaNum)
import Data.List (foldl', sortOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
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

-- | use @""@ for cell deletion and others for new input
instance IsString (Maybe String) where
  fromString = \case "" -> Nothing; str -> Just str

type Result = Either (FetchError ())

-- | evalSheet all inputs in sequence
run :: [[(CellAddr, Maybe String)]] -> (ResultsOf Term, EngineStateOf Term)
run = foldl' alg (error "no inputs", newEngineState ())
 where
  alg (_, st) inputs = recalc @Term inputs st

-- | evalSheet inputs in sequence and prepare "Results" for hspec
evalSheet :: [[(CellAddr, Maybe String)]] -> [(CellAddr, Int)]
evalSheet =
  -- make sure the results are sorted
  sortOn fst
    -- only keep (ca, val), drop all errors
    . mapMaybe (\((_, ca), c) -> fmap (ca,) . snd =<< snd =<< snd =<< cell c)
    . fst
    . run -- drop state

evalSheetKeepErrors :: [[(CellAddr, Maybe String)]] -> [(CellAddr, Result Int)]
evalSheetKeepErrors =
  sortOn fst
    . map (\((_, ca), x) -> (ca, unpack x))
    . fst
    . run
 where
  unpack :: Cell () Term () Int -> Result Int
  unpack c = case (cell c, cellError c) of
    (_, Just err) -> Left err
    (Just (_, Just (_, Just (_, Just q))), _) -> Right q
    _ -> Left RefError

-- | the specification checks a few (single-document, single-sheet) examples
-- to make sure results are expected (incl. making sure dependencies are resolved)
spec :: Spec
spec = do
  describe "sheet arithmetics" $ do
    it "reacts on new values entered"
      $ evalSheet [[((2, 2), "12"), ((0, 0), "=1+2"), ((1, 1), "=0")]]
      `shouldBe` [ ((0, 0), 3)
                 , ((1, 1), 0)
                 , ((2, 2), 12)
                 ]

    it "computes handles simple refs" $ do
      evalSheet [[((1, 0), "1")], [((2, 2), "=A2")]] `shouldBe` [((2, 2), 1)]
      evalSheet [[((0, 0), "=1+1")], [((2, 2), "=A1")]] `shouldBe` [((2, 2), 2)]
      evalSheet [[((5, 3), "8")], [((2, 2), "=D6")]] `shouldBe` [((2, 2), 8)]

    it "computes sum of previously entered values"
      $ evalSheet
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        ]
      `shouldBe` [((2, 2), 18)]

    it "updates sum when value is changed (1)"
      $ evalSheet
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 1), "0")]
        ]
      `shouldBe` [((0, 1), 0), ((2, 2), 6)]

    it "updates sum when value is changed (2)"
      $ evalSheet
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
      $ evalSheet
        [ [((0, 1), "12"), ((1, 0), "-2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 1), "0")]
        ]
      `shouldBe` [((0, 1), 0), ((2, 2), 2)]

    it "gives a reference error when a referenced cell is deleted"
      $ evalSheetKeepErrors
        [ [((0, 1), Just "12")]
        , [((2, 2), Just "=B1")]
        , [((0, 1), Nothing)]
        ]
      `shouldBe` [((2, 2), Left RefError)]

    it "gives a reference error when a referenced cell is deleted"
      $ evalSheet
        [ [((0, 1), Just "12")]
        , [((2, 2), Just "=B1")]
        , [((0, 1), Nothing)]
        , [((0, 1), Just "42")]
        ]
      `shouldBe` [((0, 1), 42), ((2, 2), 42)]

  describe "elaboration" $ do
    it "passes funky test (elaborate error term to 42)"
      $ evalSheetKeepErrors [[((0, 0), "=funky")]]
      `shouldBe` [((0, 0), Right 42)]

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
  | Funky
  deriving (Show)

instance Pretty Term where
  pretty = \case
    Num i -> pretty i
    Add x y -> pretty x <+> "+" <+> pretty y
    Ref _ ca -> pretty (showExcel26 ca)
    Sum _ (begin, end) ->
      "sum"
        <> parens (pretty $ showExcel26 begin <> ":" <> showExcel26 end)
    Funky -> "funky"

{- Parsing -}

type Parser = ReaderT SheetId (Parsec Void String)

-- | simple parser, does not deal with lexing (whitespaces might break things)
formulaP :: Parser Term
formulaP = char '=' *> termP
 where
  termP = do
    t <- choice [valueP, funkyP, try sumP, refP]
    option t (Add t <$> (char '+' *> termP))

  refP = Ref <$> sheetIdP <*> cellAddrP

  funkyP = Funky <$ string "funky"

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
  -- \| there is no additional context for typing/evaluation
  type EnvOf Term = ()

  -- \| there will not be errors
  type ErrorOf Term = ()

  -- \| all types are int
  type TypeOf Term = ()

  -- \| values are int
  type ValueOf Term = Int

  parseCell :: CellType -> Parser Term
  parseCell = \case
    CellFormula -> formulaP
    CellValue -> valueP

  -- \| dependencies of a term
  depsOf :: Term -> Set CellRangeRef
  depsOf = \case
    Add x y -> foldMap depsOf [x, y]
    Ref sheetRef ca -> Set.singleton (sheetId sheetRef, (ca, ca))
    Sum sheetRef cr -> Set.singleton (sheetId sheetRef, cr)
    _ -> Set.empty

  -- \| everything is untyped, however we elaborate `Funky ~> 42`
  -- such that we can validate whether term elaboration works:
  inferElaborate :: Term -> FetchOf Term ((), Term)
  inferElaborate =
    pure . ((),) . \case
      Funky -> Num 42
      t -> t

  -- \| evaluation is straight-forward
  -- (`Funky` should not be called, see "inferElaborate"):
  eval :: Term -> FetchOf Term Int
  eval = \case
    Num n -> pure n
    Add x y -> (+) <$> eval x <*> eval y
    Ref sheetRef ref -> fetchValue (sheetId sheetRef, ref)
    Sum sheetRef (start, end) ->
      sum
        <$> sequence
          [ fetchValue (sheetId sheetRef, (i, j))
          | i <- [row start .. row end]
          , j <- [column start .. column end]
          ]
    Funky -> throwSemanticError ()

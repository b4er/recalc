{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recalc.EngineSpec where

import Control.Arrow (Arrow (first))
import Control.Monad (foldM)
import Data.Char (isDigit, isUpper, ord, toLower)
import Data.List (sortOn)
import Data.Maybe (fromJust, catMaybes)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Void (Void)
import Network.URI (parseURI)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.Megaparsec
import Text.Megaparsec.Char

import Recalc.Engine

data Term = Num Int | Add Term Term | Ref CellAddr | Sum CellRange
 deriving Show

spec :: Spec
spec =
  describe "sheet arithmetics" $ do
    it "reacts on new values entered"
      $ runSheet [[((2, 2), "12"), ((0, 0), "=1+2"), ((1, 1), "=0")]]
        `shouldChangeM` [ ((0, 0), ok 3)
                        , ((1, 1), ok 0)
                        , ((2, 2), ok 12)
                        ]

    it "computes handles simple refs" $ do
      runSheet [[((1, 0), "1")], [((2, 2), "=A2")]] `shouldChangeM` [((2, 2), ok 1)]
      runSheet [[((0, 0), "=1+1")], [((2, 2), "=A1")]] `shouldChangeM` [((2, 2), ok 2)]
      runSheet [[((5, 3), "8")], [((2, 2), "=D6")]] `shouldChangeM` [((2, 2), ok 8)]

    it "computes sum of previously entered values"
      $ runSheet
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        ]
        `shouldChangeM` [((2, 2), ok 18)]

    it "updates sum when value is changed (1)"
      $ runSheet
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 1), "0")]
        ]
        `shouldChangeM` [((0, 1), ok 0), ((2, 2), ok 6)]

    it "updates sum when value is changed (2)"
      $ runSheet
        [ [((0, 1), "12"), ((1, 0), "2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 0), "101")]
        , [((0, 1), "0")]
        , [((1, 0), "0")]
        , [((0, 0), "0")]
        , [((1, 1), "0")]
        ]
        `shouldChangeM` [((1, 1), ok 0), ((2, 2), ok 0)]

    it "sum behaves with negative numbers"
      $ runSheet
        [ [((0, 1), "12"), ((1, 0), "-2"), ((0, 0), "=1+2"), ((1, 1), "=1")]
        , [((2, 2), "=SUM(A1:B2)")]
        , [((0, 1), "0")]
        ]
        `shouldChangeM` [((0, 1), ok 0), ((2, 2), ok 2)]

ok :: Int -> Err Value
ok = Right . Just

type Parser = Parsec Void String

termP :: Parser Term
termP = space *> term <* eof
  where
    term = try addP <|> atomP

    addP = Add <$> (atomP <* char '+') <*> term

    atomP = try sumP <|> Ref <$> refP <|> numP

    numP = Num . read <$> digits
    sumP = Sum <$> (string "SUM" *> between (char '(') (char ')') rangeP)

    rangeP = (,) <$> (refP <* char ':') <*> refP

    refP = do
      col <- someOf ['A'..'Z']
      row <- digits
      maybe (fail "Invalid cell reference") pure . readExcel $ Text.pack (col <> row)

    digits = (:) <$> oneOf' ['1'..'9'] <*> many (oneOf' ['0'..'9'])
        <|> string "0"

    oneOf' cs = satisfy (`elem` cs)
    someOf = some . oneOf'

-- | read an spreadsheet address of the form column-row
-- (columns are labelled "A..Z, AA..", and rows enumerated)
readExcel :: Text -> Maybe CellAddr
readExcel txt = go . (`Text.splitAt` txt) =<< alg 0 False (Text.unpack txt)
 where
  go (letters, digits)
    | j >= 0 = Just (j, readExcel26 letters - 1)
    | otherwise = Nothing
   where
    j = read (Text.unpack digits) - 1

  alg k b (c : cs)
    | not b, isUpper c = alg (k + 1) b cs
    | not b, isDigit c = alg k True cs
    | b, isDigit c = alg k True cs
    | otherwise = Nothing
  alg k True [] = Just k
  alg _ _ _ = Nothing

  readExcel26 :: Text -> Int
  readExcel26 = Text.foldl' (\a c -> 26 * a + ord (toLower c) - 96) 0


type Value = Maybe Int

-- instance Pretty Term where
--   pretty = \case
--     Num n -> pretty n
--     Ref r -> pretty (showExcel26 r)
--     Add x y -> pretty x <+> "+" <+> pretty y
--     Sum (s,t) -> "SUM(" <> pretty (showExcel26 s) <> ":" <> pretty (showExcel26 t) <> ")"

instance Language Term where
  type EnvOf Term = SheetId
  newEnv = id

  type ErrorOf Term = ()

  deps = \case
    Num{} -> mempty
    Ref r -> Set.singleton (r,r)
    Add x y -> foldMap deps [x,y]
    Sum r -> Set.singleton r

  infer _ = pure (Just 42) -- just a dummy "type"
  inferValue _ = pure (Just 42) -- just a dummy "type"

  type ValueOf Term = Maybe Int
  eval = \case
    Num n -> pure (Just n)
    Add x y -> liftA2 (+) <$> eval x <*> eval y
    Ref ref -> do (uri, sheetId) <- getEnv; fetchValue uri sheetId ref
    Sum (x, y) -> do
      (uri, sheetId) <- getEnv
      Just . sum . catMaybes <$> sequence
        [fetchValue uri sheetId (i,j) | i <- [fst x..fst y], j <- [snd x..snd y]]

instance Input String where
  type TermOf String = Term
  -- | parse strings starting with @"="@ as term, else treat as value
  exprOrValueOf fileName ('=':input) = Just (parse (Left <$> termP <* eof) fileName input)
  exprOrValueOf _ input = Just (Right (Right (Just (read @Int input))))

  type MetaOf String = ()
  metaOf _ = ()

type Err = Either (FetchError ())

runSheet :: [[(CellAddr, String)]] -> IO [(CellAddr, Err (Maybe Int))]
runSheet inputss = fmap (concatMap (map (first snd)) . fst) . (`runEngineT` newEngineState) $ do
    modifyDocs (insertSheet sheetId1 () . insertDocument uri1 ())
    foldM (const go) [] inputss
  where
    go :: [(CellAddr, String)] -> EngineT () () () Term IO [[((SheetId, CellAddr), Err (Maybe Int))]]
    go inputs = either (\x -> fail ("cycle: " ++ show x)) pure <$>
      recompute @String sheetId1 (snd (validateCells @String sheetId1 inputs))

uri1 :: URI
uri1 = fromJust (parseURI "file:///test1")

sheetId1 :: SheetId
sheetId1 = (uri1, "sheet arithmetics")

shouldChangeM
  :: (Eq a, Show a)
  => IO [(CellAddr, a)]
  -> [(CellAddr, a)]
  -> Expectation
shouldChangeM mx y = do x <- mx; sortOn fst x `shouldBe` sortOn fst y

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Recalc.Syntax.Parser
Description : Parser definitions for the term language.

Simple Megaparsec-based parser for the Term language.
-}
module Recalc.Syntax.Parser
  ( -- * Term Parser
    Parser
  , termP

    -- * Exported as Testing Utilities
  , decimal
  , parens
  , readExcel
  , symbol
  ) where

import Control.Monad (void)
import Data.Char (isDigit, isUpper, ord, toLower)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

import Recalc.Engine (CellAddr)
import Recalc.Syntax.Term

-- | simple parser with no internal state
type Parser = Parsec Void String

-- | parse whitespace (disallow both line- and block comments).
spaces :: Parser ()
spaces = Lexer.space Char.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

symbol :: String -> Parser String
symbol = Lexer.symbol spaces

string :: String -> Parser String
string = lexeme . Char.string

-- | parse (signed) integers
decimal :: Parser Int
decimal =
  choice
    [ lexeme Lexer.decimal
    , negate <$> (symbol "-" *> lexeme Lexer.decimal)
    ]

braces, parens :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")

startChar, middleChar :: Parser Char
startChar = Char.letterChar
middleChar = Char.alphaNumChar

keywords :: [String]
keywords = ["Type", ":", "->", ".", "+"]

identifier :: Parser Text
identifier = Text.pack <$> (lexeme . try) (word >>= check)
 where
  word = (:) <$> startChar <*> many middleChar
  check x
    | x `elem` keywords = fail $ "keyword " ++ show x ++ " cannot be an identifier"
    | otherwise = pure x

stringLiteral :: Parser String
stringLiteral = between (Char.char '"') (Char.char '"') (many (try escaped <|> normalChar))
 where
  escaped = choice $ map (\(c, esc) -> c <$ string esc) specialChars
  normalChar = satisfy (`notElem` map fst specialChars)

  specialChars =
    [ (c, take 2 (tail (show [c])))
    | c <- "\"\\\a\b\f\n\r\t\v"
    ]

-- | read an spreadsheet address of the form column-row
-- (columns are labelled "A..Z, AA..", and rows enumerated)
readExcel :: Text -> Maybe CellAddr
readExcel txt = go . (`Text.splitAt` txt) =<< alg 0 False (Text.unpack txt)
 where
  go (letters, digits)
    | r >= 0 = Just (r, readExcel26 letters - 1)
    | otherwise = Nothing
   where
    r = read (Text.unpack digits) - 1

  alg k b (c : cs)
    | not b, isUpper c = alg (k + 1) b cs
    | not b, isDigit c = alg k True cs
    | b, isDigit c = alg k True cs
    | otherwise = Nothing
  alg k True [] = Just k
  alg _ _ _ = Nothing

  readExcel26 :: Text -> Int
  readExcel26 = Text.foldl' (\a c -> 26 * a + ord (toLower c) - 96) 0

-- | parse an inferrable term
termP :: Parser (Term Infer)
termP = resolve <$> termI

resolve :: Term m -> Term m
resolve = go []
 where
  go :: [Maybe Name] -> Term m -> Term m
  go env = \case
    Inf x -> Inf (go env x)
    Lam n x -> Lam n (go ((Global <$> n) : env) x)
    Ann e t -> Ann (go env e) (go env t)
    Univ k -> Univ k
    Pi n x y -> Pi n (go env x) (go ((Global <$> n) : env) y)
    Bound i -> Bound (i + length env)
    v@(Free n) -> maybe v Bound $ elemIndex (Just n) env
    x :$ y -> go env x :$ go env y

termI :: Parser (Term Infer)
termI =
  (do x <- top; maybe x (Ann (Inf x)) <$> optional (symbol ":" *> termI))
    <|> Ann
    <$> parens lambda
    <*> (symbol ":" *> termI)
 where
  top =
    ( do
        (n, t) <- parens ((,) <$> (identifier <* symbol ":") <*> termC)
        void (symbol "->")
        Pi (Just n) t <$> termC
    )
      <|> try
        ( do
            x <- ops
            optional (symbol "->" *> termC) >>= \case
              Nothing -> pure x
              Just y -> pure (Pi Nothing (Inf x) y)
        )
      <|> Pi Nothing
      <$> lambda
      <*> (symbol "->" *> termC)

  ops = app <|> parens termI

  app = foldl (:$) <$> atom <*> (parens (sepEndBy1 termC (symbol ",")) <|> pure [])

  atom =
    choice
      [ Free . Global <$> identifier
      , parens termI
      , Univ 0 <$ symbol "*"
      ]

termC, lambda :: Parser (Term Check)
termC = lambda <|> (Inf <$> termI)
lambda =
  lams
    <$> (symbol "\\" *> some binderP)
    <*> (symbol "->" *> termC)
 where
  lams vars body = foldr Lam body vars

  binderP = Just <$> identifier <|> Nothing <$ Char.string "_"

fn :: Term Infer -> Term Check -> Term Infer
fn t = Pi Nothing (Inf t)

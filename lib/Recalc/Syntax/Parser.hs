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
  , formulaP
  , valueP
  , termI
  , keyword

    -- * Exported as Testing Utilities
  , cellReferenceOrFree
  , decimal
  , parens
  , readExcel
  , symbol
  ) where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT, ask, asks)
import Data.Char (isPrint, toLower)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Network.URI (escapeURIString, isUnescapedInURI, parseURI)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

import Recalc.Engine
import Recalc.Syntax.Term

-- | simple parser keeps the "SheetId" as state
type Parser = ReaderT SheetId (Parsec Void String)

-- | parse whitespace (disallow both line- and block comments).
spaces :: Parser ()
spaces = Lexer.space Char.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

symbol :: String -> Parser String
symbol = Lexer.symbol spaces

-- string :: String -> Parser String
-- string = lexeme . Char.string

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

isKeyword :: String -> Bool
isKeyword x =
  map toLower x
    `elem` map (map toLower) ["bool", "false", "true", "int"]

word :: Parser String
word = (:) <$> startChar <*> many middleChar

identifier, keyword :: Parser CaseInsensitive
identifier = CaseInsensitive . Text.pack <$> (lexeme . try) (word >>= check)
 where
  check x
    | isKeyword x = fail $ "keyword ‘" ++ x ++ "’ cannot be an identifier"
    | otherwise = pure x
keyword = CaseInsensitive . Text.pack <$> (lexeme . try) (word >>= check)
 where
  check x
    | isKeyword x = pure x
    | otherwise = fail $ "‘" ++ x ++ "’ is not a keyword"

{-
stringLiteral :: Parser String
stringLiteral = between (Char.char '"') (Char.char '"') (many (try escaped <|> normalChar))
 where
  escaped = choice $ map (\(c, esc) -> c <$ string esc) specialChars
  normalChar = satisfy (`notElem` map fst specialChars)

  specialChars =
    [ (c, take 2 (tail (show [c])))
    | c <- "\"\\\a\b\f\n\r\t\v"
    ]
-}

{- Value Parsing -}

valueP :: Parser (Term Infer)
valueP =
  choice
    [ LitOf . IntOf <$> decimal
    , keywordTerm . originalText <$> keyword
    ]

{- Term Parsing -}

-- | parse an inferrable term
formulaP :: Parser (Term Infer)
formulaP = resolve <$> (spaces *> symbol "=" *> termI)

resolve :: Term m -> Term m
resolve = go []
 where
  go :: [Maybe Name] -> Term m -> Term m
  go env = \case
    Inf x -> Inf (go env x)
    Lam arg n x -> Lam arg n (go ((Global <$> n) : env) x)
    Ann e t -> Ann (go env e) (go env t)
    Set k -> Set k
    Pi arg n x y -> Pi arg n (go env x) (go ((Global <$> n) : env) y)
    Bound i -> Bound (i + length env)
    v@(Free n) -> maybe v Bound $ elemIndex (Just n) env
    ref@Ref{} -> ref
    Lit lit -> Lit lit
    LitOf val -> LitOf val
    Tensor td -> Tensor (goTensor env td)
    TensorOf td arr -> TensorOf (goTensor env td) (go env <$> arr)
    x `App` (arg, y) -> go env x `App` (arg, go env y)

  goTensor env (TensorDescriptor base vdims) =
    TensorDescriptor (go env base) vdims

termI :: Parser (Term Infer)
termI =
  (do x <- top; maybe x (Ann (Inf x)) <$> optional (symbol ":" *> termI))
    <|> Ann
    <$> parens lambda
    <*> (symbol ":" *> termI)
 where
  top =
    try
      ( do
          (n, t) <- parens ((,) <$> (identifier <* symbol ":") <*> termC)
          void (symbol "->")
          Pi EArg (Just n) t <$> termC
      )
      <|> try
        ( do
            x <- ops
            optional (symbol "->" *> termC) >>= \case
              Nothing -> pure x
              Just y -> pure (Pi EArg Nothing (Inf x) y)
        )
      <|> Pi EArg Nothing
      <$> lambda
      <*> (symbol "->" *> termC)

  ops = app <|> parens termI

  app = do
    t0 <- atom

    -- any ordering of application blocks (or none): {..} and (..)
    appBlocks <-
      many
        $ choice
          [ (eapp,) <$> parens (sepEndBy1 termC (symbol ","))
          , (iapp,) <$> braces (sepEndBy1 termC (symbol ","))
          ]
    -- build the application from it using the correct *plicity
    pure $ foldl (\acc (f, ts) -> foldl f acc ts) t0 appBlocks

  eapp f x = f `App` (EArg, x)
  iapp f x = f `App` (IArg, x)

  atom =
    choice
      [ cellReferenceOrFree
      , intOf <$> decimal
      , parens termI
      , Set 0 <$ symbol "*"
      ]

keywordTerm :: Text -> Term Infer
keywordTerm w
  | Text.toLower w == "bool" = Lit Bool
  | Text.toLower w == "false" = boolOf False
  | Text.toLower w == "true" = boolOf True
  | Text.toLower w == "int" = Lit Int
  | otherwise = Free (Global (CaseInsensitive w))

cellReferenceOrFree :: Parser (Term Infer)
cellReferenceOrFree = choice [noUri, withUri, quoted]
 where
  cellRef = lexeme $ do
    cr' <- word
    case readExcel (Text.pack cr') of
      Just cr -> pure cr
      _ -> fail $ "‘" ++ cr' ++ "’ is not a cell reference"

  range x y = (min x y, max x y)

  cellRange = do
    start <- cellRef
    range start <$> option start (symbol ":" *> cellRef)

  simpleWord =
    lexeme
      $ (:)
        <$> choice (Char.letterChar : map Char.char "_~")
        <*> many (choice (Char.alphaNumChar : map Char.char "._~"))

  escapedWord = many (try escaped <|> normalChar)
   where
    escaped = choice $ map (\(c, esc) -> c <$ Char.string esc) specialChars
    normalChar = satisfy $ \c -> isPrint c && c `notElem` map fst specialChars
    specialChars = [(c, '\\' : [c]) | c <- "'[]\\"]

  -- for example: @bool@, @sheet!A3@, @A3@, or @C12:A3@
  noUri = do
    (uri, sheetName) <- ask
    prefix <- Text.pack <$> lexeme simpleWord
    (symbol "!" *> (Ref (uri, prefix) SheetOnly <$> cellRange))
      <|> ( case readExcel prefix of
              Just start ->
                Ref (uri, sheetName) Unspecified . range start
                  <$> option start (symbol ":" *> cellRef)
              Nothing ->
                pure (keywordTerm prefix)
          )

  -- eg. @[file]data!A1@
  withUri = do
    uri <- checkUri =<< between (symbol "[") (symbol "]") simpleWord
    sheetName <- simpleWord
    Ref (uri, Text.pack sheetName) FullySpecified <$> (symbol "!" *> cellRange)

  -- eg. @'Sheet 1'!B3@, or @'[file name]sheet!A1'@
  quoted =
    uncurry Ref
      <$> lexeme (between (symbol "'") (symbol "'") quotedSheetId)
      <*> (symbol "!" *> cellRange)

  -- eg. @[file name]sheet@, or @Sheet 13@
  quotedSheetId = do
    currentUri <- asks fst
    (uri, spec) <-
      option
        (currentUri, SheetOnly)
        ((,FullySpecified) <$> between (Char.char '[') (Char.char ']') (escapedWord >>= checkUri))
    sheetId <- Text.pack <$> escapedWord
    pure ((uri, sheetId), spec)

  checkUri uri' =
    case parseURI (escapeURI uri') <|> parseURI ("file://" <> escapeURI uri') of
      Nothing -> fail $ "‘" ++ uri' ++ "’ is not a valid URI"
      Just uri -> pure uri

  escapeURI = escapeSpecialChars . escapeURIString isUnescapedInURI

  escapeSpecialChars =
    Text.unpack
      . Text.replace "'" "%27"
      . Text.replace "[" "%5B"
      . Text.replace "]" "%5D"
      . Text.replace "\\" "%5C"
      . Text.pack

termC, lambda :: Parser (Term Check)
termC = lambda <|> (Inf <$> termI)
lambda =
  lams
    <$> (symbol "\\" *> some binderP)
    <*> (symbol "->" *> termC)
 where
  lams vars body = foldr (Lam EArg) body vars

  binderP = Just <$> identifier <|> Nothing <$ Char.string "_"

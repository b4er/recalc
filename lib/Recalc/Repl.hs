{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Recalc.Repl
Description : Utility functions for experimenting in GHCi

Provides a few utilities to experiment with the core language
to examine outcomes of parsing, type checking and inference,
evaluation and sheet recomputation.
-}
module Recalc.Repl
  ( ExampleData (..)
  , Result
  , uri
  , parseFormula
  , parseValue
  , check
  , infer
  , eval

    -- * Engine
  , EngineState
  , newEngineState
  , runRecompute
  , evalRecompute

    -- * re-export
  , pretty
  ) where

import Control.Monad.Except (runExcept)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (bimap, first, second)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (URI, parseURI)
import Prettyprinter
import Text.Megaparsec (eof, parse)

-- (Value, valueP)

import Recalc.Engine (Input (termOrValueOf), Isn't (..))
import Recalc.Engine qualified as Engine
import Recalc.Engine.Core
import Recalc.Semantics
import Recalc.Server.Types (Nullable (..))
import Recalc.Syntax.Parser (Parser, formulaP)
import Recalc.Syntax.Term

uri :: URI
uri = fromJust (parseURI "file://repl.rc")

testId :: SheetId
testId = (uri, "Test")

type Result = Either (Engine.FetchError SemanticError)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = \case
  Left x -> Left (f x)
  Right y -> Right y

liftParsed :: Engine.Parsed a -> Result a
liftParsed = mapLeft Engine.InvalidFormula

parseFormula :: String -> Result (Term Infer)
parseFormula = liftParsed . parse' testId (0, 0) formulaP

parseValue :: String -> Result Value
parseValue = liftParsed . parse' testId (0, 0) valueP

parse' :: SheetId -> CellAddr -> Parser a -> String -> Engine.Parsed a
parse' sheetId ca p = parse (runReaderT p sheetId <* eof) (showExcel26 ca)

infer :: [(Name, Decl)] -> Term Infer -> Result Type
infer extra x = runFetch extra (infer' 0 x)

check :: [(Name, Decl)] -> Type -> Term Check -> Result ()
check extra x ty = runFetch extra (check' 0 x ty)

eval :: [(Name, Decl)] -> Term m -> Result Value
eval extra x = runFetch extra (eval' x)

runFetch :: [(Name, Decl)] -> Engine.Fetch (Term Infer) a -> Result a
runFetch extraGlobals fx = runExcept (runReaderT fx env')
 where
  env = Engine.newEnv @(Term Infer) testId
  env' = (fetch, env{globals = globals env <> Map.fromList extraGlobals})

  -- fetching is tested in the Engine spec
  fetch _ix =
    throwSemanticError . UnknownError
      $ "use other"

data ExampleData = ExampleData
  {exampleData'f, exampleData'v :: Nullable Text}
  deriving (Show)

instance Isn't ExampleData where
  isn't ExampleData{..} = isn't exampleData'f && isn't exampleData'v

instance Engine.Input ExampleData where
  type TermOf ExampleData = (Term Infer)

  type MetaOf ExampleData = ExampleData
  metaOf = id

  termOrValueOf sheetId ca ExampleData{..} = case (exampleData'f, exampleData'v) of
    (Is formula, _) -> Just (Left <$> parseCell formulaP formula)
    (_, Is value) -> Just (Right <$> parseCell valueP value)
    _ -> Nothing
   where
    parseCell :: Parser a -> Text -> Engine.Parsed a
    parseCell p = parse' sheetId ca p . Text.unpack

exampleData :: String -> ExampleData
exampleData input = case input of
  '=' : _ -> ExampleData (Is (Text.pack input)) Missing
  _ -> ExampleData Missing (Is (Text.pack input))

type EngineState = Engine.EngineState () () ExampleData (Term Infer)

newEngineState :: EngineState
newEngineState = Engine.newEngineState

-- | run a recomputation of a single spreadsheet
runRecompute
  :: [(CellAddr, String)]
  -> EngineState
  -> ( Either [CellAddr] [(CellAddr, Either (Engine.FetchError SemanticError) Value)]
     , EngineState
     )
runRecompute rawChanges st =
  first (bimap (map snd) (map (first snd)))
    . runIdentity
    $ Engine.runEngineT (initialise >> step) st
 where
  inputs = map (second exampleData) rawChanges
  (_, changes) = Engine.validateCells @ExampleData testId inputs

  initialise = Engine.modifyDocs (Engine.insertSheet testId () . Engine.insertDocument uri ())
  step = Engine.recompute @ExampleData testId changes

-- | run recomputation but only return result
evalRecompute
  :: [(CellAddr, String)]
  -> EngineState
  -> Either [CellAddr] [(CellAddr, Either (Engine.FetchError SemanticError) Value)]
evalRecompute rawChanges = fst . runRecompute rawChanges

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> "Left" <> parens (pretty x)
    Right y -> "Right" <> parens (pretty y)

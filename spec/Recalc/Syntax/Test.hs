module Recalc.Syntax.Test (module Recalc.Syntax.Test, Parser) where

import Test.Hspec (Expectation)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (eof, runParser)

import Recalc.Syntax.Parser (Parser)

parseTest :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
parseTest p src x = runParser (p <* eof) "success-spec" src `shouldParse` x

failTest :: Show a => Parser a -> String -> Expectation
failTest p = shouldFailOn (runParser (p <* eof) "fail-spec")

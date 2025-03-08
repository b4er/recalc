{-|
Module      : Recalc.Syntax.Test
Description : Test utility functions for testing grammars.
-}
module Recalc.Syntax.Test (Parser, module Recalc.Syntax.Test) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Test.Hspec (Expectation)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (ParseErrorBundle, eof, runParser)

import Data.Void (Void)
import Recalc.Engine (SheetId)
import Recalc.Syntax.Parser (Parser)

parseTest :: (Eq a, Show a) => SheetId -> Parser a -> String -> a -> Expectation
parseTest sheetId p src x = runP sheetId p "success-spec" src `shouldParse` x

failTest :: Show a => SheetId -> Parser a -> String -> Expectation
failTest sheetId p = shouldFailOn (runP sheetId p "fail-spec")

runP :: SheetId -> Parser a -> String -> String -> Either (ParseErrorBundle String Void) a
runP sheetId p = runParser (runReaderT p sheetId <* eof)

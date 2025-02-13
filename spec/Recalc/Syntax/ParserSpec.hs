{-# LANGUAGE OverloadedStrings #-}

module Recalc.Syntax.ParserSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (eof)

import Recalc.Syntax.Parser
import Recalc.Syntax.Term
import Recalc.Syntax.Test

spec :: Spec
spec = do
  describe "decimal" . it "passes examples" $ do
    parseTest decimal "0" 0
    parseTest decimal "-1" (-1)
    parseTest decimal "1" 1
    parseTest (decimal <* eof) "42 " 42

  describe "parens" . it "passes simple examples" $ do
    parseTest (parens decimal <* eof) "(1)" 1
    failTest (parens decimal <* eof) "(1"

  describe "readExcel" . it "passes some examples" $ do
    readExcel "A0" `shouldBe` Nothing
    readExcel "A1" `shouldBe` Just (0, 0)
    readExcel "A2" `shouldBe` Just (1, 0)
    readExcel "Z53" `shouldBe` Just (52, 25)
    readExcel "AA1" `shouldBe` Just (0, 26)

  describe "termP" . it "passes the an example" $ do
    parseTest (formulaP <* eof) "=foo" (Free "foo")

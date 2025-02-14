{-# LANGUAGE OverloadedStrings #-}

module Recalc.Syntax.ParserSpec where

import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (eof)

import Recalc.Syntax.Arbitrary ()
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

  describe "termP" . it "parses simple types (variables, globals, Set)" $ do
    parseTest (formulaP <* eof) "=foo" (Free "foo")
    parseTest (formulaP <* eof) "=Bool" (Free "Bool")
    parseTest (formulaP <* eof) "=False" (Free "False")
    parseTest (formulaP <* eof) "=True" (Free "True")
    parseTest (formulaP <* eof) "= *" (Set 0)
    parseTest (formulaP <* eof) "= * " (Set 0)
    parseTest (formulaP <* eof) " =* " (Set 0)
    parseTest (formulaP <* eof) "=* " (Set 0)

  describe "termP" . it "parses Π-types" $ do
    parseTest (formulaP <* eof) "=* -> *" (Pi (pat "x") (Inf (Set 0)) (Inf (Set 0)))
    parseTest (formulaP <* eof) "=* -> *" (Pi Nothing (Inf (Set 0)) (Inf (Set 0)))
    parseTest (formulaP <* eof) "=a -> a" (Pi Nothing (Inf (Free "a")) (Inf (Free "A")))
    parseTest
      (formulaP <* eof)
      "=(a: *) -> a"
      (Pi (pat "x") (Inf (Set 0)) (Inf (Bound 0)))
    parseTest
      (formulaP <* eof)
      "=(t: * -> *) -> t(a)"
      ( Pi
          (pat "x")
          (Inf (Pi Nothing (Inf (Set 0)) (Inf (Set 0))))
          (Inf (Bound 0 :$ Inf (Free "a")))
      )

  describe "termP" . it "parses term applications" $ do
    parseTest (formulaP <* eof) "=F(x,y)" (Free "f" :$ Inf (Free "x") :$ Inf (Free "y"))
    parseTest
      (formulaP <* eof)
      "=g (\\x -> x)"
      (Free "g" :$ Lam (pat "y") (Inf (Bound 0)))

  describe "pretty for Terms"
    $ it "generates a fresh binder"
    $ do
      renderPretty (Lam (pat "x1") (Lam Nothing (Inf (Bound 0))))
        `shouldBe` "\\x1 -> \\x -> x"
      renderPretty (Lam (pat "x") (Lam Nothing (Inf (Bound 0))))
        `shouldBe` "\\x -> \\x1 -> x1"
      renderPretty (Lam (pat "x") (Lam (pat "x1") (Lam Nothing (Inf (Bound 0)))))
        `shouldBe` "\\x -> \\x1 -> \\x2 -> x2"

-- the following test is a bit problematic since we rule out invalid terms already
-- in the grammar.. for example: @_ -> (\x -> x)@ is legal to express yet
-- the grammar already rules out such a term

-- FIXME: re-visit once language+parser are not going to change much anymore

-- describe "QuickChecks" $
--   prop "pretty-printed terms parse" $ \x ->
--     parseTest (formulaP <* eof) ("=" ++ renderPretty x) x

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty

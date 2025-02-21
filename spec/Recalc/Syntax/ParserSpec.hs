{-# LANGUAGE OverloadedStrings #-}

module Recalc.Syntax.ParserSpec where

import Data.Maybe (fromJust)
import Network.URI (URI, parseURI)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Recalc.Engine.Core (CellAddr, CellRange, SheetId)
import Recalc.Syntax.Arbitrary ()
import Recalc.Syntax.Parser
import Recalc.Syntax.Term
import Recalc.Syntax.Test qualified as Test

readURI :: String -> URI
readURI = fromJust . parseURI

sheetId :: SheetId
sheetId = (uri, "Sheet1")

uri :: URI
uri = readURI "file://ParserSpec.rc"

point :: CellAddr -> CellRange
point x = (x, x)

spec :: Spec
spec = do
  let
    parseTest :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
    parseTest = Test.parseTest sheetId

    failTest :: Show a => Parser a -> String -> Expectation
    failTest = Test.failTest sheetId

  describe "decimal" . it "passes examples" $ do
    parseTest decimal "0" 0
    parseTest decimal "-1" (-1)
    parseTest decimal "1" 1
    parseTest decimal "42 " 42

  describe "parens" . it "passes simple examples" $ do
    parseTest (parens decimal) "(1)" 1
    failTest (parens decimal) "(1"

  describe "readExcel" . it "passes some examples" $ do
    readExcel "A0" `shouldBe` Nothing
    foldMap
      (\(input, expected) -> readExcel input `shouldBe` Just expected)
      [("A1", (0, 0)), ("A2", (1, 0)), ("Z53", (52, 25)), ("AA1", (0, 26))]

  describe "cellReferenceOrFree" $ do
    it "passes some examples" $ do
      parseTest cellReferenceOrFree "BOOL" (Lit Bool)
      parseTest cellReferenceOrFree "bool" (Lit Bool)
      parseTest cellReferenceOrFree "Boolean" (Free "boolean")
      parseTest cellReferenceOrFree "boolean" (Free "boolean")
      parseTest cellReferenceOrFree "A1" $ Ref sheetId Unspecified (point (0, 0))
      parseTest cellReferenceOrFree "Bool!A1" $ Ref (uri, "Bool") SheetOnly (point (0, 0))
      parseTest cellReferenceOrFree "Bool!A10" $ Ref (uri, "Bool") SheetOnly (point (9, 0))
      failTest cellReferenceOrFree "[dir/file.ending]Bool!A10"
      parseTest cellReferenceOrFree "[file.ending]Bool!A10"
        $ Ref (readURI "file://file.ending", "Bool") FullySpecified (point (9, 0))
      failTest cellReferenceOrFree "[another file.ending]sheet!A10"
      parseTest cellReferenceOrFree "'[dir/another file.ending]sheet'!B10"
        $ Ref (readURI "file://dir/another%20file.ending", "sheet") FullySpecified (point (9, 1))
      parseTest cellReferenceOrFree "'bool'!ZZ1" $ Ref (uri, "bool") SheetOnly (point (0, 701))
      parseTest cellReferenceOrFree "'Sheet 1'!C3" $ Ref (uri, "Sheet 1") SheetOnly (point (2, 2))
      failTest cellReferenceOrFree "Sheet 1!C3"
      parseTest cellReferenceOrFree "'[d/ük.x~]sheet \\[copy\\]'!B10"
        $ Ref (readURI "file://d/%C3%BCk.x~", "sheet [copy]") FullySpecified (point (9, 1))
      parseTest
        cellReferenceOrFree
        "'[d/ü\\[k\\].x~]sheet \\[copy\\]'!B10"
        (Ref (readURI "file://d/%C3%BC%5Bk%5D.x~", "sheet [copy]") FullySpecified (point (9, 1)))
      parseTest
        cellReferenceOrFree
        "[p.a_b~c]d~.e!B2"
        (Ref (readURI "file://p.a_b~c", "d~.e") FullySpecified (point (1, 1)))

    it "passes more examples with cell ranges" $ do
      parseTest cellReferenceOrFree "A1:B2" (Ref sheetId Unspecified ((0, 0), (1, 1)))
      parseTest cellReferenceOrFree "B2 : A1" (Ref sheetId Unspecified ((0, 0), (1, 1)))
      parseTest cellReferenceOrFree "sheet ! B2 : A1" (Ref (uri, "sheet") SheetOnly ((0, 0), (1, 1)))
      parseTest
        cellReferenceOrFree
        "[f]sheet!B2:A1"
        (Ref (readURI "file://f", "sheet") SheetOnly ((0, 0), (1, 1)))
      parseTest
        cellReferenceOrFree
        "[ f ] sheet  ! B2 : A1"
        (Ref (readURI "file://f", "sheet") SheetOnly ((0, 0), (1, 1)))
      parseTest
        cellReferenceOrFree
        "'[ f ] sheet '!B2:Z1"
        (Ref (readURI "file://%20f%20", " sheet ") SheetOnly ((0, 25), (1, 1)))

    it "pretty-prints corectly" $ do
      renderPretty (Ref (readURI "file://p.a_b~c", "d~.e") FullySpecified (point (1, 1)))
        `shouldBe` "[p.a_b~c]d~.e!B2"
      renderPretty
        (Ref (readURI "file://d/%C3%BC%5Bk%5D.x~", "sheet [copy]") FullySpecified (point (9, 1)))
        `shouldBe` "'[d/ü\\[k\\].x~]sheet \\[copy\\]'!B10"

  describe "termP" . it "parses simple types (variables, globals, Set)" $ do
    parseTest formulaP "=foo" (Free "foo")
    parseTest formulaP "=bool" (Lit Bool)
    parseTest formulaP "=False" (boolOf False)
    parseTest formulaP "=TRUE" (boolOf True)
    parseTest formulaP "= *" (Set 0)
    parseTest formulaP "= * " (Set 0)
    parseTest formulaP " =* " (Set 0)
    parseTest formulaP "=* " (Set 0)
    parseTest formulaP "=0" (intOf 0)
    parseTest formulaP "= 1 " (intOf 1)
    parseTest formulaP "= -3 " (intOf (-3))
    parseTest formulaP "=- 12" (intOf (-12))

  describe "termP" . it "parses Π-types" $ do
    parseTest formulaP "=* -> *" (Pi (pat "x") (Inf (Set 0)) (Inf (Set 0)))
    parseTest formulaP "=* -> *" (Pi Nothing (Inf (Set 0)) (Inf (Set 0)))
    parseTest formulaP "=a -> a" (Pi Nothing (Inf (Free "a")) (Inf (Free "A")))
    parseTest
      formulaP
      "=(a: *) -> a"
      (Pi (pat "x") (Inf (Set 0)) (Inf (Bound 0)))
    parseTest
      formulaP
      "=(t: * -> *) -> t(a)"
      ( Pi
          (pat "x")
          (Inf (Pi Nothing (Inf (Set 0)) (Inf (Set 0))))
          (Inf (Bound 0 :$ Inf (Free "a")))
      )

  describe "termP" . it "parses term applications" $ do
    parseTest formulaP "=F(x,y)" (Free "f" :$ Inf (Free "x") :$ Inf (Free "y"))
    parseTest
      formulaP
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
--     parseTest formulaP ("=" ++ renderPretty x) x

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty

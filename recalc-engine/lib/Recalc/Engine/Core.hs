module Recalc.Engine.Core where

import Data.Char (isDigit, isUpper, ord, toLower)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (URI)

type SheetName = Text

type SheetId = (URI, SheetName)

-- | row and column
type CellAddr = (Int, Int)

row, column :: CellAddr -> Int
row = fst
column = snd

type CellRange = (CellAddr, CellAddr)

type CellRangeRef = (SheetId, CellRange)

type CellRef = (SheetId, CellAddr)

-- | read an spreadsheet address of the form column-row
-- (columns are labelled "A..Z, AA..", and rows enumerated)
--
-- >>> readExcel "A2"
-- Just (1,0)
readExcel :: Text -> Maybe CellAddr
readExcel txt = go . (`Text.splitAt` txt) =<< alg 0 False (Text.unpack txt)
 where
  go (letters, digits)
    | r >= 0 = Just (r, readExcelCol letters - 1)
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

  readExcelCol :: Text -> Int
  readExcelCol = Text.foldl' (\a c -> 26 * a + ord (toLower c) - 96) 0

-- | show a zero-indexed cell address in Excel-style
--
-- >>> showExcel (0,4)
-- "E1"
showExcel26 :: CellAddr -> String
showExcel26 (r, c) = rowStr <> show (r + 1)
 where
  rowStr = concatMap sequence (tail $ iterate (['A' .. 'Z'] :) []) !! c

module Recalc.Engine.Core where

import Data.Text (Text)
import Network.URI (URI)

-- | Spreadsheets are uniquely determined by a resource id and sheet name
type SheetId = (URI, Text)

-- | Row and column (both zero-indexed)
type CellAddr = (Int, Int)

row, column :: CellAddr -> Int
row = fst
column = snd

-- | Beginning (top left) and end (bottom right) of a range
type CellRange = (CellAddr, CellAddr)

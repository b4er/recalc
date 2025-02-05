module Recalc.Engine.Internal where

-- | column and row (zero-indexed)
type CellAddr  = (Int, Int)

-- | beginning (top left) and end (bottom right) of a range
type CellRange = (CellAddr, CellAddr)

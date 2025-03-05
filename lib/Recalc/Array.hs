{-|
Module      : Recalc.Array
Description : Core array operations.

This module provides a few core operations on multi-dimensional arrays.
-}
module Recalc.Array (Array, Data.Array.Dynamic.fromList, tensorContract) where

import Data.Array.Convert (convert)
import Data.Array.Dynamic (Array, fromList)
import Data.Array.DynamicS qualified as ArrayS
import Numeric.LinearAlgebra as N hiding (Convert)
import Prelude hiding ((<>))

-- |
--
-- Tensor contraction along provided x-axis and y-axis. Essentially,
-- performs a generalized dot-product by summing over the specified
-- contraction dimensions of the input tensors.
--
-- The resulting tensor
-- retains the remaining dimensions of both inputs in their original
-- order, excluding the contracted ones.
--
-- === Parameters:
-- - @xAxes@: indices in the first tensor to contract over
-- - @yAxes@: indices in the second tensor to contract over corresponding to xAxes
-- - @x@: input tensor of shape @[m1, ..., mM]@
-- - @y@: input tensor of shape @[n1, ..., nN]@
--
-- The result is a tensor of shape [c1, ..., cC], where the shape is
-- derived by removing the contracted dimensions from both inputs.
--
-- Example: Contracting a @[3, 4, 5]@ tensor with a @[5, 6, 7]@ tensor
-- along the last dimension of the first and the first dimension of
-- the second results in a @[3, 4, 6, 7]@ tensor.
tensorContract :: (Numeric a) => [Int] -> [Int] -> Array a -> Array a -> Array a
tensorContract xAxes yAxes x y =
  let
    (x', xShape) = (convert x, ArrayS.shapeL x')
    (y', yShape) = (convert y, ArrayS.shapeL y')

    -- new tensor shape
    (xIxs, xDims) = unzip [(i, d) | (i, d) <- zip [0 ..] xShape, i `notElem` xAxes]
    (yIxs, yDims) = unzip [(j, d) | (j, d) <- zip [0 ..] yShape, j `notElem` yAxes]

    shape = xDims ++ yDims

    -- prepare matrix-multiplication: align contraction dims, reshape into matrices
    xPermuted = ArrayS.transpose (xIxs ++ xAxes) x'
    yPermuted = ArrayS.transpose (yAxes ++ yIxs) y'

    xMat = N.reshape (product (map (xShape !!) xAxes)) (ArrayS.toVector xPermuted)
    yMat = N.reshape (product yDims) (ArrayS.toVector yPermuted)

    -- contract
    result = flatten (xMat <> yMat)
  in
    convert $ ArrayS.fromVector shape result

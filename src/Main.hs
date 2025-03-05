{-# LANGUAGE DataKinds #-}

module Main where

import Recalc.Language (Env (..), Mode (Infer), Term, prelude)
import Recalc.Univer (univerMain)

main :: IO ()
main = univerMain @(Term Infer) (Env prelude)

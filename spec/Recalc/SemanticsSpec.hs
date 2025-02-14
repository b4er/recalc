{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Recalc.SemanticsSpec where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Recalc.Engine
import Recalc.Semantics
import Recalc.Syntax.Term

spec :: Spec
spec = do
  describe "check" $ it "passes basic examples" $ do
    shouldBe () ()

  describe "infer" $ it "passes basic examples" $ do
    shouldBe () ()

  describe "eval" $ do
    it "evaluates Set(_) correctly" $ do
      runEval (Set 0) `shouldBe` Right (VSet 0)
      runEval (Set 1) `shouldBe` Right (VSet 1)

    it "evaluates variables+globals correctly" $ do
      runEval (Free "J") `shouldBe` Right (vfree "J")
      runEval (Free "J") `shouldBe` Right (vfree "j")

      runEval (Free "not")
        `shouldBe` Right
          (VLam (pat "x") (VNeutral . NApp (NFree "not")))
      runEval (Free "not")
        `shouldBe` Right
          (VLam (pat "a") (VNeutral . NApp (NFree "not")))
      runEval (Free "not")
        `shouldNotBe` Right
          (VLam (pat "a") (\_x -> VNeutral (NFree "not")))

    -- SKI combinators
    let
      apT =
        Lam (pat "f")
          $ Lam (pat "g")
          $ Lam (pat "x")
          $ Inf ((Bound 2 :$ Inf (Bound 0)) :$ Inf (Bound 1 :$ Inf (Bound 0)))
      constT = Lam (pat "x") $ Lam (pat "y") $ Inf (Bound 1)
      idT = Lam (pat "x") (Inf (Bound 0))

    -- for evaluation the type annotations are not required
    let annAny = (`Ann` Free "any")

    it "evaluates simple lambda abstractions (SKI)" $ do
      runEval constT `shouldBe` Right (VLam (pat "x") (VLam Nothing . const))

      runEval (Lam Nothing (Inf (Bound 0))) `shouldBe` Right (VLam Nothing id)
      runEval idT `shouldBe` Right (VLam (pat "y") id)

      runEval (annAny idT :$ idT) `shouldBe` Right (VLam (pat "y") id)

      runEval (annAny apT :$ constT :$ constT) `shouldBe` Right (VLam Nothing id)

      runEval (annAny apT :$ idT :$ idT) `shouldBe` Right (VLam Nothing (\x -> x `vapp` x))

      runEval apT
        `shouldBe` Right
          ( VLam Nothing $ \x ->
              VLam Nothing $ \y ->
                VLam Nothing $ \z ->
                  vapp x z `vapp` vapp y z
          )

    it "evaluates simple lambda abstractions (examples)" $ do
      runEval (Pi (pat "t") (Inf (Set 0)) (Inf (Set 1)))
        `shouldBe` Right
          (VPi Nothing (VSet 0) (const (VSet 1)))

      runEval (Pi (pat "t") (Inf (annAny idT :$ Inf (Set 0))) (Inf (Set 1)))
        `shouldBe` Right
          (VPi Nothing (VSet 0) (const (VSet 1)))

      runEval (Free "f" :$ Inf (Free "g" :$ Inf (Free "x")))
        `shouldBe` Right
          (VNeutral (NApp (NFree "f") (VNeutral (NApp (NFree "g") (VNeutral (NFree "x"))))))

{-- Utilities to run tests: --}

type Result = Either (FetchError SemanticError)

runFetch :: Fetch (Term Infer) a -> Result a
runFetch fx = runExcept (runReaderT fx env)
 where
  sheetId1 = (fromJust (parseURI "file:///SemanticSpec.rc"), "Test Sheet 1")

  env = (fetch, newEnv @(Term Infer) sheetId1)

  -- fetching is tested in the Engine spec
  fetch _ix =
    throwSemanticError . UnknownError
      $ "fetching not supported during tests."

runEval :: Term m -> Result Value
runEval x = runFetch (eval' x)

runCheck :: Value -> Term Check -> Result ()
runCheck x ty = runFetch (check' 0 x ty)

runInfer :: Term Infer -> Result Type
runInfer x = runFetch (infer x)

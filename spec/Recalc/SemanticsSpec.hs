{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Recalc.SemanticsSpec where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (prop)

import Recalc.Engine hiding (runFetch)
import Recalc.Language
import Recalc.Syntax.Arbitrary (Set0 (..))
import Recalc.Syntax.Term

spec :: Spec
spec = do
  describe "check" $ do
    prop "passes arbitrary test for Inf" $ \x ->
      let
        xTy = runInfer [] x
        res = do ty <- xTy; runCheck [] ty (Inf x)
      in
        res `shouldBe` void xTy

  describe "infer" $ do
    it "passes Boolean examples" $ do
      runInfer [] (boolOf False) `shouldBe` Right vbool
      runInfer [] (Lit Bool) `shouldBe` Right (VSet 0)

    prop "passes Set(_) examples (hierarchy of universes)" $ \k ->
      runInfer [] (Set k) `shouldBe` Right (VSet (k + 1))

    prop "infers arbitrary terms (x: *)" $ \(Set0 x) ->
      runInfer [] x `shouldBe` Right (VSet 0)

    {-prop "infers globals" $ \name ty gs' ->
      do
        let
          gs = map (bimap Global typeDecl) $ filter (\(n, _) -> n /= name) gs'
        runInfer ((Global name, typeDecl ty) : gs) (Free (Global name))
        `shouldBe` Right ty-}

    it "infers built-ins correctly (boolean operators)" $ do
      runInfer [] (Free "not") `shouldBe` Right (vbool `vfun` vbool)
      runInfer [] (Free "or") `shouldBe` Right (vbool `vfun` vbool `vfun` vbool)

    it "infers application of built-ins (boolean operators + examples)" $ do
      runInfer [] (Free "not" :$ Inf (boolOf False)) `shouldBe` Right vbool
      runInfer
        [ ("any", typeDecl (VSet 0))
        , (Local Nothing 0, typeDecl (vpi Nothing (VSet 0) id))
        ]
        (Free (Local Nothing 0) :$ Inf (Free "any"))
        `shouldBe` Right (vfree "any")

  describe "eval" $ do
    it "evaluates Set(_)" $ do
      runEval (Set 0) `shouldBe` Right (VSet 0)
      runEval (Set 1) `shouldBe` Right (VSet 1)

    it "evaluates variables+globals" $ do
      runEval (Free "J") `shouldBe` Right (vfree "J")
      runEval (Free "J") `shouldBe` Right (vfree "j")

      runEval (Free "not")
        `shouldBe` Right
          (vlam (pat "x") (VNeutral . NApp (NFree "not")))
      runEval (Free "not")
        `shouldBe` Right
          (vlam (pat "a") (VNeutral . NApp (NFree "not")))
      runEval (Free "not")
        `shouldNotBe` Right
          (vlam (pat "a") (\_x -> VNeutral (NFree "not")))

    it "evaluates applications (Boolean values)" $ do
      runEval (Free "not" :$ Inf (boolOf True)) `shouldBe` Right (vboolOf False)
      runEval (Free "not" :$ Inf (boolOf False)) `shouldBe` Right (vboolOf True)
      runEval (Free "and" :$ Inf (boolOf True) :$ Inf (boolOf False)) `shouldBe` Right (vboolOf False)

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
      runEval constT `shouldBe` Right (vlam (pat "x") (vlam Nothing . const))

      runEval (Lam Nothing (Inf (Bound 0))) `shouldBe` Right (vlam Nothing id)
      runEval idT `shouldBe` Right (vlam (pat "y") id)

      runEval (annAny idT :$ idT) `shouldBe` Right (vlam (pat "y") id)

      runEval (annAny apT :$ constT :$ constT) `shouldBe` Right (vlam Nothing id)

      runEval (annAny apT :$ idT :$ idT) `shouldBe` Right (VLam Nothing (\x -> x `vapp` x))

      runEval apT
        `shouldBe` Right
          ( vlam Nothing $ \x ->
              vlam Nothing $ \y ->
                VLam Nothing $ \z -> do
                  xz <- x `vapp` z
                  yz <- y `vapp` z
                  xz `vapp` yz
          )

    it "evaluates simple lambda abstractions (examples)" $ do
      runEval (Pi (pat "t") (Inf (Set 0)) (Inf (Set 1)))
        `shouldBe` Right
          (vpi Nothing (VSet 0) (const (VSet 1)))

      runEval (Pi (pat "t") (Inf (annAny idT :$ Inf (Set 0))) (Inf (Set 1)))
        `shouldBe` Right
          (vpi Nothing (VSet 0) (const (VSet 1)))

      runEval (Free "f" :$ Inf (Free "g" :$ Inf (Free "x")))
        `shouldBe` Right
          (VNeutral (NApp (NFree "f") (VNeutral (NApp (NFree "g") (VNeutral (NFree "x"))))))

type Result = Either (FetchError SemanticError)

runFetch :: [(Name, Decl)] -> Fetch Env SemanticError Value a -> Result a
runFetch extra = runFetchWith env f
 where
  env = Env (prelude <> Map.fromList extra)
  f _ _ = throwError RefError

runInfer :: [(Name, Decl)] -> Term Infer -> Result Type
runInfer extra = runFetch extra . infer' []

runCheck :: [(Name, Decl)] -> Type -> Term Check -> Result ()
runCheck extra ty = runFetch extra . check' [] ty

runEval :: Term m -> Result Value
runEval = runFetch mempty . eval'

typeDecl :: Type -> Decl
typeDecl = (`Decl` Nothing)

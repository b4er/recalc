{-# LANGUAGE OverloadedStrings #-}

module Recalc.Syntax.UnifySpec where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Recalc.Syntax.Term
import Recalc.Syntax.Unify

spec :: Spec
spec = applySpec >> unifySpec

applySpec, unifySpec :: Spec
applySpec = describe "apply" $ do
  it "passes simple examples" $ do
    apply mempty (Inf (Free (Implicit 0))) `shouldBe` Inf (Free (Implicit 0))
    apply (Map.fromList [(0, Lit Int)]) (Inf (Free (Implicit 0)))
      `shouldBe` Inf (Lit Int)
  it "does not skip binders" $ do
    apply (Map.fromList [(0, Lit Int)]) (Lam EArg Nothing (Inf (Free (Implicit 0))))
      `shouldBe` Lam EArg Nothing (Inf (Lit Int))
    apply
      (Map.fromList [(0, Lit Bool)])
      (Pi IArg Nothing (Inf (Set 0)) (Lam EArg Nothing (Inf (Free (Implicit 0)))))
      `shouldBe` Pi IArg Nothing (Inf (Set 0)) (Lam EArg Nothing (Inf (Lit Bool)))
unifySpec = describe "unification" $ do
  it "unifies literals" $ do
    unify (Inf (intOf 12), Inf (intOf 12))
      `shouldBe` Right mempty
    unify (Inf (intOf 13), Inf (intOf 12))
      `shouldSatisfy` match

  it "solves simple equations (directly assign variable)" $ do
    unify (Inf (Free (Implicit 0)), Inf (intOf 42))
      `shouldBe` Right (Map.fromList [(0, intOf 42)])
    unify (Inf (intOf 43), Inf (Free (Implicit 1)))
      `shouldBe` Right (Map.fromList [(1, intOf 43)])
    unify (Inf (Free (Implicit 1)), Inf (Free (Implicit 1)))
      `shouldBe` Right mempty

  it "passes examples for occurs-check" $ do
    unify
      ( Inf (Free (Implicit 1))
      , Inf (Pi EArg Nothing (Inf (Free (Implicit 1))) (Inf (Free "r")))
      )
      `shouldSatisfy` occursCheck
    unify
      ( Inf (Free (Implicit 1))
      , Inf (Ann (Inf (Free (Implicit 1))) (Lit Int))
      )
      `shouldSatisfy` occursCheck

  it "fails on unsolvable examples" $ do
    let td = TensorDescriptor (Free (Implicit 0)) [Inf (intOf 2)]
    unify
      ( Inf (Tensor td)
      , Inf (TensorOf td (map (Inf . intOf) [0, 1]))
      )
      `shouldSatisfy` match
    unify
      ( Inf (TensorOf td (map (Inf . intOf) [1, 0]))
      , Inf (TensorOf td (map (Inf . intOf) [0, 0]))
      )
      `shouldSatisfy` match

  it "passes example with application" $ do
    unify
      ( Inf (App (Free (Implicit 0)) (EArg, Inf (intOf 3)))
      , Inf (App (intOf 4) (EArg, Inf (intOf 3)))
      )
      `shouldBe` Right (Map.fromList [(0, intOf 4)])

  it "passes nested examples" $ do
    unify
      ( Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 0)))
            . Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 0)))
          $ Inf (Free (Implicit 0))
      , Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 1)))
            . Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 2)))
          $ Inf (Free "x")
      )
      `shouldBe` Right (Map.fromList [(0, Free "x"), (1, Free "x"), (2, Free "x")])

    unify
      ( Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 0)))
            . Inf
          $ Pi EArg Nothing (Inf (Pi EArg Nothing (Inf (Free (Implicit 0))) $ Inf (Free (Implicit 0))))
          $ Inf (Free (Implicit 0))
      , Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 1)))
            . Inf
          $ Pi EArg Nothing (Inf (Free (Implicit 2)))
          $ Inf (Free "x")
      )
      `shouldBe` Right
        ( Map.fromList
            [(0, Free "x"), (1, Free "x"), (2, Pi EArg Nothing (Inf (Free "x")) (Inf (Free "x")))]
        )

    -- tensors
    unify
      ( Inf (Tensor (TensorDescriptor (Lit Bool) []))
      , Inf (Tensor (TensorDescriptor (Lit Bool) []))
      )
      `shouldBe` Right mempty

    unify
      ( Inf (Tensor (TensorDescriptor (Free (Implicit 0)) []))
      , Inf (Tensor (TensorDescriptor (Lit Int) []))
      )
      `shouldBe` Right (Map.fromList [(0, Lit Int)])

    unify
      ( Inf
          ( Tensor
              ( TensorDescriptor
                  (Lit Int)
                  [Inf (Free (Implicit 0)), Inf (Free (Implicit 1))]
              )
          )
      , Inf
          ( Tensor
              ( TensorDescriptor
                  (Lit Int)
                  [Inf (intOf 2), Inf (intOf 3)]
              )
          )
      )
      `shouldBe` Right (Map.fromList [(0, intOf 2), (1, intOf 3)])

occursCheck, match :: Either UnificationError a -> Bool
occursCheck = \case
  Left OccursCheck{} -> True
  _ -> False
match = \case
  Left UnifyMismatchC{} -> True
  Left UnifyMismatchI{} -> True
  _ -> False

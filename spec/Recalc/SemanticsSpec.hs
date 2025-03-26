{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Recalc.SemanticsSpec
Description : Test the core language's evaluation, type checking
              and type inference.
-}
module Recalc.SemanticsSpec where

import Control.Monad (void, (<=<))
import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (prop)

import Recalc.Array
import Recalc.Engine
import Recalc.Language
import Recalc.Syntax.Arbitrary (Set0 (..))
import Recalc.Syntax.Term
import Recalc.Syntax.Unify (UnificationError (UnifyMismatchI))

spec :: Spec
spec = do
  describe "check" $ do
    prop "passes arbitrary test for Inf" $ \x ->
      let
        xTy = runInfer_ [] x
        res = do ty <- xTy; runCheck_ [] ty (Inf x)
      in
        res `shouldBe` void xTy

  describe "infer" $ do
    it "passes Boolean examples" $ do
      runInfer_ [] (boolOf False) `shouldBe` Right vbool
      runInfer_ [] (Lit Bool) `shouldBe` Right (VSet 0)

    prop "passes Set(_) examples (hierarchy of universes)" $ \k ->
      runInfer_ [] (Set k) `shouldBe` Right (VSet (k + 1))

    prop "infers arbitrary terms (x: *)" $ \(Set0 x) ->
      runInfer_ [] x `shouldBe` Right (VSet 0)

    {-prop "infers globals" $ \name ty gs' ->
      do
        let
          gs = map (bimap Global typeDecl) $ filter (\(n, _) -> n /= name) gs'
        runInfer ((Global name, typeDecl ty) : gs) (Free (Global name))
        `shouldBe` Right ty-}

    it "infers built-ins correctly (boolean operators)" $ do
      runInfer_ [] (Free "not") `shouldBe` Right (vbool `vfun` vbool)
      runInfer_ [] (Free "or") `shouldBe` Right (vbool `vfun` vbool `vfun` vbool)

    it "infers application of built-ins (boolean operators + examples)" $ do
      runInfer_ [] (Free "not" :$ Inf (boolOf False)) `shouldBe` Right vbool
      runInfer_
        [ ("any", typeDecl (VSet 0))
        , ("f", typeDecl (vpi EArg Nothing (VSet 0) id))
        ]
        (Free "f" :$ Inf (Free "any"))
        `shouldBe` Right (vfree "any")

    let vtensor = VTensor . VTensorDescriptor (VLit Int)

    it "infers implicits correctly" $ do
      -- matrix examples
      runInfer_
        [("m", typeDecl (vtensor (map (VLitOf . IntOf) [3, 2])))]
        (Free "mmult" :$ Inf (Free "m"))
        `shouldBe` Right
          ( VPi IArg (Just (CaseInsensitive "k")) (VLit Int) $ \k ->
              pure $ vtensor [VLitOf (IntOf 2), k] `vfun` vtensor [VLitOf (IntOf 3), k]
          )
      runInfer_
        [ ("m", typeDecl (vtensor (map (VLitOf . IntOf) [3, 2])))
        , ("n", typeDecl (vtensor (map (VLitOf . IntOf) [2, 4])))
        ]
        (Free "mmult" :$ Inf (Free "m"))
        `shouldBe` Right
          ( VPi IArg (Just (CaseInsensitive "k")) (VLit Int) $ \k ->
              pure
                $ vtensor [VLitOf (IntOf 2), k] `vfun` vtensor [VLitOf (IntOf 3), k]
          )
      -- identity and const
      let
        constT =
          VPi IArg (Just (CaseInsensitive "a")) (VSet 0) $ \a -> pure $ VPi IArg (Just (CaseInsensitive "b")) (VSet 0) $ \b -> pure (a `vfun` b `vfun` a)

        idT = VPi IArg (Just (CaseInsensitive "a")) (VSet 0) $ \a -> pure (a `vfun` a)

      runInfer_
        [ ("id", typeDecl idT)
        ]
        (Free "id" :$ Inf (intOf 3))
        `shouldBe` Right (VLit Int)

      runInfer_
        [ ("const", typeDecl constT)
        , ("id", typeDecl idT)
        ]
        (Free "const" :$ Inf (Free "id") :$ Inf (boolOf False))
        `shouldBe` Right idT

    let
      matrices =
        [ ("A", typeDecl (vtensor (map (VLitOf . IntOf) [3, 2])))
        , ("B", typeDecl (vtensor (map (VLitOf . IntOf) [2, 5])))
        ]

      -- the result of mmult(A): should elaborate to
      -- `{k:int} -> mmult{3, 2, k}(A)` since `k` is not known
      multA =
        Elaborate . Lam IArg (Just (CaseInsensitive "k")) . Inf
          $ Free "mmult"
            `App` (IArg, Inf (intOf 3))
            `App` (IArg, Inf (intOf 2))
            `App` (IArg, Inf (Bound 0))
            `App` (EArg, Inf (Free "A"))

    it "elaborates implicit applications" $ do
      (snd <$> runInfer matrices (Free "mmult" :$ Inf (Free "A"))) `shouldBe` Right multA

      (snd <$> runInfer matrices (Free "mmult" :$ Inf (Free "A") :$ Inf (Free "B")))
        -- should elaborate to `mmult{3, 2, 5}(A,B)` now `k` is known (it's a bit nested)
        `shouldBe` Right (multA `App` (IArg, Inf (intOf 5)) `App` (EArg, Inf (Free "B")))

      -- failing case B*A:
      (snd <$> runInfer matrices (Free "mmult" :$ Inf (Free "B") :$ Inf (Free "A")))
        `shouldBe` Left (SemanticError (UnificationError (UnifyMismatchI (intOf 3, intOf 5))))

  describe "eval" $ do
    it "evaluates Set(_)" $ do
      runEval [] (Set 0) `shouldBe` Right (VSet 0)
      runEval [] (Set 1) `shouldBe` Right (VSet 1)

    it "evaluates variables+globals" $ do
      runEval' [] (Free "J") `shouldBe` Right (vfree "J")
      runEval' [] (Free "J") `shouldBe` Right (vfree "j")

      runEval [] (Free "not")
        `shouldBe` Right
          (vlam EArg (pat "x") (VNeutral . NApp EArg (NFree "not")))
      runEval [] (Free "not")
        `shouldBe` Right
          (vlam EArg (pat "a") (VNeutral . NApp EArg (NFree "not")))
      runEval [] (Free "not")
        `shouldNotBe` Right
          (vlam EArg (pat "a") (\_x -> VNeutral (NFree "not")))

    it "evaluates applications (Boolean values)" $ do
      runEval [] (Free "not" :$ Inf (boolOf True)) `shouldBe` Right (vboolOf False)
      runEval [] (Free "not" :$ Inf (boolOf False)) `shouldBe` Right (vboolOf True)
      runEval [] (Free "and" :$ Inf (boolOf True) :$ Inf (boolOf False)) `shouldBe` Right (vboolOf False)

    -- SKI combinators
    let
      apT =
        Lam EArg (pat "f")
          $ Lam EArg (pat "g")
          $ Lam EArg (pat "x")
          $ Inf ((Bound 2 :$ Inf (Bound 0)) :$ Inf (Bound 1 :$ Inf (Bound 0)))
      constT = Lam EArg (pat "x") $ Lam EArg (pat "y") $ Inf (Bound 1)
      idT = Lam EArg (pat "x") (Inf (Bound 0))

    -- for evaluation the type annotations are not required
    let annAny = (`Ann` Free "any")

    it "evaluates simple lambda abstractions (SKI)" $ do
      runEval' [] constT `shouldBe` Right (vlam EArg (pat "x") (vlam EArg Nothing . const))

      runEval' [] (Lam EArg Nothing (Inf (Bound 0))) `shouldBe` Right (vlam EArg Nothing id)
      runEval' [] idT `shouldBe` Right (vlam EArg (pat "y") id)

      runEval' [] (annAny idT :$ idT) `shouldBe` Right (vlam EArg (pat "y") id)

      runEval' [] (annAny apT :$ constT :$ constT) `shouldBe` Right (vlam EArg Nothing id)

      runEval' [] (annAny apT :$ idT :$ idT) `shouldBe` Right (VLam EArg Nothing (\x -> x `vapp` x))

      runEval' [] apT
        `shouldBe` Right
          ( vlam EArg Nothing $ \x ->
              vlam EArg Nothing $ \y ->
                VLam EArg Nothing $ \z -> do
                  xz <- x `vapp` z
                  yz <- y `vapp` z
                  xz `vapp` yz
          )

    it "evaluates simple lambda abstractions (examples)" $ do
      runEval' [] (Pi EArg (pat "t") (Inf (Set 0)) (Inf (Set 1)))
        `shouldBe` Right
          (vpi EArg Nothing (VSet 0) (const (VSet 1)))

      runEval' [] (Pi EArg (pat "t") (Inf (annAny idT :$ Inf (Set 0))) (Inf (Set 1)))
        `shouldBe` Right
          (vpi EArg Nothing (VSet 0) (const (VSet 1)))

      runEval' [] (Free "f" :$ Inf (Free "g" :$ Inf (Free "x")))
        `shouldBe` Right
          (VNeutral (NApp EArg (NFree "f") (VNeutral (NApp EArg (NFree "g") (VNeutral (NFree "x"))))))

    it "does matrix multiplication" $ do
      let
        vtd s = VTensorDescriptor (VLit Int) (map (VLitOf . IntOf) s)
        matrix s vs = VTensorOf (vtd s) (fromList s (map (VLitOf . IntOf) vs))
        decl (sx, x) = Decl (VTensor (vtd sx)) (Just (matrix sx x))

        mmult a b =
          runEval
            [("A", decl a), ("B", decl b)]
            (Free "mmult" :$ Inf (Free "A") :$ Inf (Free "B"))

      ([2, 2], [1, 2, 3, 4]) `mmult` ([2, 2], [1, 2, 3, 4])
        `shouldBe` Right (matrix [2, 2] [7, 10, 15, 22])

      ([2, 2], [0, 2, 3, 4]) `mmult` ([2, 2], [1, 2, 0, 4])
        `shouldBe` Right (matrix [2, 2] [0, 8, 3, 22])

      ([2, 2], [-1, 2, 3, -4]) `mmult` ([2, 2], [5, -6, -7, 8])
        `shouldBe` Right (matrix [2, 2] [-19, 22, 43, -50])

      ([1, 3], [1, 0, -1]) `mmult` ([3, 2], [2, -3, 4, 0, -1, 5])
        `shouldBe` Right (matrix [1, 2] [3, -8])

      ([2, 2], [-1, 2, 3, -4]) `mmult` ([2, 2], [1, 0, 0, 1])
        `shouldBe` Right (matrix [2, 2] [-1, 2, 3, -4])

      ([2, 2], [-1, 2, 3, -4]) `mmult` ([2, 2], [0, 0, 0, 0])
        `shouldBe` Right (matrix [2, 2] [0, 0, 0, 0])

type Result = Either (FetchError SemanticError)

runFetch :: [(Name, Decl)] -> FetchOf (Term Infer) a -> Result a
runFetch extra = runFetchWith env f
 where
  env = Env (prelude <> Map.fromList extra)
  f _ = throwError RefError

runInfer :: [(Name, Decl)] -> Term Infer -> Result (Type, Term Infer)
runInfer extra = runFetch extra . infer' []

runInfer_ :: [(Name, Decl)] -> Term Infer -> Result Type
runInfer_ extra = fmap fst . runInfer extra

runCheck :: [(Name, Decl)] -> Type -> Term Check -> Result (Term Check)
runCheck extra ty = runFetch extra . check' [] ty

runCheck_ :: [(Name, Decl)] -> Type -> Term Check -> Result ()
runCheck_ extra = fmap void . runCheck extra

runEval :: [(Name, Decl)] -> Term Infer -> Result Value
runEval extra = runFetch extra . (eval' [] . snd <=< inferElaborate)

-- | does _not_ do term elaboration!
runEval' :: [Value] -> Term m -> Result Value
runEval' extra = runFetch mempty . eval' extra

typeDecl :: Type -> Decl
typeDecl ty = Decl ty Nothing

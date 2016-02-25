module PrologInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Inspector.Extras
import           Prolog

spec :: Spec
spec = do
  describe "inspector" $ do
    describe "declaresPredicate" $ do
      it "is True when rule is declared" $ do
        declaresPredicate (named "foo") (pl "foo(X) :- bar(X).") `shouldBe` True

      it "is True when fact is declared" $ do
        declaresPredicate (named "foo") (pl "foo(tom).") `shouldBe` True

      it "is False when predicate is not declared" $ do
        declaresPredicate (named "foo") (pl "bar(tom).") `shouldBe` False

    describe "declaresWithArity" $ do
      it "is True when predicate is declared with given arity" $ do
        declaresWithArity 1 (named "foo") (pl "foo(tom).") `shouldBe` True

      it "is False when predicate is declared with another arity" $ do
        declaresWithArity 2 (named "foo") (pl "foo(tom).") `shouldBe` False

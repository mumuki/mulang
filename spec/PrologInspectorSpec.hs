module PrologInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Inspector.Extras
import           Prolog

spec :: Spec
spec = do
  describe "inspector" $ do
    describe "declaresFact" $ do
      it "is True when fact is declared" $ do
        declaresFact (named "foo") (pl "foo(3).") `shouldBe` True

      it "is False when fact is not declared" $ do
        declaresFact (named "baz") (pl "foo(5) :- bar(X).") `shouldBe` False
        declaresFact (named "foo") (pl "foo(5).") `shouldBe` False

    describe "declaresRule" $ do
      it "is True when rule is declared" $ do
        declaresRule (named "foo") (pl "foo(X) :- bar(X).") `shouldBe` True

      it "is False when rule is not declared" $ do
        declaresRule (named "baz") (pl "foo(X) :- bar(X).") `shouldBe` False

    describe "usesAnnonymousVariable" $ do
      it "is True when _ is used in rule" $ do
        usesAnnonymousVariable (pl "foo(_) :- bar(X).") `shouldBe` True

      it "is True when _ is used in fact" $ do
        usesAnnonymousVariable (pl "foo(_).") `shouldBe` True

      it "is False when _ is not used" $ do
        usesAnnonymousVariable (pl "foo(4).") `shouldBe` False

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

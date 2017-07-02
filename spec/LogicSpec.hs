module LogicSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Prolog

spec :: Spec
spec = do
  describe "inspector" $ do
    describe "declaresFact" $ do
      it "is True when fact is declared" $ do
        declaresFact (named "foo") (pl "foo(a).") `shouldBe` True

      it "is False when fact is not declared" $ do
        declaresFact (named "foo") (pl "foo(a) :- bar(X).") `shouldBe` False
        declaresFact (named "baz") (pl "foo(a).") `shouldBe` False

    describe "declaresRule" $ do
      it "is True when rule is declared" $ do
        declaresRule (named "foo") (pl "foo(X) :- bar(X).") `shouldBe` True

      it "is False when rule is not declared" $ do
        declaresRule (named "baz") (pl "foo(X) :- bar(X).") `shouldBe` False

    describe "uses" $ do
      it "is True when predicate is used, unscoped" $ do
        uses (named "bar") (pl "foo(X) :- bar(X).") `shouldBe` True

      it "is True when predicate is used" $ do
        (uses (named "bar") `scoped` "foo") (pl "foo(X) :- bar(X).") `shouldBe` True

      it "is True when predicate is used in not, unscoped" $ do
        uses (named "bar") (pl "foo(X) :- not(bar(X)).") `shouldBe` True

      it "is True when predicate is used in forall, unscoped" $ do
        uses (named "bar") (pl "foo(X) :- forall(bar(X), baz(X)).") `shouldBe` True
        uses (named "baz") (pl "foo(X) :- forall(bar(X), baz(X)).") `shouldBe` True

      it "is False when predicate is not used" $ do
        (uses (named "bar") `scoped` "foo") (pl "foo(X) :- baz(X).") `shouldBe` False

    describe "usesForall" $ do
      it "is True when used, unscuped" $ do
        usesForall (pl "foo(X) :- forall(f(x), y(X)).") `shouldBe` True

      it "is True when used" $ do
        (usesForall `scoped` "foo") (pl "foo(X) :- forall(bar(X), g(X)).") `shouldBe` True

      it "is False when not used" $ do
        usesForall (pl "foo(X) :- bar(X), baz(X).") `shouldBe` False

    describe "usesNot" $ do
      it "is True when used, unscoped" $ do
        usesNot (pl "foo(X) :- not(f(x)).") `shouldBe` True

      it "is True when used" $ do
        (usesNot `scoped` "foo") (pl "foo(X) :- not(g(X)).") `shouldBe` True

      it "is False when not used" $ do
        usesNot (pl "foo(X) :- bar(X), baz(X).") `shouldBe` False

    describe "usesAnonymousVariable" $ do
      it "is True when _ is used in rule" $ do
        usesAnonymousVariable (pl "foo(_) :- bar(X).") `shouldBe` True

      it "is True when _ is used in fact" $ do
        usesAnonymousVariable (pl "foo(_).") `shouldBe` True

      it "is False when _ is not used" $ do
        usesAnonymousVariable (pl "foo(a).") `shouldBe` False

    describe "declaresPredicate" $ do
      it "is True when rule is declared" $ do
        declaresPredicate (named "foo") (pl "foo(X) :- bar(X).") `shouldBe` True

      it "is True when fact is declared" $ do
        declaresPredicate (named "foo") (pl "foo(tom).") `shouldBe` True

      it "is False when predicate is not declared" $ do
        declaresPredicate (named "foo") (pl "bar(tom).") `shouldBe` False

    describe "declaresComputationWithArity" $ do
      it "is True when fact is declared with given arity" $ do
        declaresComputationWithArity 1 (named "foo") (pl "foo(tom).") `shouldBe` True

      it "is True when rule is declared with given arity" $ do
        declaresComputationWithArity 1 (named "foo") (pl "foo(tom) :- bar(5), baz(6).") `shouldBe` True

      it "is False when fact is declared with another arity" $ do
        declaresComputationWithArity 2 (named "foo") (pl "foo(tom).") `shouldBe` False

    describe "usesUnifyOperator" $ do
      it "is True when equal" $ do
        usesUnifyOperator (pl "baz(X):- X = 4.") `shouldBe` True

      it "is False when no equal" $ do
        usesUnifyOperator (pl "baz(X):- baz(X).") `shouldBe` False

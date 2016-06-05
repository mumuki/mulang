module PrologSpec (spec) where

import           Test.Hspec
import           Prolog
import           Language.Mulang

spec :: Spec
spec = do
  describe "parse" $ do
    it "simplest fact/0" $ do
      pl "foo." `shouldBe` FactDeclaration "foo" []

    it "simplest fact/1" $ do
      pl "foo(bar)." `shouldBe` FactDeclaration "foo" [LiteralPattern "bar"]

    it "multiple facts" $ do
      pl "foo(bar).baz(bar)." `shouldBe` Sequence [FactDeclaration "foo" [],FactDeclaration "baz" []]

    it "simplest fact/2" $ do
      pl "foo(bar,baz)." `shouldBe` FactDeclaration "foo" []

    it "simplest rule/1" $ do
      pl "baz(bar):-foo." `shouldBe` RuleDeclaration "baz" [] []

    it "simplest rule/0" $ do
      pl "baz:-foo." `shouldBe` RuleDeclaration "baz" [] []

    it "simplest rule/1 with condition/1" $ do
      pl "baz(bar):-foo(bar)." `shouldBe` RuleDeclaration "baz" [] []

    it "fact with functors" $ do
      pl "foo(f(bar))." `shouldBe` FactDeclaration "foo" [FunctorPattern "f" [LiteralPattern "bar"]]

    it "fact with functor with variables" $ do
      pl "foo(f(X))." `shouldBe` FactDeclaration "foo" [FunctorPattern "f" [VariablePattern "bar"]]

    it "fact with multiple variables" $ do
      pl "foo(X, Y)." `shouldBe` FactDeclaration "foo" [VariablePattern "X", VariablePattern "Y"]

    it "multiple rules" $ do
      pl "baz(bar) :- foo(bar).foo(bar) :- bar(bar)." `shouldBe` Sequence [
                                                                      RuleDeclaration "baz" [] [],
                                                                      RuleDeclaration "foo" [] []]

    it "simplest rule/2 with condition/2" $ do
      pl "baz(bar,fux):-foo(bar,goo)." `shouldBe` RuleDeclaration "baz" [] []

    it "rule/1 with multiple conditions" $ do
      pl "baz(bar):-foo(bar),goo(bar)." `shouldBe` RuleDeclaration "baz" [] []

    it "rule/1 with multiple mixed conditions" $ do
      pl "baz(bar):-foo(bar),goo(bar),baz." `shouldBe` RuleDeclaration "baz" [] []

    it "rule/1 with whitespeces among coditions" $ do
      pl "baz(bar):-foo(bar) , goo(bar), baz." `shouldBe` RuleDeclaration "baz" [] []

    it "rule/1 with whitespeces among individuals" $ do
      pl "baz(bar):-foo(bar, baz) , goo(bar), baz." `shouldBe` RuleDeclaration "baz" [] []

    it "rule/1 with multiple whitespaces" $ do
      pl "baz(bar) :-\n\
          \    foo(bar),\n\
          \    goo(bar),\n\
          \    baz." `shouldBe` RuleDeclaration "baz" [] []

module PrologSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Prolog

spec :: Spec
spec = do
  describe "parse" $ do
    it "simplest fact/0" $ do
      pl "foo." `shouldBe` FactDeclaration "foo" []

    it "simplest fact/1" $ do
      pl "foo(bar)." `shouldBe` FactDeclaration "foo" [LiteralPattern "bar"]

    it "literal integer" $ do
      pl "foo(3)." `shouldBe` FactDeclaration "foo" [LiteralPattern "3.0"]

    it "literal float" $ do
      pl "foo(3.4)." `shouldBe` FactDeclaration "foo" [LiteralPattern "3.4"]

    it "wildcard" $ do
      pl "foo(_)." `shouldBe` FactDeclaration "foo" [WildcardPattern]

    it "multiple facts" $ do
      pl "foo(bar).baz(bar)." `shouldBe` Sequence [
                                            FactDeclaration "foo" [LiteralPattern "bar"],
                                            FactDeclaration "baz" [LiteralPattern "bar"]]

    it "simplest fact/2" $ do
      pl "foo(bar,baz)." `shouldBe` FactDeclaration "foo" [LiteralPattern "bar", LiteralPattern "baz"]

    it "simplest rule/1" $ do
      pl "baz(bar):-foo." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" []]

    it "simplest rule/0" $ do
      pl "baz:-foo." `shouldBe` RuleDeclaration "baz" [] [Exist "foo" []]

    it "whitespaces are ignored/0" $ do
      pl "baz:-foo(X), baz(X,Y)." `shouldBe` (pl "baz :- foo( X) ,baz( X , Y) .")

    it "simplest rule/1 with condition/1" $ do
      pl "baz(bar):-foo(bar)." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"]]

    it "fact with functors" $ do
      pl "foo(f(bar))." `shouldBe` FactDeclaration "foo" [FunctorPattern "f" [LiteralPattern "bar"]]

    it "fact with functor with variables" $ do
      pl "foo(f(X))." `shouldBe` FactDeclaration "foo" [FunctorPattern "f" [VariablePattern "X"]]

    it "fact with multiple variables" $ do
      pl "foo(X, Y)." `shouldBe` FactDeclaration "foo" [VariablePattern "X", VariablePattern "Y"]

    it "multiple rules" $ do
      pl "baz(bar) :- foo(bar).foo(bar) :- bar(bar)." `shouldBe` Sequence [
                                                                      RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"]],
                                                                      RuleDeclaration "foo" [LiteralPattern "bar"] [Exist "bar" [LiteralPattern "bar"]]]

    it "simplest rule/2 with condition/2" $ do
      pl "baz(bar,fux):-foo(bar,goo)." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar", LiteralPattern "fux"] [Exist "foo" [LiteralPattern "bar", LiteralPattern "goo"]]

    it "rule/1 with not" $ do
      pl "baz(X):-not(bar(X))." `shouldBe` RuleDeclaration "baz" [VariablePattern "X"] [Not (Exist "bar" [VariablePattern "X"])]

    it "rule/1 with forall" $ do
      pl "baz(X):- forall(bar(X), bar(X))." `shouldBe` RuleDeclaration "baz" [VariablePattern "X"] [Forall (Exist "bar" [VariablePattern "X"]) (Exist "bar" [VariablePattern "X"])]

    it "rule/1 with major" $ do
      pl "baz(X):- X > 4." `shouldBe`  RuleDeclaration "baz" [VariablePattern "X"] [Exist ">" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with minor" $ do
      pl "baz(X):- X < 4." `shouldBe`  RuleDeclaration "baz" [VariablePattern "X"] [Exist "<" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with minor equal" $ do
      pl "baz(X):- X =< 4." `shouldBe`  RuleDeclaration "baz" [VariablePattern "X"] [Exist "=<" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with major equal" $ do
      pl "baz(X):- X >= 4." `shouldBe`  RuleDeclaration "baz" [VariablePattern "X"] [Exist ">=" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with distinct" $ do
      pl "baz(X):- X \\= 4." `shouldBe` RuleDeclaration "baz" [VariablePattern "X"] [Exist "\\=" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with unify operator" $ do
      pl "baz(X):- X = 4." `shouldBe` RuleDeclaration "baz" [VariablePattern "X"] [Exist "=" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with simple is" $ do
      pl "baz(X):- X is 4." `shouldBe` RuleDeclaration "baz" [VariablePattern "X"] [Exist "is" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with multiple conditions" $ do
      pl "baz(bar):-foo(bar),goo(bar)." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"]]

    it "rule/1 with multiple mixed conditions" $ do
      pl "baz(bar):-foo(bar),goo(bar),baz." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"],Exist "baz" []]

    it "rule/1 with whitespeces among coditions" $ do
      pl "baz(bar):-foo(bar) , goo(bar), baz." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"],Exist "baz" []]

    it "rule/1 with whitespeces among individuals" $ do
      pl "baz(bar):-foo(bar, baz) , goo(bar), baz." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar",LiteralPattern "baz"],Exist "goo" [LiteralPattern "bar"],Exist "baz" []]

    it "rule/1 with multiple whitespaces" $ do
      pl "baz(bar) :-\n\
          \    foo(bar),\n\
          \    goo(bar),\n\
          \    baz." `shouldBe` RuleDeclaration "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"],Exist "baz" []]

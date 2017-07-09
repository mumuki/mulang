{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module PrologSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Prolog

import           Data.Text (unpack)
import           NeatInterpolation (text)

unpackPl = pl.unpack

spec :: Spec
spec = do
  describe "parse" $ do
    it "simplest fact/0" $ do
      pl "foo." `shouldBe` Fact "foo" []

    it "simplest fact/1" $ do
      pl "foo(bar)." `shouldBe` Fact "foo" [LiteralPattern "bar"]

    it "literal integer" $ do
      pl "foo(3)." `shouldBe` Fact "foo" [LiteralPattern "3.0"]

    it "literal float" $ do
      pl "foo(3.4)." `shouldBe` Fact "foo" [LiteralPattern "3.4"]

    it "wildcard" $ do
      pl "foo(_)." `shouldBe` Fact "foo" [WildcardPattern]

    it "multiple facts" $ do
      pl "foo(bar).baz(bar)." `shouldBe` Sequence [
                                            Fact "foo" [LiteralPattern "bar"],
                                            Fact "baz" [LiteralPattern "bar"]]

    it "multiple facts, whitespaces separated" $ do
      pl "foo(bar). baz(bar)." `shouldBe` Sequence [
                                            Fact "foo" [LiteralPattern "bar"],
                                            Fact "baz" [LiteralPattern "bar"]]

    it "multiple facts, newlines separated" $ do
      pl "foo(bar).\nbaz(bar)." `shouldBe` Sequence [
                                            Fact "foo" [LiteralPattern "bar"],
                                            Fact "baz" [LiteralPattern "bar"]]


    it "simplest fact/2" $ do
      pl "foo(bar,baz)." `shouldBe` Fact "foo" [LiteralPattern "bar", LiteralPattern "baz"]

    it "simplest rule/1" $ do
      pl "baz(bar):-foo." `shouldBe` Rule "baz" [LiteralPattern "bar"] [Exist "foo" []]

    it "simplest rule/0" $ do
      pl "baz:-foo." `shouldBe` Rule "baz" [] [Exist "foo" []]

    it "rules with withiespaces" $ do
      pl "baz:-foo(X), baz(X,Y)." `shouldBe` (pl "baz :- foo( X) ,baz( X , Y) .")

    it "rules with bang" $ do
      pl "baz:-fail,!." `shouldBe` Rule "baz" [] [Exist "fail" [], Exist "!" []]

    it "simplest rule/1 with condition/1" $ do
      pl "baz(bar):-foo(bar)." `shouldBe` Rule "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"]]

    it "fact with functors" $ do
      pl "foo(f(bar))." `shouldBe` Fact "foo" [FunctorPattern "f" [LiteralPattern "bar"]]

    it "fact with functor with variables" $ do
      pl "foo(f(X))." `shouldBe` Fact "foo" [FunctorPattern "f" [VariablePattern "X"]]

    it "fact with multiple variables" $ do
      pl "foo(X, Y)." `shouldBe` Fact "foo" [VariablePattern "X", VariablePattern "Y"]

    it "multiple rules" $ do
      pl "baz(bar) :- foo(bar).foo(bar) :- bar(bar)." `shouldBe` Sequence [
                                                                      Rule "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"]],
                                                                      Rule "foo" [LiteralPattern "bar"] [Exist "bar" [LiteralPattern "bar"]]]

    it "simplest rule/2 with condition/2" $ do
      pl "baz(bar,fux):-foo(bar,goo)." `shouldBe` Rule "baz" [LiteralPattern "bar", LiteralPattern "fux"] [Exist "foo" [LiteralPattern "bar", LiteralPattern "goo"]]

    it "rule/1 with not" $ do
      pl "baz(X):-not(bar(X))." `shouldBe` Rule "baz" [VariablePattern "X"] [Not (Exist "bar" [VariablePattern "X"])]

    it "rules with infix not" $ do
      pl "baz(X):-\\+ bar(X)." `shouldBe` Rule "baz" [VariablePattern "X"] [Not (Exist "bar" [VariablePattern "X"])]


    it "rule/1 with complex not" $ do
      pl "baz(X):-not((bar(X), baz(Y)))." `shouldBe` Rule "baz" [VariablePattern "X"] [Not (Sequence [
                                                                                        Exist "bar" [VariablePattern "X"],
                                                                                        Exist "baz" [VariablePattern "Y"]]) ]


    it "rule/1 with forall" $ do
      pl "baz(X):- forall(bar(X), bar(X))." `shouldBe` Rule "baz" [VariablePattern "X"] [Forall (Exist "bar" [VariablePattern "X"]) (Exist "bar" [VariablePattern "X"])]

    it "rule/1 with findall" $ do
      pl "baz(X):- findall(Y, bar(X, Y), Z)." `shouldBe` Rule "baz" [VariablePattern "X"] [Findall (Exist "Y" []) (Exist "bar" [VariablePattern "X", VariablePattern "Y"]) (Exist "Z" [])]

    it "rule/1 with major" $ do
      pl "baz(X):- X > 4." `shouldBe`  Rule "baz" [VariablePattern "X"] [Exist ">" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with minor" $ do
      pl "baz(X):- X < 4." `shouldBe`  Rule "baz" [VariablePattern "X"] [Exist "<" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with minor equal" $ do
      pl "baz(X):- X =< 4." `shouldBe`  Rule "baz" [VariablePattern "X"] [Exist "=<" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with major equal" $ do
      pl "baz(X):- X >= 4." `shouldBe`  Rule "baz" [VariablePattern "X"] [Exist ">=" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with distinct" $ do
      pl "baz(X):- X \\= 4." `shouldBe` Rule "baz" [VariablePattern "X"] [Exist "\\=" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with unify operator" $ do
      pl "baz(X):- X = 4." `shouldBe` Rule "baz" [VariablePattern "X"] [Exist "=" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with simple is" $ do
      pl "baz(X):- X is 4." `shouldBe` Rule "baz" [VariablePattern "X"] [Exist "is" [VariablePattern "X",LiteralPattern "4.0"]]

    it "rule/1 with is with parenthesis" $ do
      pl "baz(X):- X is (4)." `shouldBe` Rule "baz" [VariablePattern "X"] [Exist "is" [VariablePattern "X",LiteralPattern "4.0"]]

    it "fact/1 with tuples" $ do
      pl "baz((1, 2))." `shouldBe` Fact "baz" [TuplePattern [LiteralPattern "1.0",LiteralPattern "2.0"]]

    it "rule/1 with is and math" $ do
      pl "baz(X, Y):- X is Y - 5." `shouldBe` (Rule "baz"
                                                 [VariablePattern "X",VariablePattern "Y"]
                                                 [Exist "is" [
                                                    VariablePattern "X",
                                                    FunctorPattern "-" [VariablePattern "Y", LiteralPattern "5.0"]]])
    it "rule/1 with is and parenthesis" $ do
      pl "baz(X, Y):- X is (Y / 5) + 20." `shouldBe` (Rule "baz"
                                                        [VariablePattern "X",VariablePattern "Y"]
                                                        [Exist "is" [
                                                          VariablePattern "X",
                                                          FunctorPattern "+" [
                                                            TuplePattern [FunctorPattern "/" [VariablePattern "Y", LiteralPattern "5.0"]],
                                                            LiteralPattern "20.0"]]])

    it "rule/1 with is and functions" $ do
      pl "baz(X, Y):- X is f(Y)." `shouldBe` (Rule "baz"
                                                [VariablePattern "X",VariablePattern "Y"]
                                                [Exist "is" [
                                                  VariablePattern "X",
                                                  FunctorPattern "f" [VariablePattern "Y"]]])

    it "rule/1 with > and math" $ do
      pl "baz(X):- X + 50 > x * 2." `shouldBe` (Rule "baz"
                                                  [VariablePattern "X"]
                                                  [Exist ">" [
                                                    FunctorPattern "+" [VariablePattern "X", LiteralPattern "50.0"],
                                                    FunctorPattern "*" [LiteralPattern "x", LiteralPattern "2.0"]]])

    it "rule/1 with multiple conditions" $ do
      pl "baz(bar):-foo(bar),goo(bar)." `shouldBe` Rule "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"]]

    it "rule/1 with multiple conditions" $ do
      pl "baz(bar):-foo(bar) ; goo(bar)." `shouldBe` Rule "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"]]

    it "rule/1 with multiple mixed conditions" $ do
      pl "baz(bar):-foo(bar),goo(bar),baz." `shouldBe` Rule "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar"],Exist "goo" [LiteralPattern "bar"],Exist "baz" []]

    it "rule/1 with whitespeces among individuals" $ do
      pl "baz(bar):-foo(bar, baz) , goo(bar), baz." `shouldBe` Rule "baz" [LiteralPattern "bar"] [Exist "foo" [LiteralPattern "bar",LiteralPattern "baz"],Exist "goo" [LiteralPattern "bar"],Exist "baz" []]

    it "rule/1 with multiple whitespaces" $ do
      let expression = pl "baz(bar) :- foo(bar), goo(bar), baz."

      pl "baz(bar):-foo(bar) , goo(bar), baz." `shouldBe` expression
      pl "baz(bar) :- foo(bar) , goo(bar) , baz." `shouldBe` expression
      pl "baz(bar) :- foo( bar ) , goo( bar ) , baz." `shouldBe` expression
      pl "baz( bar ) :- foo( bar ) , goo( bar ) , baz." `shouldBe` expression
      pl "baz( bar ) :- \n foo( bar ) , \n goo( bar ), \n  baz." `shouldBe` expression
      pl "baz(bar) :-\n  foo(bar),\n  goo(bar),\n  baz.\n" `shouldBe` expression

    it "sample real code" $ do
      unpackPl [text|
parent(bart, homer).
parent(lisa, homer).
parent(maggie, homer).

parent(homer, abraham).

siblings(One, Another) :-
  parent(One, Parent),
  parent(Other, Parent),
  One \= Parent.
      |] `shouldBe` Sequence [
        Fact "parent" [LiteralPattern "bart",LiteralPattern "homer"],
        Fact "parent" [LiteralPattern "lisa",LiteralPattern "homer"],
        Fact "parent" [LiteralPattern "maggie",LiteralPattern "homer"],
        Fact "parent" [LiteralPattern "homer",LiteralPattern "abraham"],
        Rule "siblings" [VariablePattern "One",VariablePattern "Another"] [
          Exist "parent" [VariablePattern "One",VariablePattern "Parent"],
          Exist "parent" [VariablePattern "Other",VariablePattern "Parent"],
          Exist "\\=" [VariablePattern "One",VariablePattern "Parent"]]]

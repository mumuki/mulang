module GobstonesSmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Generic.Smell
import           Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do
  describe "hasRedundantIf" $ do
    it "is False when there is no if" $ do
      let  code = gbs "program{x := False}"

      hasRedundantIf code `shouldBe` False

    it "is False when there are no literals" $ do
      let code = gbs "function f(){if(m){t := 2}else{t := 4} return (t)}"

      hasRedundantIf code `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in a function" $ do
      let code = gbs "function f(m){return (m == True)}"

      hasRedundantBooleanComparison code `shouldBe` True

    it "is False when no comparison" $ do
      let code = gbs "function f(m){return (m)}"

      hasRedundantBooleanComparison code `shouldBe` False

  describe "hasRedundantLocalVariableReturn" $ do
    it "is True when local variable is not necessary" $ do
      let code = gbs "function f(m) { x  := 5  return (x) }"

      hasRedundantLocalVariableReturn code `shouldBe` True

    it "is False when local variable is not necessary, but there are many variables" $ do
      let code = gbs "function f(m){x:= 5  y:= 2  return (x)}"

      hasRedundantLocalVariableReturn code `shouldBe` False

    it "is False when local variable is necessary in return" $ do
      let code = gbs "function f(m){x := 5  return (x+x)}"

      hasRedundantLocalVariableReturn code `shouldBe` False

    it "is False when local variable is updated" $ do
      let code = gbs "function f(m){ x:= 5  x := x + 1  return (x)}"

      hasRedundantLocalVariableReturn code `shouldBe` False

    it "is False when local variable is used as a cache" $ do
      let code = gbs "function f(m){ x := 5  y := (1+x)  z := g(y)  return (x)}"

      hasRedundantLocalVariableReturn code `shouldBe` False

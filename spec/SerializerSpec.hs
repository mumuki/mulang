module SerializerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Serializer (brace, bracket)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "Serializer" $ do
    it "Serializes non empty asts" $ do
      (brace (js "if (c) 1 else 2")) `shouldBe` "{If{Reference{c}}{MuNumber{1.0}}{MuNumber{2.0}}}"
      (brace (js "({x: true, z: null})")) `shouldBe` "{MuObject{Sequence{Variable{x}{MuBool{True}}}{Variable{z}{MuNil}}}}"
      (brace (js "function f(y) {return x + y}")) `shouldBe` "{Function{f}{Equation{VariablePattern{y}}{UnguardedBody{Return{Application{Primitive{Plus}}{Reference{x}}{Reference{y}}}}}}}"
      (brace (js "function f(y) {return y * x}")) `shouldBe` "{Function{f}{Equation{VariablePattern{y}}{UnguardedBody{Return{Application{Primitive{Multiply}}{Reference{y}}{Reference{x}}}}}}}"

    it "Serializes non empty asts with brackets" $ do
      (bracket (js "if (c) 1 else 2")) `shouldBe` "[If[Reference[c]][MuNumber[1.0]][MuNumber[2.0]]]"

    it "Serializes empty asts" $ do
      (brace (js "")) `shouldBe` "{None}"

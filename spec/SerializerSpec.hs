module SerializerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Serializer (serialize)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "Serializer" $ do
    it "Serializes non empty asts" $ do
      (serialize (js "if (c) 1 else 2")) `shouldBe` "{If{Reference{c}}{MuNumber{1.0}}{MuNumber{2.0}}}"
      (serialize (js "({x: true, z: null})")) `shouldBe` "{MuObject{Sequence{Variable{x}{MuBool{True}}}{Variable{z}{MuNil}}}}"
      (serialize (js "function f(y) {return x + y}")) `shouldBe` "{Function{f}{Equation{VariablePattern{y}}{UnguardedBody{Return{Application{Primitive{Plus}}{Reference{x}}{Reference{y}}}}}}}"
      (serialize (js "function f(y) {return y * x}")) `shouldBe` "{Function{f}{Equation{VariablePattern{y}}{UnguardedBody{Return{Application{Primitive{Multiply}}{Reference{y}}{Reference{x}}}}}}}"

    it "Serializes empty asts" $ do
      (serialize (js "")) `shouldBe` "{None}"

module BuilderSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Builder (merge)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "merge" $ do
    it "merges empty asts" $ do
      (js "" `merge` js "") `shouldBe` None

    it "merges asts with single expressions " $ do
      (js "" `merge` js "var x = 1") `shouldBe` (Variable "x" (MuNumber 1))
      (js "var x = 1" `merge` js "") `shouldBe` (Variable "x" (MuNumber 1))

    it "merges asts with single expressions at both sides " $ do
      (js "var x = 1" `merge` js "var y = 2") `shouldBe` Sequence [Variable "x" (MuNumber 1), Variable "y" (MuNumber 2)]

    it "merges asts with multiple expressions at both sides " $ do
      (js "var x = 1; var y = 2" `merge` js "var z = 3; var k = 4") `shouldBe` Sequence [
        Variable "x" (MuNumber 1),
        Variable "y" (MuNumber 2),
        Variable "z" (MuNumber 3),
        Variable "k" (MuNumber 4)]

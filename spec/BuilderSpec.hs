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
      (js "" `merge` js "let x = 1") `shouldBe` (Variable "x" (MuNumber 1))
      (js "let x = 1" `merge` js "") `shouldBe` (Variable "x" (MuNumber 1))

    it "merges asts with single expressions at both sides " $ do
      (js "let x = 1" `merge` js "let y = 2") `shouldBe` Sequence [Variable "x" (MuNumber 1), Variable "y" (MuNumber 2)]

    it "merges asts with multiple expressions at both sides " $ do
      (js "let x = 1; let y = 2" `merge` js "let z = 3; let k = 4") `shouldBe` Sequence [
        Variable "x" (MuNumber 1),
        Variable "y" (MuNumber 2),
        Variable "z" (MuNumber 3),
        Variable "k" (MuNumber 4)]

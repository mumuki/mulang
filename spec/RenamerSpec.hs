module RenamerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Transform.Renamer (rename)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "rename" $ do
    it "renames empty asts" $ do
      (rename (js "")) `shouldBe` None

    it "renames single var" $ do
      (rename (js "let x = 1")) `shouldBe` (js "let mulang_var_n0 = 1")

    it "renames two vars" $ do
      (rename (js "let x = 1; let y = 2;")) `shouldBe` (js "let mulang_var_n0 = 1; let mulang_var_n1 = 2;")

    it "renames three vars" $ do
      (rename (js "let x = 1; let y = 2; let z = 3;")) `shouldBe` (
        js "let mulang_var_n0 = 1; let mulang_var_n1 = 2; let mulang_var_n2 = 3;")

    it "renames references" $ do
      (rename (js "let x = 1; console.log(x * 2)")) `shouldBe` (js "let mulang_var_n0 = 1; console.log(mulang_var_n0 * 2)")

    it "renames three vars with references" $ do
      (rename (js "let x = 1; let y = 2; let z = x + f(y);")) `shouldBe` (
        js "let mulang_var_n0 = 1; let mulang_var_n1 = 2; let mulang_var_n2 = mulang_var_n0 + f(mulang_var_n1);")

    it "does not rename unknown references" $ do
      (rename (js "console.log(x * 2)")) `shouldBe` (js "console.log(x * 2)")

    it "renames single param" $ do
      (rename (js "function f(x) {}")) `shouldBe` (js "function f(mulang_param_n0) {}")

    it "renames multiple param" $ do
      (rename (js "function f(x, y) {}")) `shouldBe` (js "function f(mulang_param_n0, mulang_param_n1) {}")

    it "renames multiple params with references" $ do
      (rename (js "function f(x, y) { return x + y }")) `shouldBe` (
        js "function f(mulang_param_n0, mulang_param_n1) { return mulang_param_n0 + mulang_param_n1 }")

    it "renames multiple params with mixed references" $ do
      (rename (js "let y = 0; function f(x) { return x + y }")) `shouldBe` (
        js "let mulang_var_n0 = 0; function f(mulang_param_n0) { return mulang_param_n0 + mulang_var_n0 }")

    it "resets references renames across multiple computations" $ do
      (rename (js "function f(x) { return 2 * x }; function g(x) { return 2 * x };")) `shouldBe` (
        js "function f(mulang_param_n0) { return 2 * mulang_param_n0 }; function g(mulang_param_n0) { return 2 * mulang_param_n0 }")

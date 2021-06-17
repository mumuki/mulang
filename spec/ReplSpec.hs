module ReplSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Interpreter.Repl
import           Language.Mulang.Interpreter.Internals (Value (..))

spec :: Spec
spec = do
  describe "repl" $ do
    let s0 = newSession js
    it "evals and returns" $ do
      (r1, _) <- repl "1" s0
      r1 `shouldBe` (MuNumber 1)

    it "can eval multiple statements and return" $ do
      (r1, s1) <- repl "var x = 1" s0
      r1 `shouldBe` (MuNumber 1)

      (r2, s2) <- repl "x + 3" s1
      r2 `shouldBe` (MuNumber 4)

      (_, s3) <- repl "function double(x) { return x * 2 }" s2
      (r4, _) <- repl "double(x)" s3
      r4 `shouldBe` (MuNumber 2)

    it "can save state" $ do
      (_, s1) <- repl "function succ(x) { return x + 1 }" s0
      (_, s2) <- repl "function pred(x) { return x - 1 }" s1

      (r, _) <- repl "succ(succ(pred(10)))" (reload s2)
      r `shouldBe` (MuNumber 11)


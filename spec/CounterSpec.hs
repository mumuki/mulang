module CounterSpec (spec) where

import           Test.Hspec
import           Language.Mulang hiding (Equal, NotEqual)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "countIfs" $ do
    it "counts 0" $ do
      (unmatching countIfs) (js "") `shouldBe` 0

    it "counts 1" $ do
      (unmatching countIfs) (js "if (true) {}") `shouldBe` 1
      (unmatching countIfs) (js "if (true) {} else {}" ) `shouldBe` 1

    it "counts 2 or more" $ do
      (unmatching countIfs) (js "if (true) {} else {}\n\
                                \if (true) {} else {}" ) `shouldBe` 2
      (unmatching countIfs) (js "if (true) {} else {}\n\
                                \if (true) {} else {}\n\
                                \if (true) {}\n" ) `shouldBe` 3

    it "counts across procedures" $ do
      (unmatching countIfs) (js "function f1() {if (true) {} else {}}\n\
                                \function f2() {if (true) {} else {}}\n" ) `shouldBe` 2

  describe "countReturns" $ do
    it "counts 0" $ do
      (unmatching countReturns) (js "") `shouldBe` 0
      (unmatching countReturns) (js "function foo(){}") `shouldBe` 0

    it "counts 1" $ do
      (unmatching countReturns) (js "function foo() { return }" ) `shouldBe` 1
      (unmatching countReturns) (js "function foo() { return } function foo() { }") `shouldBe` 1

    it "counts 2 or more" $ do
      (unmatching countReturns) (js "function foo() { return }\n\
                                    \function bar() { return 4 }" ) `shouldBe` 2

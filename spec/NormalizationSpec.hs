module NormalizationSpec (spec) where

  import           Test.Hspec
  import           Language.Mulang.Parsers.Haskell (hs)
  import           Language.Mulang.Parsers.JavaScript (js)

  spec :: Spec
  spec = do
    describe "sorts declarations by default" $ do
      it "sorts declarations on haskell" $ do
        hs "f 1 = 1\ng 2 = 2" `shouldBe` hs "g 2 = 2\nf 1 = 1"

      it "sorts declarations on javascript if there are only declarations" $ do
        js "function f() {} function g() {}" `shouldBe` js "function g() {} function f() {}"

      it "doesn't sort declarations on javascript if there are also statements" $ do
        js "function f() {}; var x = 2; function g() {}" `shouldNotBe` js "function g() {}; var x = 2; function f() {}"

module NormalizationSpec (spec) where

  import           Test.Hspec
  import           Language.Mulang.Parsers.Java (java)
  import           Language.Mulang.Parsers.Python (py)
  import           Language.Mulang.Parsers.Haskell (hs)
  import           Language.Mulang.Parsers.JavaScript (js)

  spec :: Spec
  spec = do
    describe "sorts declarations by default" $ do
      it "sorts functions on Python" $ do
        py "def foo():\n  return 2\n\ndef bar():\n  return 1\n\n" `shouldBe` py "def bar():\n  return 1\n\ndef foo():\n  return 2\n\n"

      it "sorts classes on Java" $ do
        java "class Foo {} class Bar {}" `shouldBe` java "class Bar {} class Foo {}"

      it "sorts functions on haskell" $ do
        hs "f 1 = 1\ng 2 = 2" `shouldBe` hs "g 2 = 2\nf 1 = 1"

      it "sorts declarations on haskell even when there are variables" $ do
        hs "n = 1\nf 1 = 1\ng 2 = 2" `shouldBe` hs "g 2 = 2\nf 1 = 1\nn = 1"

      it "sorts functions on javascript if there are only functions" $ do
        js "function f() {} function g() {}" `shouldBe` js "function g() {} function f() {}"

      it "doesn't sort functions on javascript if there are duplicates names" $ do
        js "function f() { return 1 } function g() {} function f() { return 2 }" `shouldNotBe` js "function g() {} function f() { return 2 } function f() { return 1 } "

      it "doesn't sort declarations on javascript if there are also statements" $ do
        js "function f() {}; var x = 2; function g() {}" `shouldNotBe` js "function g() {}; var x = 2; function f() {}"

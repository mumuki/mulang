module NormalizerSpec (spec) where

  import           Test.Hspec
  import           Language.Mulang.Ast
  import           Language.Mulang.Ast.Operator
  import           Language.Mulang.Parsers.Haskell (hs)
  import           Language.Mulang.Parsers.Java (java)
  import           Language.Mulang.Parsers.JavaScript (js)
  import           Language.Mulang.Parsers.Python (py)
  import           Language.Mulang.Normalizers.Java (javaNormalizationOptions)
  import           Language.Mulang.Normalizers.Python (pythonNormalizationOptions)
  import           Language.Mulang.Normalizers.Haskell (haskellNormalizationOptions)
  import           Language.Mulang.Transform.Normalizer

  njava = normalize javaNormalizationOptions . java
  npy = normalize pythonNormalizationOptions . py
  nhs = normalize haskellNormalizationOptions . hs

  spec :: Spec
  spec = do
    describe "can insert implicit retuns" $ do
      let options = defaultNormalizationOptions { insertImplicitReturn = True}

      it "does not insert return in single literal statement" $ do
        normalize options (py "def x(): x = 1") `shouldBe`  SimpleProcedure "x" [] (Assignment "x" (MuNumber 1.0))

      it "inserts return in single literal expression" $ do
        normalize options (py "def x(): 3") `shouldBe`  SimpleProcedure "x" [] (Return (MuNumber 3.0))

      it "inserts return in last literal expression" $ do
        normalize options (js "function x() { let x = 1; x += 1; x }") `shouldBe`  SimpleProcedure "x" [] (Sequence [
                                                                                          Variable "x" (MuNumber 1.0),
                                                                                          Assignment "x" (Application (Primitive Plus) [Reference "x",MuNumber 1.0]),
                                                                                          Return (Reference "x")])

    describe "sorts declarations by default" $ do
      it "sorts functions on Python" $ do
        npy "def foo():\n  return 2\n\ndef bar():\n  return 1\n\n" `shouldBe` py "def bar():\n  return 1\n\ndef foo():\n  return 2\n\n"

      it "sorts classes on Java" $ do
        njava "class Foo {} class Bar {}" `shouldBe` java "class Bar {} class Foo {}"

      it "sorts functions on haskell" $ do
        nhs "g 2 = 2\nf 1 = 1" `shouldBe` hs "f 1 = 1\ng 2 = 2"

      it "sorts declarations on haskell even when there are variables" $ do
        nhs "g 2 = 2\nf 1 = 1\nn = 1" `shouldBe` hs "f 1 = 1\ng 2 = 2\nn = 1"

      it "sorts functions on javascript if there are only functions" $ do
        js "function f() {} function g() {}" `shouldBe` js "function g() {} function f() {}"

      it "doesn't sort functions on javascript if there are duplicates names" $ do
        js "function f() { return 1 } function g() {} function f() { return 2 }" `shouldNotBe` js "function g() {} function f() { return 2 } function f() { return 1 } "

      it "doesn't sort declarations on javascript if there are also statements" $ do
        js "function f() {}; let x = 2; function g() {}" `shouldNotBe` js "function g() {}; let x = 2; function f() {}"

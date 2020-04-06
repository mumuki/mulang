{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ProceduralSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Identifier
import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Literal
import           Language.Mulang.Inspector.Matcher
import           Language.Mulang.Inspector.Procedural

spec :: Spec
spec = do
  describe "usesForLoop" $ do
    it "is True when present in function" $ do
      usesForLoop (js "function f() { for(;;) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in lambda" $ do
      usesForLoop (js "var f = function() { for(;;) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in object" $ do
      usesForLoop (js "var x = {f: function() { for(;;) { console.log('foo') } }}")  `shouldBe` True

    it "is True when present in method" $ do
      usesForLoop (js "var o = {f: function() { for(;;) { console.log('foo') }  }}")  `shouldBe` True

    it "is False when not present in function" $ do
      usesForLoop (js "function f() {}")  `shouldBe` False

  describe "usesLoop" $ do
    it "is True when repeat is present" $ do
      let code = SimpleFunction "f" [] (Sequence [Repeat (MuNumber 2) None, Return (MuNumber 2)])

      usesLoop code `shouldBe` True

    it "is True when foreach is present" $ do
      let code = SimpleFunction "f" [] (Sequence [
                                          For [Generator (VariablePattern "x") (MuList [MuNumber 2])] None,
                                          Return (MuNumber 2)
                                        ])

      usesLoop code `shouldBe` True

    it "is True when for is present" $ do
      let code = js "function f() { for(;;); }"

      usesLoop code `shouldBe` True

    it "is True when while is present" $ do
      let code = js "function f() { while(true); }"

      usesLoop code `shouldBe` True

    it "is True when a for-of is present" $ do
      let code = js "function printAll(list) { for (let e of list) { console.log(e) } }"

      usesForEach code `shouldBe` True
      usesLoop code `shouldBe` True

    it "is False when none of the aforementioned are present" $ do
      let code = js "function f(x){return 1;}"

      usesLoop code `shouldBe` False

  describe "usesRepeat" $ do
    it "is True when present in function" $ do
      let code = SimpleFunction "f" [] (Sequence [Repeat (MuNumber 2) None, Return (MuNumber 2)])

      usesRepeat code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = js "function f(x){return 1;}"

      usesRepeat code  `shouldBe` False


  describe "usesSwitch" $ do
    it "is True when present in function" $ do
      let code = Switch (Reference "x") [(None, MuNumber 0)] None

      usesSwitch code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = js "function f(x){return 1;}"

      usesSwitch code  `shouldBe` False

  describe "declaresProcedure" $ do
    describe "with procedure declarations" $ do
      it "is True when procedure is declared" $ do
        let code =  js "function f(){}"

        declaresProcedure (named "f") code `shouldBe` True

      it "is False when procedures is not declared" $ do
        let code = js "function f(){}"

        declaresProcedure (named "g") code `shouldBe` False

      it "is False when using a matcher and procedure does not have a body" $ do
        (declaresProcedureMatching (with . isNumber $ 2) anyone) (js "function f() {}") `shouldBe` False

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      usesWhile (js "function f() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in lambda" $ do
      usesWhile (js "var f = function() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in object" $ do
      usesWhile (js "var x = {f: function() { while(true) { console.log('foo') } }}")  `shouldBe` True

    it "is True when present in method" $ do
      usesWhile (js "var o = {f: function() { while(true) { console.log('foo') }  }}")  `shouldBe` True

    it "is False when not present in function" $ do
      usesWhile (js "function f() {}")  `shouldBe` False

module GobstonesInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Binding
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do
  describe "declaresEntryPoint" $ do
    describe "with program declarations" $ do
      it "is True when program is declared" $ do
        let code = gbs "program{ Poner(Verde) }"

        declaresEntryPoint anyone code `shouldBe` True

      it "is False when program is not declared" $ do
        let code = gbs "procedure F(){}"

        declaresEntryPoint anyone code `shouldBe` False

  describe "declaresProcedure" $ do
    describe "with procedure declarations" $ do
      it "is True when procedure is declared" $ do
        let code =  gbs "procedure F(){}"

        declaresProcedure (named "F") code `shouldBe` True

      it "is True when any procedures is declared" $ do
        let code = gbs "procedure F(){}"

        declaresProcedure anyone code `shouldBe` True

      it "is False when procedures is not declared" $ do
        let code = gbs "procedure F(){}"

        declaresProcedure (named "G") code `shouldBe` False

  describe "declaresFunction" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        let code = gbs "function f(){return (1)}"

        declaresFunction (named "f") code `shouldBe` True

      it "is True when any functions is declared" $ do
        let code = gbs "function f(){return (1)}"

        declaresFunction anyone code `shouldBe` True

      it "is False when functions is not declared" $ do
        let code = gbs "function f(){return (1)}"

        declaresFunction (named "g") code `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        let code = gbs "program { f := 2}"

        declaresFunction (named "f") code `shouldBe` False

      it "is False when constant is declared with a number literal" $ do
        let code = gbs "program {f := 3}"

        declaresFunction  (named "f") code `shouldBe` False

  describe "declaresComputationWithExactArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        let code = gbs "function f(x){return (x+1)}"

        (declaresComputationWithExactArity 1) (named "f") code `shouldBe` True

      it "is False when function is declared with another arity" $ do
        let code = gbs "function f(x){return (x+1)}"

        (declaresComputationWithExactArity 2) (named "f") code `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        let code = gbs "function f(x,y){return (x+y)}"

        (declaresComputationWithExactArity 2) (named "f") code `shouldBe` True

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      let code = gbs "function f(){while(True){} return (x)}"

      usesWhile code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = gbs "function f(x){return (1)}"

      usesWhile code  `shouldBe` False

  describe "usesIf" $ do
    it "is True when present in function" $ do
      let code = gbs "function f(){if(True){}else{} return (x)}"

      usesIf code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = gbs "function f(x){return (1)}"

      usesIf code  `shouldBe` False

  describe "usesSwitch" $ do
    it "is True when present in function" $ do
      let code = gbs "function f(x) {switch (2) to { 2 -> {x := 2}} return (x)}"

      usesSwitch code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = gbs "function f(x){return (1)}"

      usesSwitch code  `shouldBe` False

  describe "usesRepeat" $ do
    it "is True when present in function" $ do
      let code = gbs "function f(){repeat(2){} return (x)}"

      usesRepeat code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = gbs "function f(x){return (1)}"

      usesRepeat code  `shouldBe` False

  describe "uses" $ do
    it "is True when function application is used within function" $ do
      let code = gbs "function f() { return (m()) }"

      uses (named "m") code  `shouldBe` True

      let code = gbs "procedure F() { M() }"

      uses (named "M")  code  `shouldBe` True

      let code = gbs "function f(){ return (m(x()))}"

      uses (named "x") code  `shouldBe` True

    it "is True through function application in function" $ do
      let code = gbs "procedure F(x){ G() } procedure G(){ M()} "

      transitive (uses (named "M")) "F" code `shouldBe` True

    it "is True through function application in function" $ do
      let code = gbs "procedure F(x) { G(2) } procedure G(p) { M() }"

      transitive (uses (named "M")) "F" code `shouldBe` True

    it "is False through function application in function" $ do
      let code = gbs "procedure F(x){ G() } procedure G(){}"

      transitive (uses (named "M")) "F" code `shouldBe` False

  describe "declaresVariable" $ do
      it "is True when declare a variable" $ do
        let code = gbs "procedure F(){ x := 2}"

        declaresVariable (named "x") code `shouldBe` True

      it "is True when any variable is declared" $ do
        let code = gbs "procedure F(){ x := 2}"

        declaresVariable anyone code `shouldBe` True

      it "is False when variable is not declared" $ do
        let code = gbs "procedure F(){ x := 2}"

        declaresVariable (named "y") code `shouldBe` False
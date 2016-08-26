module GobstonesInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do
  describe "declaresFunction" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        declaresFunction (named "f") (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]") `shouldBe` True

      it "is True when any functions is declared" $ do
        declaresFunction anyone (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]") `shouldBe` True

      it "is False when functions is not declared" $ do
        declaresFunction (named "g") (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]") `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction (named "f") (gbs "[{\"alias\": \"program\",\"body\": [  {    \"alias\": \":=\",\"arity\": \"binary\",\"variable\": {      \"value\": \"f\",      \"arity\": \"name\"    },\"expression\": {      \"value\": 2,      \"arity\": \"literal\"    }  }]}\r\n]") `shouldBe` False

      it "is False when constant is declared with a number literal" $ do
        declaresFunction  (named "f") (gbs "[{\"alias\": \"program\",\"body\": [  {    \"alias\": \":=\",\"arity\": \"binary\",\"variable\": {      \"value\": \"f\",      \"arity\": \"name\"    },\"expression\": {      \"value\": 3,      \"arity\": \"literal\"    }  }]}\r\n]") `shouldBe` False

  describe "declaresComputationWithExactArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresComputationWithExactArity 1) (named "f") (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": \"+\",  \"arity\": \"binary\",  \"left\": {    \"value\": \"x\",\"arity\": \"name\"  },  \"right\": {    \"value\": 1,\"arity\": \"literal\"  }}}\r\n]") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresComputationWithExactArity 2) (named "f") (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": \"+\",  \"arity\": \"binary\",  \"left\": {    \"value\": \"x\",\"arity\": \"name\"  },  \"right\": {    \"value\": 1,\"arity\": \"literal\"  }}}\r\n]") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresComputationWithExactArity 2) (named "f") (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  },  {    \"value\": \"y\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": \"+\",  \"arity\": \"binary\",  \"left\": {    \"value\": \"x\",\"arity\": \"name\"  },  \"right\": {    \"value\": \"y\",\"arity\": \"name\"  }}}\r\n]") `shouldBe` True

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      usesWhile (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [],\"body\": [  {    \"alias\": \"while\",\"expression\": {      \"value\": true,      \"arity\": \"literal\",      \"reserved\": true    },\"body\": null  }],\"return\": {  \"value\": \"x\",  \"arity\": \"name\"}}\r\n]")  `shouldBe` True
  
    it "is False when not present in function" $ do
      usesWhile (gbs "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]")  `shouldBe` False

  describe "uses" $ do
    it "is True when function application is used within function" $ do
      --  code = "function f() { return (m()) }"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [],\"body\": [],\"return\": {  \"alias\": \"functionCall\",  \"name\": \"m\",  \"parameters\": []}}\r\n]"
      uses (named "m")  (gbs code)  `shouldBe` True

      --  code = "procedure F() { M() }"
      let code = "[{\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"alias\": \"procedureDeclaration\",\"parameters\": [],\"body\": [  {    \"arity\": \"routine\",\"alias\": \"ProcedureCall\",\"name\": \"M\",\"parameters\": []  }]}\r\n]\r\n\r\n"
      uses (named "M")  (gbs code)  `shouldBe` True

    --it "is True through function application in function" $ do
      --transitive (uses (named "m")) "f" (gbs "function g() { m }; function f(x) { g() }") `shouldBe` True

    --it "is True through function application in function" $ do
      --transitive (uses (named "m")) "f" (gbs "function g(p) { return m }; function f(x) { return g(2) }") `shouldBe` True

    it "is False through function application in function" $ do
      transitive (uses (named "m")) "f" (gbs "[{\"value\": \"G\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"G\",\"alias\": \"procedureDeclaration\",\"parameters\": [],\"body\": null},{\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"alias\": \"procedureDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [  {    \"arity\": \"routine\",\"alias\": \"ProcedureCall\",\"name\": \"G\",\"parameters\": []  }]}\r\n]") `shouldBe` False


    
  -- que ande el usesIf, el usesWhile, usesRepeat, usesSwitch, uses (retornos, parametro, program,function, procedure), declaresProgram, declaresProcedure, declaresVariable? y probar transitividad para algun caso 
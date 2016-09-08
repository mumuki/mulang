module GobstonesInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do
  describe "declaresProcedure" $ do
    describe "with procedure declarations" $ do
      it "is True when procedure is declared" $ do
        --  code = "procedure F(){}"
        let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": null\r\n  }\r\n]"
        declaresProcedure (named "F") (gbs code) `shouldBe` True

      it "is True when any procedures is declared" $ do
        --  code = "procedure F(){}"
        let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": null\r\n  }\r\n]"
        declaresProcedure anyone (gbs code) `shouldBe` True

      it "is False when procedures is not declared" $ do
        --  code = "procedure F(){}"
        let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": null\r\n  }\r\n]"
        declaresProcedure (named "G") (gbs code) `shouldBe` False

  describe "declaresFunction" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        --  code = "function f(){return (1)}"
        let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
        declaresFunction (named "f") (gbs code) `shouldBe` True

      it "is True when any functions is declared" $ do
        --  code = "function f(){return (1)}"
        let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
        declaresFunction anyone (gbs code) `shouldBe` True

      it "is False when functions is not declared" $ do
        --  code = "function f(){return (1)}"
        let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
        declaresFunction (named "g") (gbs code) `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        --  code = "program { f := 2}"
        let code = "[{\"alias\": \"program\",\"body\": [  {    \"alias\": \":=\",\"arity\": \"binary\",\"variable\": {      \"value\": \"f\",      \"arity\": \"name\"    },\"expression\": {      \"value\": 2,      \"arity\": \"literal\"    }  }]}\r\n]"
        declaresFunction (named "f") (gbs code) `shouldBe` False

      it "is False when constant is declared with a number literal" $ do
        --  code = "orogram {f := 3}"
        let code = "[{\"alias\": \"program\",\"body\": [  {    \"alias\": \":=\",\"arity\": \"binary\",\"variable\": {      \"value\": \"f\",      \"arity\": \"name\"    },\"expression\": {      \"value\": 3,      \"arity\": \"literal\"    }  }]}\r\n]"
        declaresFunction  (named "f") (gbs code) `shouldBe` False

  describe "declaresComputationWithExactArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        --  code = "function f(x){return (x+1)}"
        let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": \"+\",  \"arity\": \"binary\",  \"left\": {    \"value\": \"x\",\"arity\": \"name\"  },  \"right\": {    \"value\": 1,\"arity\": \"literal\"  }}}\r\n]"
        (declaresComputationWithExactArity 1) (named "f") (gbs code) `shouldBe` True

      it "is False when function is declared with another arity" $ do
        --  code = "function f(x){return (x+1)}"
        let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": \"+\",  \"arity\": \"binary\",  \"left\": {    \"value\": \"x\",\"arity\": \"name\"  },  \"right\": {    \"value\": 1,\"arity\": \"literal\"  }}}\r\n]"
        (declaresComputationWithExactArity 2) (named "f") (gbs code) `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        --  code = "function f(x,y){return (x+y)}"
        let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  },  {    \"value\": \"y\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": \"+\",  \"arity\": \"binary\",  \"left\": {    \"value\": \"x\",\"arity\": \"name\"  },  \"right\": {    \"value\": \"y\",\"arity\": \"name\"  }}}\r\n]"
        (declaresComputationWithExactArity 2) (named "f") (gbs code) `shouldBe` True

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      -- code = "function f(){while(True){} return (x)}"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [],\"body\": [  {    \"alias\": \"while\",\"expression\": {      \"value\": true,      \"arity\": \"literal\",      \"reserved\": true    },\"body\": null  }],\"return\": {  \"value\": \"x\",  \"arity\": \"name\"}}\r\n]"
      usesWhile (gbs code)  `shouldBe` True
  
    it "is False when not present in function" $ do
      -- code = "function f(x){return (1)}"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
      usesWhile (gbs code)  `shouldBe` False

  describe "usesIf" $ do
    it "is True when present in function" $ do
      -- code = "function f(){if(True){}else{} return (x)}"
      let code = "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \"conditional\",\r\n        \"condition\": {\r\n          \"value\": true,\r\n          \"arity\": \"literal\",\r\n          \"reserved\": true\r\n        },\r\n        \"left\": null,\r\n        \"right\": null\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      usesIf (gbs code)  `shouldBe` True
  
    it "is False when not present in function" $ do
      --  code = "function f(x){return (1)}"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
      usesIf (gbs code)  `shouldBe` False

  describe "usesSwitch" $ do
    it "is True when present in function" $ do
      --  code = "program {switch (2) to { 2 -> {x := 2}} }"
      let code = "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"switch\",\"value\" : {\"value\" : 2 , \"arity\" : \"literal\"} ,\"cases\" : [{\"case\" : {\"value\" : 2 , \"arity\" : \"literal\"} , \"body\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" , \"variable\" : {\"value\" : \"x\" , \"arity\" : \"name\"}, \"expression\" :{\"value\" : 2, \"arity\" : \"literal\"} }] }]}],\"from\": 0}]"
      usesSwitch (gbs code)  `shouldBe` True
  
    it "is False when not present in function" $ do
      --  code = "function f(x){return (1)}"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
      usesSwitch (gbs code)  `shouldBe` False

  describe "usesRepeat" $ do
    it "is True when present in function" $ do
      -- code = "function f(){repeat(2){} return (x)}"
      let code = "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \"repeat\",\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        },\r\n        \"body\": null\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      usesRepeat (gbs code)  `shouldBe` True
  
    it "is False when not present in function" $ do
      -- code = "function f(x){return (1)}"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [],\"return\": {  \"value\": 1,  \"arity\": \"literal\"}}\r\n]"
      usesRepeat (gbs code)  `shouldBe` False

  describe "uses" $ do
    it "is True when function application is used within function" $ do
      --  code = "function f() { return (m()) }"
      let code = "[{\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"alias\": \"functionDeclaration\",\"parameters\": [],\"body\": [],\"return\": {  \"alias\": \"functionCall\",  \"name\": \"m\",  \"parameters\": []}}\r\n]"
      uses (named "m")  (gbs code)  `shouldBe` True

      --  code = "procedure F() { M() }"
      let code = "[{\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"alias\": \"procedureDeclaration\",\"parameters\": [],\"body\": [  {    \"arity\": \"routine\",\"alias\": \"ProcedureCall\",\"name\": \"M\",\"parameters\": []  }]}\r\n]\r\n\r\n"
      uses (named "M")  (gbs code)  `shouldBe` True

      -- code = "function f(){ return (m(x()))}"
      let code = "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"alias\": \"functionCall\",\r\n      \"name\": \"m\",\r\n      \"parameters\": [\r\n        {\r\n          \"alias\": \"functionCall\",\r\n          \"name\": \"x\",\r\n          \"parameters\": []\r\n        }\r\n      ]\r\n    }\r\n  }\r\n]"
      uses (named "x") (gbs code)  `shouldBe` True

    it "is True through function application in function" $ do 
      --  code = "procedure G(){ M()} procedure F(x){ G() }"
      let code = "[\r\n  {\r\n    \"value\": \"G\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"G\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"arity\": \"routine\",\r\n        \"alias\": \"ProcedureCall\",\r\n        \"name\": \"M\",\r\n        \"parameters\": []\r\n      }\r\n    ]\r\n  },\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"arity\": \"routine\",\r\n        \"alias\": \"ProcedureCall\",\r\n        \"name\": \"G\",\r\n        \"parameters\": []\r\n      }\r\n    ]\r\n  }\r\n]"
      transitive (uses (named "M")) "F" (gbs code) `shouldBe` True

    it "is True through function application in function" $ do
      --  code = "procedure G(p) { M() } procedure F(x) { G(2) }"
      let code = "[\r\n  {\r\n    \"value\": \"G\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"G\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"p\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"arity\": \"routine\",\r\n        \"alias\": \"ProcedureCall\",\r\n        \"name\": \"M\",\r\n        \"parameters\": []\r\n      }\r\n    ]\r\n  },\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"arity\": \"routine\",\r\n        \"alias\": \"ProcedureCall\",\r\n        \"name\": \"G\",\r\n        \"parameters\": [\r\n          {\r\n            \"value\": 2,\r\n            \"arity\": \"literal\"\r\n          }\r\n        ]\r\n      }\r\n    ]\r\n  }\r\n]"
      transitive (uses (named "M")) "F" (gbs code) `shouldBe` True

    it "is False through function application in function" $ do
      --  code = "procedure G(){} procedure F(x){ G() }"
      let code = "[{\"value\": \"G\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"G\",\"alias\": \"procedureDeclaration\",\"parameters\": [],\"body\": null},{\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"alias\": \"procedureDeclaration\",\"parameters\": [  {    \"value\": \"x\",\"arity\": \"name\"  }],\"body\": [  {    \"arity\": \"routine\",\"alias\": \"ProcedureCall\",\"name\": \"G\",\"parameters\": []  }]}\r\n]"
      transitive (uses (named "M")) "F" (gbs code) `shouldBe` False

  describe "declaresVariable" $ do
      it "is True when declare a variable" $ do
        --  code = "procedure F(){ x := 2}"
        let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]"
        declaresVariable (named "x") (gbs code) `shouldBe` True

      it "is True when any variable is declared" $ do
        --  code = "procedure F(){ x := 2}"
        let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]"
        declaresVariable anyone (gbs code) `shouldBe` True

      it "is False when variable is not declared" $ do
        --  code = "procedure F(){ x := 2}"
        let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]"
        declaresVariable (named "y") (gbs code) `shouldBe` False
  

    
  -- uses (retornos, parametro, program), declaresProgram
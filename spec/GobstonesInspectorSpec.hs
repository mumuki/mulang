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
        declaresFunction (named "f") (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": 1,\r\n      \"arity\": \"literal\"\r\n    }\r\n  }\r\n]") `shouldBe` True

      it "is True when any functions is declared" $ do
        declaresFunction anyone (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": 1,\r\n      \"arity\": \"literal\"\r\n    }\r\n  }\r\n]") `shouldBe` True

      it "is False when functions is not declared" $ do
        declaresFunction (named "g") (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": 1,\r\n      \"arity\": \"literal\"\r\n    }\r\n  }\r\n]") `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction (named "f") (gbs "[\r\n  {\r\n    \"alias\": \"program\",\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"f\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]") `shouldBe` False

      it "is False when constant is declared with a number literal" $ do
        declaresFunction  (named "f") (gbs "[\r\n  {\r\n    \"alias\": \"program\",\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"f\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 3,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]") `shouldBe` False

  describe "declaresComputationWithExactArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresComputationWithExactArity 1) (named "f") (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"+\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": 1,\r\n        \"arity\": \"literal\"\r\n      }\r\n    }\r\n  }\r\n]") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresComputationWithExactArity 2) (named "f") (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"+\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": 1,\r\n        \"arity\": \"literal\"\r\n      }\r\n    }\r\n  }\r\n]") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresComputationWithExactArity 2) (named "f") (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      {\r\n        \"value\": \"y\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"+\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": \"y\",\r\n        \"arity\": \"name\"\r\n      }\r\n    }\r\n  }\r\n]") `shouldBe` True

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      usesWhile (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \"while\",\r\n        \"expression\": {\r\n          \"value\": true,\r\n          \"arity\": \"literal\",\r\n          \"reserved\": true\r\n        },\r\n        \"body\": null\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]")  `shouldBe` True
  
    it "is False when not present in function" $ do
      usesWhile (gbs "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": 1,\r\n      \"arity\": \"literal\"\r\n    }\r\n  }\r\n]")  `shouldBe` False

  describe "uses" $ do
    it "is True when function application is used within function" $ do
      --  code = "function f() { return (m()) }"
      let code = "[\r\n  {\r\n    \"value\": \"f\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"f\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"alias\": \"functionCall\",\r\n      \"name\": \"m\",\r\n      \"parameters\": []\r\n    }\r\n  }\r\n]"
      uses (named "m")  (gbs code)  `shouldBe` True

      --  code = "procedure F() { M() }"
      let code = "[\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"arity\": \"routine\",\r\n        \"alias\": \"ProcedureCall\",\r\n        \"name\": \"M\",\r\n        \"parameters\": []\r\n      }\r\n    ]\r\n  }\r\n]\r\n\r\n"
      uses (named "M")  (gbs code)  `shouldBe` True

    --it "is True through function application in function" $ do
      --transitive (uses (named "m")) "f" (gbs "function g() { m }; function f(x) { g() }") `shouldBe` True

    --it "is True through function application in function" $ do
      --transitive (uses (named "m")) "f" (gbs "function g(p) { return m }; function f(x) { return g(2) }") `shouldBe` True

    it "is False through function application in function" $ do
      transitive (uses (named "m")) "f" (gbs "[\r\n  {\r\n    \"value\": \"G\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"G\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": null\r\n  },\r\n  {\r\n    \"value\": \"F\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"F\",\r\n    \"alias\": \"procedureDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"arity\": \"routine\",\r\n        \"alias\": \"ProcedureCall\",\r\n        \"name\": \"G\",\r\n        \"parameters\": []\r\n      }\r\n    ]\r\n  }\r\n]") `shouldBe` False


    
    
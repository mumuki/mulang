module GobstonesSmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do
  describe "hasRedundantIf" $ do
    it "is False when there is no if" $ do
      --  code = "program{x := False}"
      let code = "[\r\n  {\r\n    \"alias\": \"program\",\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": false,\r\n          \"arity\": \"literal\",\r\n          \"reserved\": true\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]"
      hasRedundantIf (gbs code) `shouldBe` False

    it "is False when there are no literals" $ do
      --  code = "function x(){if(m){t := 2}else{t := 4} return (t)}"
      let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \"conditional\",\r\n        \"condition\": {\r\n          \"value\": \"m\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"left\": [\r\n          {\r\n            \"alias\": \":=\",\r\n            \"arity\": \"binary\",\r\n            \"variable\": {\r\n              \"value\": \"t\",\r\n              \"arity\": \"name\"\r\n            },\r\n            \"expression\": {\r\n              \"value\": 2,\r\n              \"arity\": \"literal\"\r\n            }\r\n          }\r\n        ],\r\n        \"right\": [\r\n          {\r\n            \"alias\": \":=\",\r\n            \"arity\": \"binary\",\r\n            \"variable\": {\r\n              \"value\": \"t\",\r\n              \"arity\": \"name\"\r\n            },\r\n            \"expression\": {\r\n              \"value\": 4,\r\n              \"arity\": \"literal\"\r\n            }\r\n          }\r\n        ]\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"t\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      hasRedundantIf (gbs code) `shouldBe` False
   
  describe "hasRedundantBooleanComparison" $ do
    --it "is True when comparing a literal in a function" $ do
      -- code = "function x(m){return (m == True)}"
      --let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"==\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": true,\r\n        \"arity\": \"literal\",\r\n        \"reserved\": true\r\n      }\r\n    }\r\n  }\r\n]"
      --hasRedundantBooleanComparison (gbs code) `shouldBe` True

    it "is False when no comparison" $ do
      --  code = "function x(m){return (m)}"
      let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"m\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      hasRedundantBooleanComparison (gbs code) `shouldBe` False
   
  describe "hasRedundantLocalVariableReturn" $ do
    --  code = "function x(m) { x  := 5 return (x) }"
    -- cambiar todos los primerros assign _ por declare.. _ 
    --let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
    --it "is True when local variable is not necessary" $ do
      --hasRedundantLocalVariableReturn (gbs code) `shouldBe` True

    it "is False when local variable is not necessary, but there are many variables" $ do
      --  code = "function x(m){x := 5  y := 2  return (x)}"
      let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"y\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      hasRedundantLocalVariableReturn (gbs code) `shouldBe` False

    it "is False when local variable is necessary in return" $ do
      --  code  = "function x(m){x := 5  return (x+x)}"
      let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"+\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    }\r\n  }\r\n]"
      hasRedundantLocalVariableReturn (gbs code) `shouldBe` False

    it "is False when local variable is updated" $ do
      --  code = "function x(m){ x:= 5  x := x + 1  return (x)}"
      let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": \"+\",\r\n          \"arity\": \"binary\",\r\n          \"left\": {\r\n            \"value\": \"x\",\r\n            \"arity\": \"name\"\r\n          },\r\n          \"right\": {\r\n            \"value\": 1,\r\n            \"arity\": \"literal\"\r\n          }\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      hasRedundantLocalVariableReturn (gbs code) `shouldBe` False

    it "is False when local variable is used as a cache" $ do
      --  code = "function x(m){ x := 5  y := (1+x)  z := g(y)  return (x)}"
      let code = "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"y\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": \"+\",\r\n          \"arity\": \"binary\",\r\n          \"left\": {\r\n            \"value\": 1,\r\n            \"arity\": \"literal\"\r\n          },\r\n          \"right\": {\r\n            \"value\": \"x\",\r\n            \"arity\": \"name\"\r\n          }\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"z\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"alias\": \"functionCall\",\r\n          \"name\": \"g\",\r\n          \"parameters\": [\r\n            {\r\n              \"value\": \"y\",\r\n              \"arity\": \"name\"\r\n            }\r\n          ]\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]"
      hasRedundantLocalVariableReturn (gbs code) `shouldBe` False
	
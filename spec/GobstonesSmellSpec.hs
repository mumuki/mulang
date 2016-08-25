module GobstonesSmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do
  describe "hasRedundantIf" $ do{-
    it "is True when both branches are boolean literal returns" $ do
      hasRedundantIf (parseGobstones "function x() { if(m) return true else return false }") `shouldBe` True
      hasRedundantIf (parseGobstones "function x() { if(m) return false else return true }") `shouldBe` True
    
    it "is True when return an if with boolean literals" $ do
      hasRedundantIf (js "function x() { return m ? true : false }") `shouldBe` True

    it "is True when return an if with boolean literals, top level" $ do
      hasRedundantIf (parseGobstones "m ? true : false") `shouldBe` True

    it "is True when return an if with boolean literals, in method" $ do
      hasRedundantIf (parseGobstones "var y = {x: function(m){ return m ? true : false }}") `shouldBe` True
 -}
    it "is False when there is no if" $ do
      hasRedundantIf (parseGobstones "[\r\n  {\r\n    \"alias\": \"program\",\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": false,\r\n          \"arity\": \"literal\",\r\n          \"reserved\": true\r\n        }\r\n      }\r\n    ]\r\n  }\r\n]") `shouldBe` False

    it "is False when there are no literals" $ do
      hasRedundantIf (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \"conditional\",\r\n        \"condition\": {\r\n          \"value\": \"m\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"left\": [\r\n          {\r\n            \"alias\": \":=\",\r\n            \"arity\": \"binary\",\r\n            \"variable\": {\r\n              \"value\": \"t\",\r\n              \"arity\": \"name\"\r\n            },\r\n            \"expression\": {\r\n              \"value\": 2,\r\n              \"arity\": \"literal\"\r\n            }\r\n          }\r\n        ],\r\n        \"right\": [\r\n          {\r\n            \"alias\": \":=\",\r\n            \"arity\": \"binary\",\r\n            \"variable\": {\r\n              \"value\": \"t\",\r\n              \"arity\": \"name\"\r\n            },\r\n            \"expression\": {\r\n              \"value\": 4,\r\n              \"arity\": \"literal\"\r\n            }\r\n          }\r\n        ]\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"t\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]") `shouldBe` False
   
  describe "hasRedundantBooleanComparison" $ do
  	{-
    it "is True when comparing a literal in a function" $ do
      hasRedundantBooleanComparison (parseGobstones "function x(m) { return m == true }") `shouldBe` True
	-}
    it "is False when no comparison" $ do
      hasRedundantBooleanComparison (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"m\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]") `shouldBe` False
   

  
  describe "hasRedundantLocalVariableReturn" $ do
 	
    --it "is True when local variable is not necessary" $ do
      --hasRedundantLocalVariableReturn (js "function x(m) { var x  = 5; return x; }") `shouldBe` True

    it "is False when local variable is not necessary, but there are many variables" $ do
      hasRedundantLocalVariableReturn (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"y\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 2,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]") `shouldBe` False

    it "is False when local variable is necessary in return" $ do
      hasRedundantLocalVariableReturn (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"+\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      }\r\n    }\r\n  }\r\n]") `shouldBe` False

    it "is False when local variable is updated" $ do
      hasRedundantLocalVariableReturn (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": \"+\",\r\n          \"arity\": \"binary\",\r\n          \"left\": {\r\n            \"value\": \"x\",\r\n            \"arity\": \"name\"\r\n          },\r\n          \"right\": {\r\n            \"value\": 1,\r\n            \"arity\": \"literal\"\r\n          }\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]") `shouldBe` False

    it "is False when local variable is used as a cache" $ do
      hasRedundantLocalVariableReturn (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"x\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": 5,\r\n          \"arity\": \"literal\"\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"y\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"value\": \"+\",\r\n          \"arity\": \"binary\",\r\n          \"left\": {\r\n            \"value\": 1,\r\n            \"arity\": \"literal\"\r\n          },\r\n          \"right\": {\r\n            \"value\": \"x\",\r\n            \"arity\": \"name\"\r\n          }\r\n        }\r\n      },\r\n      {\r\n        \"alias\": \":=\",\r\n        \"arity\": \"binary\",\r\n        \"variable\": {\r\n          \"value\": \"z\",\r\n          \"arity\": \"name\"\r\n        },\r\n        \"expression\": {\r\n          \"alias\": \"functionCall\",\r\n          \"name\": \"g\",\r\n          \"parameters\": [\r\n            {\r\n              \"value\": \"y\",\r\n              \"arity\": \"name\"\r\n            }\r\n          ]\r\n        }\r\n      }\r\n    ],\r\n    \"return\": {\r\n      \"value\": \"x\",\r\n      \"arity\": \"name\"\r\n    }\r\n  }\r\n]") `shouldBe` False
	
  describe "hasAssignmentReturn" $ do
    --it "is True when return contains assignment" $ do
      --hasAssignmentReturn (parseGobstones "function x(m) { return x = 4 }") `shouldBe` True

    it "is False when return does not contain assignment" $ do
      hasAssignmentReturn (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": \"==\",\r\n      \"arity\": \"binary\",\r\n      \"left\": {\r\n        \"value\": \"x\",\r\n        \"arity\": \"name\"\r\n      },\r\n      \"right\": {\r\n        \"value\": 4,\r\n        \"arity\": \"literal\"\r\n      }\r\n    }\r\n  }\r\n]") `shouldBe` False

  describe "returnsNull" $ do
    --it "is True when returns null" $ do
      --returnsNull (js "function x(m) { return null }") `shouldBe` True

    it "is False when returns a number" $ do
      returnsNull (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": 1,\r\n      \"arity\": \"literal\"\r\n    }\r\n  }\r\n]") `shouldBe` False

  describe "doesNullTest" $ do
    --it "is True when tests for null" $ do
      --doesNullTest (js "function x(m) { if ( m == null) 1 else 2 } ") `shouldBe` True

    it "is False when not does null test" $ do
      doesNullTest (parseGobstones "[\r\n  {\r\n    \"value\": \"x\",\r\n    \"arity\": \"routine\",\r\n    \"reserved\": false,\r\n    \"led\": null,\r\n    \"lbp\": 0,\r\n    \"name\": \"x\",\r\n    \"alias\": \"functionDeclaration\",\r\n    \"parameters\": [\r\n      {\r\n        \"value\": \"m\",\r\n        \"arity\": \"name\"\r\n      }\r\n    ],\r\n    \"body\": [],\r\n    \"return\": {\r\n      \"value\": 1,\r\n      \"arity\": \"literal\"\r\n    }\r\n  }\r\n]") `shouldBe` False
{-
  describe "doesTypeTest" $ do
    it "is True when tests for string" $ do
      doesTypeTest (js "function x(m) { if ( m == \"foo\") 1 else 2 } ") `shouldBe` True

    it "is False when not does type test" $ do
      doesTypeTest (js "function x(m) { return 1 }") `shouldBe` False

   -}
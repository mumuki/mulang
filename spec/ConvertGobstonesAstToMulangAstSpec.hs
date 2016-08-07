module ConvertGobstonesAstToMulangAstSpec (spec) where

import	Test.Hspec
import	Language.Mulang
import	Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do

describe "translateProgramGobstonesToMulangExpression" $ do
    it "translate simple program Gobstones" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": null,\"from\": 0}]"

      gobstonesAst `shouldBe` Sequence [MuNull]

    it "translate null program Gobstones" $ do
      let gobstonesAst =  parseGobstones "null"

      gobstonesAst `shouldBe` MuNull

    it "translate simple procedure Call" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0}]"
      									
      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []]]
     
    it "translate simple procedure declaration " $ do
      let gobstonesAst =  parseGobstones  "[{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\": []}]"

      gobstonesAst `shouldBe` Sequence  [ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]
	
    it "translate simple procedure Application " $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\": []}]"

      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []],ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]
      
    it "translate simple procedure declaration with a parameter" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"

      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]
     
    it "translate simple procedure declaration and application  with a parameter" $ do
      let gobstonesAstNumber =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : 2, \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"
      
      let gobstonesAstColour =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : 2,\"reserved\" : true , \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"

      let gobstonesAstBool =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : true,\"reserved\" : true , \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"
      
      let gobstonesAstDirection =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : [1,0],\"reserved\" : true , \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"

      gobstonesAstNumber `shouldBe` Sequence [Sequence [Application (Variable "F") [MuNumber 2]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

      gobstonesAstColour `shouldBe` Sequence [Sequence [Application (Variable "F") [MuSymbol "Negro"]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

      gobstonesAstBool `shouldBe` Sequence [Sequence [Application (Variable "F") [MuBool True]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

      gobstonesAstDirection `shouldBe` Sequence [Sequence [Application (Variable "F") [MuSymbol "Este"]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]
 	
 	{-
	it "translate simple variable assignment" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : 1, \"arity\" : \"literal\"}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      gobstonesAst `shouldBe` Sequence [VariableAssignment "x" (MuNumber 2)]

	-}
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

      let gobstonesAstWithAparam =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"

      gobstonesAst `shouldBe` Sequence  [ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]

      gobstonesAstWithAparam `shouldBe` Sequence [Sequence [Application (Variable "F") []],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]
	
    it "translate simple procedure Application " $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\": []}]"

      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []],ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]
      
    it "translate simple function declaration" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"functionDeclaration\",\"body\": [],\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"parameters\": [],\"return\" : {\"from\" : 21, \"row\" : 1,\"to\" : 22, \"value\" : 2, \"arity\" : \"literal\"}}]"

      let gobstonesAstWithAparam =  parseGobstones "[{\"alias\": \"functionDeclaration\",\"body\": [],\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"f\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"f\",\"parameters\": [{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}],\"return\" : {\"from\" : 21, \"row\" : 1,\"to\" : 22, \"value\" : 2, \"arity\" : \"literal\"}}]"

      gobstonesAst `shouldBe` Sequence [FunctionDeclaration "f" [Equation [] (UnguardedBody (Return (MuNumber 2.0)))]]

      gobstonesAstWithAparam `shouldBe` Sequence [FunctionDeclaration "f" [Equation [VariablePattern "parameter"] (UnguardedBody (Return (MuNumber 2.0)))]]
   
    it "translate simple variable assignment" $ do
      let gobstonesAstNumber =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : 1, \"arity\" : \"literal\"}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      let gobstonesAstColour = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : 3, \"arity\" : \"literal\", \"reserved\" : true}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      let gobstonesAstBool = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : true, \"arity\" : \"literal\", \"reserved\" : true}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      let gobstonesAstDirection = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : [1,0], \"arity\" : \"literal\", \"reserved\" : true}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      let gobstonesAstVariable = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : \"y\" , \"arity\" : \"literal\", \"reserved\" : true}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      let gobstonesAstFunctionCall = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"alias\" : \"functionCall\" , \"name\" : \"f\" , \"parameters\" : [{\"value\" : 2, \"arity\" : \"literal\"}]}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"      

      --let gobstonesAstSimpleExpression = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"value\" : \"==\" , \"arity\" : \"binary\" , \"left\" : {\"value\" : true , \"arity\" : \"literal\" , \"reserved\" : true} , \"right\" : {\"value\" : true , \"arity\" : literal}} , \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"      

      gobstonesAstNumber `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuNumber 1.0)]]

      gobstonesAstColour `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuSymbol "Verde")]]

      gobstonesAstBool `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuBool True)]]

      gobstonesAstDirection `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuSymbol "Este")]]

      gobstonesAstVariable `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuString "y")]]

      gobstonesAstFunctionCall `shouldBe` Sequence [Sequence [VariableAssignment "x" (Application (Variable "f") [MuNumber 2.0])]]

      --gobstonesAstSimpleExpression `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuBool True)]]--falta todo lo que son expressiones

     
    it "translate simple procedure declaration and application  with a parameter" $ do
      let gobstonesAstNumber =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : 2, \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"
      
      let gobstonesAstColour =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : 2,\"reserved\" : true , \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"

      let gobstonesAstBool =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : true,\"reserved\" : true , \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"
      
      let gobstonesAstDirection =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": [{\"from\" : 15, \"row\" : 1, \"to\" : 16, \"value\" : [1,0],\"reserved\" : true , \"arity\" : \"literal\" }]}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\":[{\"from\": 32,\"row\": 4,\"to\": 41,\"value\" : \"parameter\",\"arity\" : \"name\", \"reserved\": false,\"led\": null,\"std\": null,\"lbp\": 0}] }]"

      gobstonesAstNumber `shouldBe` Sequence [Sequence [Application (Variable "F") [MuNumber 2]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

      gobstonesAstColour `shouldBe` Sequence [Sequence [Application (Variable "F") [MuSymbol "Negro"]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

      gobstonesAstBool `shouldBe` Sequence [Sequence [Application (Variable "F") [MuBool True]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

      gobstonesAstDirection `shouldBe` Sequence [Sequence [Application (Variable "F") [MuSymbol "Este"]],ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]
 	
    it "translate conditional declaration" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"conditional\", \"condition\" : {\"value\" : true, \"arity\" : \"literal\" , \"reserved\" : true}, \"left\" : null, \"right\" : null}],\"from\": 0}]"

      let gobstonesAstIfWithBody =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"conditional\", \"condition\" : {\"value\" : true, \"arity\" : \"literal\" , \"reserved\" : true}, \"left\" : [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : 1, \"arity\" : \"literal\"}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }], \"right\" : null}],\"from\": 0}]"

      gobstonesAst `shouldBe` Sequence [Sequence [If (MuBool True) MuNull MuNull]]

      gobstonesAstIfWithBody `shouldBe` Sequence [Sequence [If (MuBool True) (Sequence [VariableAssignment "x" (MuNumber 1.0)]) MuNull]]


    it "translate while declaration" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"while\", \"expression\" : {\"value\" : true, \"arity\" : \"literal\" , \"reserved\" : true}, \"body\" : null}],\"from\": 0}]"

      let gobstonesAstWhileWithBody =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"while\", \"expression\" : {\"value\" : true, \"arity\" : \"literal\" , \"reserved\" : true}, \"body\" : [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : 1, \"arity\" : \"literal\"}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }]}],\"from\": 0}]"

      gobstonesAst `shouldBe` Sequence [Sequence [While (MuBool True) MuNull]]

      gobstonesAstWhileWithBody `shouldBe` Sequence [Sequence [While (MuBool True) (Sequence [VariableAssignment "x" (MuNumber 1.0)])]]

      



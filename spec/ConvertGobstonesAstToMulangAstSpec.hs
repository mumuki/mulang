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

      let gobstonesAstWithPoner =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"PutStone\" , \"parameters\" : [{\"value\" : 3, \"arity\" : \"literal\" , \"reserved\" : true}] }],\"from\": 0}]"

      let gobstonesAstWithSacar =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"RemoveStone\" , \"parameters\" : [{\"value\" : 3, \"arity\" : \"literal\" , \"reserved\" : true}] }],\"from\": 0}]"

      let gobstonesAstWithMover =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"MoveClaw\" , \"parameters\" : [{\"value\" : [1,0], \"arity\" : \"literal\" , \"reserved\" : true}] }],\"from\": 0}]"

      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []],ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]

      gobstonesAstWithPoner `shouldBe` Sequence [Sequence [Application (Variable "Poner") [MuSymbol "Verde"]]]

      gobstonesAstWithSacar `shouldBe` Sequence [Sequence [Application (Variable "Sacar") [MuSymbol "Verde"]]]

      gobstonesAstWithMover `shouldBe` Sequence [Sequence [Application (Variable "Mover") [MuSymbol "Este"]]]

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

      let gobstonesAstSimpleExpression = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : \"&&\" , \"arity\" : \"binary\",\"left\" : {\"value\" : \"z\" , \"arity\" : \"name\"} , \"right\" : {\"value\" : \"y\" , \"arity\" : \"name\"}}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      let gobstonesAstComposeExpression = parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \":=\",\"arity\" : \"binary\", \"variable\" : {\"from\" : 13, \"row\" : 1,\"to\" : 14, \"value\" : \"x\", \"arity\" : \"name\"}, \"expression\" : {\"from\" : 18, \"row\" : 1,\"to\" : 19, \"value\" : \"&&\" , \"arity\" : \"binary\",\"left\" : {\"value\" : \"==\" , \"arity\" : \"binary\", \"left\" : {\"value\" : true , \"arity\" : \"literal\" , \"reserved\" : true } , \"right\" : {\"value\" : 2 , \"arity\" : \"literal\"} } , \"right\" : {\"value\" : \"!=\" , \"arity\" : \"binary\" , \"left\" : {\"value\" : \"x\" , \"arity\" : \"name\"},\"right\" : {\"value\" : \"t\" , \"arity\" : \"name\"} }}, \"assignment\" : true, \"from\" : 13,\"to\" : 20 }],\"from\": 0}]"

      gobstonesAstNumber `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuNumber 1.0)]]

      gobstonesAstColour `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuSymbol "Verde")]]

      gobstonesAstBool `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuBool True)]]

      gobstonesAstDirection `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuSymbol "Este")]]

      gobstonesAstVariable `shouldBe` Sequence [Sequence [VariableAssignment "x" (MuString "y")]]

      gobstonesAstFunctionCall `shouldBe` Sequence [Sequence [VariableAssignment "x" (Application (Variable "f") [MuNumber 2.0])]]

      gobstonesAstSimpleExpression `shouldBe` Sequence [Sequence [VariableAssignment "x" (Application (Variable "&&") [MuString "z",MuString "y"])]]

      gobstonesAstComposeExpression `shouldBe` Sequence [Sequence [VariableAssignment "x" (Application (Variable "&&") [Application (Variable "==") [MuBool True,MuNumber 2.0],Application (Variable "!=") [MuString "x",MuString "t"]])]]

     
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

    it "translate switch declaration" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"switch\",\"value\" : {\"value\" : 2 , \"arity\" : \"literal\"} ,\"cases\" : [{\"case\" : {\"value\" : 2 , \"arity\" : \"literal\"} , \"body\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" , \"variable\" : {\"value\" : \"x\" , \"arity\" : \"name\"}, \"expression\" :{\"value\" : 2, \"arity\" : \"literal\"} }] }]}],\"from\": 0}]"

      gobstonesAst `shouldBe`  Sequence [Sequence [Switch (MuNumber 2.0) [(MuNumber 2.0,Sequence [VariableAssignment "x" (MuNumber 2.0)])]]]

    it "translate repeat declaration" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\" : \"repeat\",\"expression\" : {\"value\" : 2,\"arity\" : \"literal\"}, \"body\" : [{ \"alias\" : \":=\",\"arity\" : \"binary\",\"variable\" : {\"value\" : \"x\", \"arity\" : \"name\"},\"expression\" : {\"value\" : 2 ,\"arity\" : \"literal\" } }] }],\"from\": 0}]"

      gobstonesAst `shouldBe`  Sequence [Sequence [Repeat (MuNumber 2.0) (Sequence [VariableAssignment "x" (MuNumber 2.0)])]]

    it "translate a complete program" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"arity\" : \"routine\", \"alias\" : \"ProcedureCall\" , \"name\" : \"F\" , \"parameters\" : [{\"value\" :3,\"arity\" : \"literal\" , \"reserved\" : true}]},{\"arity\" : \"routine\", \"alias\" : \"ProcedureCall\" , \"name\" : \"G\" , \"parameters\" : [{\"value\" :2,\"arity\" : \"literal\" , \"reserved\" : true}, {\"value\" :3,\"arity\" : \"literal\" , \"reserved\" : true}]},{\"arity\" : \"routine\", \"alias\" : \"ProcedureCall\" , \"name\" : \"X\" , \"parameters\" : [{\"value\" : [1,0],\"arity\" : \"literal\" , \"reserved\" : true}]},{\"alias\" : \":=\", \"arity\" : \"binary\" ,\"variable\" : {\"value\" : \"y\" , \"arity\" : \"name\"},\"expression\" : {\"alias\" : \"functionCall\" , \"name\" : \"f\" , \"parameters\" : [{\"value\" : false, \"arity\" : \"literal\" , \"reserved\" : true}] } }],\"from\": 0},{\"value\" : \"F\" , \"arity\" : \"routine\" , \"reserved\" : false , \"led\" : null, \"lbp\" : 0 , \"name\" : \"F\" , \"alias\" : \"procedureDeclaration\" , \"parameters\" : [{\"value\" : \"x\" , \"arity\" : \"name\"}] ,\"body\" : [{\"alias\" : \"PutStone\" , \"parameters\" : [{\"value\" : \"x\" ,\"arity\" : \"name\"}]},{\"alias\" : \"RemoveStone\" , \"parameters\" : [{\"value\" : \"x\" ,\"arity\" : \"name\"}]} ] },{\"value\" : \"G\" , \"arity\" : \"routine\" , \"reserved\" : false , \"led\" : null, \"lbp\" : 0 , \"name\" : \"G\" , \"alias\" : \"procedureDeclaration\" , \"parameters\" : [{\"value\" : \"n1\" , \"arity\" : \"name\"},{\"value\" : \"n2\" , \"arity\" : \"name\"}] , \"body\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" , \"variable\" :{\"value\" : \"x\" , \"arity\" : \"name\"}, \"expression\" : {\"value\" : \"n1\" , \"arity\" : \"name\"}},{\"alias\" : \":=\" , \"arity\" : \"binary\" , \"variable\" :{\"value\" : \"z\" , \"arity\" : \"name\"}, \"expression\" : {\"value\" : \"n2\" , \"arity\" : \"name\"}},{\"alias\" : \"while\" , \"expression\" :{\"value\" : true, \"arity\" : \"literal\" , \"reserved\" : true},\"body\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" , \"variable\" :{\"value\" : \"x\" , \"arity\" : \"name\"}, \"expression\" : {\"value\" : \"n1\" , \"arity\" : \"name\"}}]}, {\"alias\" : \"switch\" , \"value\" : {\"value\" : \"dir\" , \"arity\" : \"name\"} , \"cases\" :[{\"case\": {\"value\" : [0,-1], \"arity\" : \"literal\" , \"reserved\" : true},\"body\" :[{\"alias\" : \"PutStone\" , \"parameters\" : [{\"value\" : 3 , \"arity\" : \"literal\"}] }] },{\"case\": {\"value\" : [1,0], \"arity\" : \"literal\" , \"reserved\" : true},\"body\" :[{\"alias\" : \"PutStone\" , \"parameters\" : [{\"value\" : 3 , \"arity\" : \"literal\"}] }] },{\"case\": {\"value\" : [-1,0], \"arity\" : \"literal\" , \"reserved\" : true},\"body\" :[{\"alias\" : \"PutStone\" , \"parameters\" : [{\"value\" : 3 , \"arity\" : \"literal\"}] }] },{\"case\": {\"value\" : [0,1], \"arity\" : \"literal\" , \"reserved\" : true},\"body\" :[{\"alias\" : \"PutStone\" , \"parameters\" : [{\"value\" : 3 , \"arity\" : \"literal\"}] }] } ] } ] },{\"value\" : \"X\" , \"arity\" : \"routine\" , \"reserved\" : false , \"led\" : null, \"lbp\" : 0 , \"name\" : \"X\" , \"alias\" : \"procedureDeclaration\" , \"parameters\" : [{\"value\": \"dir\", \"arity\" : \"name\"}] ,\"body\" :[{\"alias\" : \"MoveClaw\", \"parameters\" : [{\"value\": \"dir\" , \"arity\" : \"name\"}]} ] },{\"value\" : \"f\" , \"arity\" : \"routine\" , \"reserved\" : false , \"led\" : null, \"lbp\" : 0 , \"name\" : \"f\" , \"alias\" : \"functionDeclaration\" , \"parameters\" : [{\"value\" : \"bool\" , \"arity\" : \"name\"}] , \"body\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" , \"variable\" :{\"value\" : \"g\" ,\"arity\" : \"name\" } ,\"expression\" :{\"value\" : 2 , \"arity\" : \"literal\"} } , {\"alias\" : \"conditional\", \"condition\" :{\"value\" : false, \"arity\" : \"literal\"},\"left\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" ,\"variable\" :{\"value\" : \"resultado\" ,\"arity\" : \"name\" }, \"expression\": {\"value\" : true , \"arity\" : \"literal\"} }], \"right\" : [{\"alias\" : \":=\" , \"arity\" : \"binary\" ,\"variable\" :{\"value\" : \"resultado\" ,\"arity\" : \"name\" }, \"expression\": {\"value\" : \"resultado\" , \"arity\" : \"name\"} }] } ] , \"return\" : {\"value\" : \"resultado\" , \"arity\" : \"name\"}}]"

      gobstonesAst `shouldBe`  Sequence [Sequence [Application (Variable "F") [MuSymbol "Verde"],Application (Variable "G") [MuSymbol "Negro",MuSymbol "Verde"],Application (Variable "X") [MuSymbol "Este"],VariableAssignment "y" (Application (Variable "f") [MuBool False])],ProcedureDeclaration "F" [Equation [VariablePattern "x"] (UnguardedBody (Sequence [Application (Variable "Poner") [MuString "x"],Application (Variable "Sacar") [MuString "x"]]))],ProcedureDeclaration "G" [Equation [VariablePattern "n1",VariablePattern "n2"] (UnguardedBody (Sequence [VariableAssignment "x" (MuString "n1"),VariableAssignment "z" (MuString "n2"),While (MuBool True) (Sequence [VariableAssignment "x" (MuString "n1")]),Switch (MuString "dir") [(MuSymbol "Sur",Sequence [Application (Variable "Poner") [MuNumber 3.0]]),(MuSymbol "Este",Sequence [Application (Variable "Poner") [MuNumber 3.0]]),(MuSymbol "Oeste",Sequence [Application (Variable "Poner") [MuNumber 3.0]]),(MuSymbol "Norte",Sequence [Application (Variable "Poner") [MuNumber 3.0]])]]))],ProcedureDeclaration "X" [Equation [VariablePattern "dir"] (UnguardedBody (Sequence [Application (Variable "Mover") [MuString "dir"]]))],FunctionDeclaration "f" [Equation [VariablePattern "bool"] (UnguardedBody (Sequence [VariableAssignment "g" (MuNumber 2.0),If (MuBool False) (Sequence [VariableAssignment "resultado" (MuBool True)]) (Sequence [VariableAssignment "resultado" (MuString "resultado")]),Return (MuString "resultado")]))]]

   

    


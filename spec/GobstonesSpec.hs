module GobstonesSpec (spec) where

import	Test.Hspec
import	Language.Mulang
import	Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do

describe "gobstones" $ do
    it "translates simple Gobstones program" $ do
      (gbs "program {}") `shouldBe` EntryPoint MuNull

    it "translates null Gobstones program" $ do
      (gba "null") `shouldBe` MuNull

    it "translates simple procedure Call" $ do
      (gbs "program{F()}") `shouldBe` EntryPoint (Application (Variable "F") [])

    it "translates simple procedure declaration " $ do
      (gbs "procedure F(){}") `shouldBe` ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]

    it "translates simple procedure declaration with a param" $ do
      let code = gbs "program{F()} procedure F(parameter){}"

      code `shouldBe` Sequence [EntryPoint (Application (Variable "F") []),ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

    it "translates simple procedure Application " $ do
      let code = gbs "program{F()} procedure F(){}"

      code `shouldBe` Sequence [EntryPoint (Application (Variable "F") []),ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]

    it "translates simple procedure Application " $ do
      let code = gbs "program{Poner(Verde)}"

      code `shouldBe` EntryPoint (Application (Variable "Poner") [MuSymbol "Verde"])

    it "translates simple procedure Application " $ do
      let code =  gbs "program{Sacar(Verde)}"

      code `shouldBe` EntryPoint (Application (Variable "Sacar") [MuSymbol "Verde"])

    it "translates simple procedure Application " $ do
      let code = gbs "program{Mover(Este)}"

      code `shouldBe` EntryPoint (Application (Variable "Mover") [MuSymbol "Este"])

    it "translates simple function declaration" $ do
      let code = gbs "function f(){return (Verde)}"

      code `shouldBe` FunctionDeclaration "f" [Equation [] (UnguardedBody (Sequence [MuNull,Return (MuSymbol "Verde")]))]

    it "translates simple function declaration" $ do
      let  code = gbs "function f(parameter){return (2)}"

      code `shouldBe` FunctionDeclaration "f" [Equation [VariablePattern "parameter"] (UnguardedBody (Sequence [MuNull,Return (MuNumber 2.0)]))]

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= 1}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (MuNumber 1.0))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= Verde}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (MuSymbol "Verde"))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= True}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (MuBool True))

    it "translates simple variable assignment" $ do
      let  code = gbs "program{x:= Este}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (MuSymbol "Este"))

    it "translates simple variable assignment" $ do
      let  code = gbs "program{x:= y}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (Variable "y"))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= f(2)}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (Application (Variable "f") [MuNumber 2.0]))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= z && y}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (Application (Variable "&&") [Variable "z",Variable "y"]))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= not z}"

      code `shouldBe` EntryPoint (VariableDeclaration "x" (Application (Variable "not") [Variable "z"]))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x := True == 2 && x /= t}"

      code `shouldBe`  EntryPoint (VariableDeclaration "x" (Application (Variable "&&") [Application Equal [MuBool True,MuNumber 2.0],Application NotEqual [Variable "x",Variable "t"]]))

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(2)} procedure F(parameter){}"

      code `shouldBe` Sequence [EntryPoint (Application (Variable "F") [MuNumber 2.0]),ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(Negro)} procedure F(parameter){}"

      code `shouldBe` Sequence [EntryPoint (Application (Variable "F") [MuSymbol "Negro"]),ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(True)} procedure F(parameter){}"

      code `shouldBe` Sequence [EntryPoint (Application (Variable "F") [MuBool True]),ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(Verde)} procedure F(parameter){}"

      code `shouldBe` Sequence [EntryPoint (Application (Variable "F") [MuSymbol "Verde"]),ProcedureDeclaration "F" [Equation [VariablePattern "parameter"] (UnguardedBody MuNull)]]

    it "translates conditional declaration" $ do
      let code = gbs "program{if(True){}}"

      code `shouldBe` EntryPoint (If (MuBool True) MuNull MuNull)

    it "translates conditional declaration" $ do
      let code = gbs "program{if(True){x := 1}}"

      code `shouldBe` EntryPoint (If (MuBool True) (VariableDeclaration "x" (MuNumber 1.0)) MuNull)

    it "translates while declaration" $ do
      let code = gbs "program{while(True){}}"

      code `shouldBe` EntryPoint (While (MuBool True) MuNull)

    it "translates while declaration" $ do
      let code = gbs "program{while(True){x := 1}}"

      code `shouldBe` EntryPoint (While (MuBool True) (VariableDeclaration "x" (MuNumber 1.0)))

    it "translates switch declaration" $ do
      let code = gbs "program{switch(2) to {2 -> {x := 2}}}"

      code `shouldBe`  EntryPoint (Switch (MuNumber 2.0) [(MuNumber 2.0,VariableDeclaration "x" (MuNumber 2.0))])

    it "translates repeat declaration" $ do
      let code = gbs "program{repeat(2){x := 2}}"

      code `shouldBe`  EntryPoint (Repeat (MuNumber 2.0) (VariableDeclaration "x" (MuNumber 2.0)))

    it "translates a complete program" $ do
      let code = gbs "program{F(Verde) G(2,3) X(Este) y := f(False) } procedure F(x){ Poner(x) Poner(x) Poner(x) Sacar(x) } procedure G(n1,n2){ x := n1 z := n2 while(True){ x := n1} switch(dir) to { Sur -> {Poner(Verde)} Este -> {Poner(Verde)} Oeste -> {Poner(Verde)} Norte -> {Poner(Verde)}}} procedure X(dir){ Mover(dir) } function f(bool){ g := 2 if(False){ resultado := True} else { resultado := resultado} return (resultado)}"

      code `shouldBe`  Sequence [EntryPoint (Sequence [Application (Variable "F") [MuSymbol "Verde"],Application (Variable "G") [MuNumber 2.0,MuNumber 3.0],Application (Variable "X") [MuSymbol "Este"],VariableAssignment "y" (Application (Variable "f") [MuBool False])]),ProcedureDeclaration "F" [Equation [VariablePattern "x"] (UnguardedBody (Sequence [Application (Variable "Poner") [Variable "x"],Application (Variable "Poner") [Variable "x"],Application (Variable "Poner") [Variable "x"],Application (Variable "Sacar") [Variable "x"]]))],ProcedureDeclaration "G" [Equation [VariablePattern "n1",VariablePattern "n2"] (UnguardedBody (Sequence [VariableAssignment "x" (Variable "n1"),VariableAssignment "z" (Variable "n2"),While (MuBool True) (VariableDeclaration "x" (Variable "n1")),Switch (Variable "dir") [(MuSymbol "Sur",Application (Variable "Poner") [MuSymbol "Verde"]),(MuSymbol "Este",Application (Variable "Poner") [MuSymbol "Verde"]),(MuSymbol "Oeste",Application (Variable "Poner") [MuSymbol "Verde"]),(MuSymbol "Norte",Application (Variable "Poner") [MuSymbol "Verde"])]]))],ProcedureDeclaration "X" [Equation [VariablePattern "dir"] (UnguardedBody (Application (Variable "Mover") [Variable "dir"]))],FunctionDeclaration "f" [Equation [VariablePattern "bool"] (UnguardedBody (Sequence [VariableAssignment "g" (MuNumber 2.0),If (MuBool False) (VariableDeclaration "resultado" (MuBool True)) (VariableDeclaration "resultado" (Variable "resultado")),Return (Variable "resultado")]))]]

    it "transform VariableAssignment to VariableDeclaration" $ do
      let code =  gbs "program{ x := 2 y := 3 x := 1 } function f () { x := 4 y := 6 z := 9 y := True return (x) } procedure P(){ x := 4 y := 6 z := 9 y := True }"

      code `shouldBe`  Sequence [EntryPoint (Sequence [VariableAssignment "x" (MuNumber 2.0),VariableAssignment "y" (MuNumber 3.0),VariableAssignment "x" (MuNumber 1.0)]),FunctionDeclaration "f" [Equation [] (UnguardedBody (Sequence [VariableAssignment "x" (MuNumber 4.0),VariableAssignment "y" (MuNumber 6.0),VariableAssignment "z" (MuNumber 9.0),VariableAssignment "y" (MuBool True),Return (Variable "x")]))],ProcedureDeclaration "P" [Equation [] (UnguardedBody (Sequence [VariableAssignment "x" (MuNumber 4.0),VariableAssignment "y" (MuNumber 6.0),VariableAssignment "z" (MuNumber 9.0),VariableAssignment "y" (MuBool True)]))]]






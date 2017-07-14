module GobstonesSpec (spec) where

import  Test.Hspec
import  Language.Mulang
import  Language.Mulang.Parsers.Gobstones

program body = EntryPoint "program" body

spec :: Spec
spec = do

describe "gobstones" $ do
    it "translates programs with returns" $ do
      (gbs "program { result := foo(); return (result) }") `shouldBe` program (Sequence [
                                                                                (Variable "result" (Application (Reference "foo") [])),
                                                                                Return (Reference "result")])

    it "translates simple Gobstones program" $ do
      (gbs "program {}") `shouldBe` program MuNull

    it "translates null Gobstones program" $ do
      (gba "null") `shouldBe` MuNull

    it "translates simple procedure Call" $ do
      (gbs "program{F()}") `shouldBe` program (Application (Reference "F") [])

    it "translates simple procedure declaration " $ do
      (gbs "procedure F(){}") `shouldBe` SimpleProcedure "F" [] MuNull

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(2)} procedure F(parameter){}"

      code `shouldBe` Sequence [program (Application (Reference "F") [MuNumber 2.0]), SimpleProcedure "F" [VariablePattern "parameter"] MuNull]

    it "translates simple procedure Application " $ do
      let code = gbs "program{F()} procedure F(){}"

      code `shouldBe` Sequence [program (Application (Reference "F") []), SimpleProcedure "F" [] MuNull]

    it "translates Poner" $ do
      let code = gbs "program{Poner(Verde)}"

      code `shouldBe` program (Application (Reference "Poner") [MuSymbol "Verde"])

    it "translates Sacar" $ do
      let code =  gbs "program{Sacar(Verde)}"

      code `shouldBe` program (Application (Reference "Sacar") [MuSymbol "Verde"])

    it "translates Mover" $ do
      let code = gbs "program{Mover(Este)}"

      code `shouldBe` program (Application (Reference "Mover") [MuSymbol "Este"])

    it "translates simple function declaration" $ do
      let code = gbs "function f(){return (Verde)}"

      code `shouldBe` SimpleFunction "f" [] (Sequence [MuNull, Return (MuSymbol "Verde")])

    it "translates simple function declaration" $ do
      let  code = gbs "function f(parameter){return (2)}"

      code `shouldBe` SimpleFunction "f" [VariablePattern "parameter"] (Sequence [MuNull, Return (MuNumber 2.0)])

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= 1}"

      code `shouldBe` program (Variable "x" (MuNumber 1.0))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= Verde}"

      code `shouldBe` program (Variable "x" (MuSymbol "Verde"))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= True}"

      code `shouldBe` program (Variable "x" MuTrue)

    it "translates simple variable assignment" $ do
      let  code = gbs "program{x:= Este}"

      code `shouldBe` program (Variable "x" (MuSymbol "Este"))

    it "translates simple variable assignment" $ do
      let  code = gbs "program{x:= y}"

      code `shouldBe` program (Variable "x" (Reference "y"))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= f(2)}"

      code `shouldBe` program (Variable "x" (Application (Reference "f") [MuNumber 2.0]))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= z && y}"

      code `shouldBe` program (Variable "x" (Application (Reference "&&") [Reference "z",Reference "y"]))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x:= not z}"

      code `shouldBe` program (Variable "x" (Application (Reference "not") [Reference "z"]))

    it "translates simple variable assignment" $ do
      let code = gbs "program{x := True == 2 && x /= t}"

      code `shouldBe`  program (Variable "x" (Application (Reference "&&") [Application Equal [MuTrue,MuNumber 2.0],Application NotEqual [Reference "x",Reference "t"]]))

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(Negro)} procedure F(parameter){}"

      code `shouldBe` Sequence [
                        program (Application (Reference "F") [MuSymbol "Negro"]),
                        SimpleProcedure "F" [VariablePattern "parameter"] MuNull]

    it "translates simple procedure declaration and application  with a parameter" $ do
      let code = gbs "program{F(True)} procedure F(parameter){}"

      code `shouldBe` Sequence [
                        program (Application (Reference "F") [MuTrue]),
                        SimpleProcedure "F" [VariablePattern "parameter"] MuNull]

    it "translates conditional declaration" $ do
      let code = gbs "program{if(True){}}"

      code `shouldBe` program (If MuTrue MuNull MuNull)

    it "translates conditional declaration" $ do
      let code = gbs "program{if(True){x := 1}}"

      code `shouldBe` program (If MuTrue (Variable "x" (MuNumber 1.0)) MuNull)

    it "translates while declaration" $ do
      let code = gbs "program{while(True){}}"

      code `shouldBe` program (While MuTrue MuNull)

    it "translates while declaration" $ do
      let code = gbs "program{while(True){x := 1}}"

      code `shouldBe` program (While MuTrue (Variable "x" (MuNumber 1.0)))

    it "translates switch declaration" $ do
      let code = gbs "program{switch(2) to {2 -> {x := 2}}}"

      code `shouldBe`  program (Switch (MuNumber 2.0) [(MuNumber 2.0,Variable "x" (MuNumber 2.0))])

    it "translates repeat declaration" $ do
      let code = gbs "program{repeat(2){x := 2}}"

      code `shouldBe`  program (Repeat (MuNumber 2.0) (Variable "x" (MuNumber 2.0)))

    it "translates a complete program" $ do
      let code = gbs "program{F(Verde) G(2,3) X(Este) y := f(False) } procedure F(x){ Poner(x) Poner(x) Poner(x) Sacar(x) } procedure G(n1,n2){ x := n1 z := n2 while(True){ x := n1} switch(dir) to { Sur -> {Poner(Verde)} Este -> {Poner(Verde)} Oeste -> {Poner(Verde)} Norte -> {Poner(Verde)}}} procedure X(dir){ Mover(dir) } function f(bool){ g := 2 if(False){ resultado := True} else { resultado := resultado} return (resultado)}"

      code `shouldBe`  Sequence [program (Sequence [Application (Reference "F") [MuSymbol "Verde"],Application (Reference "G") [MuNumber 2.0,MuNumber 3.0],Application (Reference "X") [MuSymbol "Este"],Assignment "y" (Application (Reference "f") [MuFalse])]),Procedure "F" [Equation [VariablePattern "x"] (UnguardedBody (Sequence [Application (Reference "Poner") [Reference "x"],Application (Reference "Poner") [Reference "x"],Application (Reference "Poner") [Reference "x"],Application (Reference "Sacar") [Reference "x"]]))],Procedure "G" [Equation [VariablePattern "n1",VariablePattern "n2"] (UnguardedBody (Sequence [Assignment "x" (Reference "n1"),Assignment "z" (Reference "n2"),While MuTrue (Variable "x" (Reference "n1")),Switch (Reference "dir") [(MuSymbol "Sur",Application (Reference "Poner") [MuSymbol "Verde"]),(MuSymbol "Este",Application (Reference "Poner") [MuSymbol "Verde"]),(MuSymbol "Oeste",Application (Reference "Poner") [MuSymbol "Verde"]),(MuSymbol "Norte",Application (Reference "Poner") [MuSymbol "Verde"])]]))],Procedure "X" [Equation [VariablePattern "dir"] (UnguardedBody (Application (Reference "Mover") [Reference "dir"]))],Function "f" [Equation [VariablePattern "bool"] (UnguardedBody (Sequence [Assignment "g" (MuNumber 2.0),If MuFalse (Variable "resultado" MuTrue) (Variable "resultado" (Reference "resultado")),Return (Reference "resultado")]))]]

    it "transform Assignment to Variable" $ do
      let code =  gbs "program{ x := 2 y := 3 x := 1 } function f () { x := 4 y := 6 z := 9 y := True return (x) } procedure P(){ x := 4 y := 6 z := 9 y := True }"

      code `shouldBe`  Sequence [
                          program (Sequence [Assignment "x" (MuNumber 2.0),Assignment "y" (MuNumber 3.0),Assignment "x" (MuNumber 1.0)]),
                          Function "f" [Equation [] (UnguardedBody (Sequence [Assignment "x" (MuNumber 4.0),Assignment "y" (MuNumber 6.0),Assignment "z" (MuNumber 9.0),Assignment "y" MuTrue,Return (Reference "x")]))],
                          Procedure "P" [Equation [] (UnguardedBody (Sequence [Assignment "x" (MuNumber 4.0),Assignment "y" (MuNumber 6.0),Assignment "z" (MuNumber 9.0),Assignment "y" MuTrue]))]]


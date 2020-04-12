{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module CSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast hiding (Equal, NotEqual)
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.C

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = c . unpack

cContext :: Expression -> Expression
cContext expr = Sequence [SubroutineSignature "main" [] "int" [], SimpleFunction "main" [] expr]

spec :: Spec
spec = do
  describe "parse" $ do

    context "declare variabels" $ do

      it "parses simple variable" $ do
        run "int a;" `shouldBe` Sequence [
            TypeSignature "a" (SimpleType "int" []),
            Variable "a" None
          ]

      it "parses pointer to variable" $ do
        run "int * a;" `shouldBe` Sequence [
            TypeSignature "*a" (SimpleType "int" []),
            Variable "a" None
          ]

      it "parses array without size variable" $ do
        run "int a[];" `shouldBe` Sequence [
            TypeSignature "a[]" (SimpleType "int" []),
            Variable "a" None
          ]

      it "parses array with size variable" $ do
        run "int a[10];" `shouldBe` Sequence [
            TypeSignature "a[10]" (SimpleType "int" []),
            Variable "a" None
          ]

      it "parses int with inicialization" $ do
        run "int a = 10;" `shouldBe` Sequence [
            TypeSignature "a" (SimpleType "int" []),
            Variable "a" (MuNumber 10.0)
          ]

      it "parses char with initialization" $ do
        run "char a = 'a';" `shouldBe` Sequence [
            TypeSignature "a" (SimpleType "char" []),
            Variable "a" (MuChar 'a')
          ]

      it "parses string with initialization" $ do
        run "char *a = \"Hello\";" `shouldBe` Sequence [
            TypeSignature "*a" (SimpleType "char" []),
            Variable "a" (MuString "Hello")
          ]

      it "parses double with initialization" $ do
        run "double a = 0.1;" `shouldBe` Sequence [
            TypeSignature "a" (SimpleType "double" []),
            Variable "a" (MuNumber 0.1)
          ]

      it "parses array with initialization" $ do
        run "int a[3] = {1, 2, 3};" `shouldBe` Sequence [
            TypeSignature "a[3]" (SimpleType "int" []),
            Variable "a" (MuList [MuNumber 1, MuNumber 2, MuNumber 3])
          ]

      it "parses references" $ do
        run "int main () { a; }" `shouldBe` cContext (Reference "a")

      it "parses if" $ do
        run [text|
          int main () {
            if(1) {
              2;
            } else {
              3;
            }
          }
          |] `shouldBe` cContext (If (MuNumber 1) (MuNumber 2) (MuNumber 3))

      it "parses for" $ do
        run [text|
          int main () {
            for(i; i; i) {
              i;
            }
          }
          |] `shouldBe` cContext (ForLoop (Reference "i") (Reference "i") (Reference "i") (Reference "i"))

      it "parses return" $ do
        run [text|
          int main () {
            return 123;
          }
          |] `shouldBe` cContext (Return (MuNumber 123))

      it "parses binary operators" $ do
        run [text|
          int main () {
            a + b;
          }
          |] `shouldBe` cContext (Application (Primitive Plus) [Reference "a", Reference "b"])

      it "parses unary operators" $ do
        run [text|
          int main () {
            !a;
          }
          |] `shouldBe` cContext (Application (Primitive Negation) [Reference "a"])

      it "parses assign operators" $ do
        run [text|
          int main () {
            a *= 2;
          }
          |] `shouldBe` cContext (Assignment "a" (Application (Primitive Multiply) [Reference "a", MuNumber 2]))

      it "parses logical operators" $ do
        run [text|
          int main () {
            a || b;
          }
          |] `shouldBe` cContext (Application (Primitive Or) [Reference "a", Reference "b"])

      it "parses simple assignment" $ do
        run [text|
          int main () {
            a = 123;
          }
          |] `shouldBe` cContext (Assignment "a" (MuNumber 123))

      it "parses while" $ do
        run [text|
          int main () {
            while(1) {
              2;
            }
          }
          |] `shouldBe` cContext (While (MuNumber 1) (MuNumber 2))

      it "parses while" $ do
        run [text|
          int main () {
            switch(a) {
              case 1:
                break;
              case 2:
                continue;
              default:
                1;
            }
          }
          |] `shouldBe` cContext (Switch (Reference "a") [(MuNumber 1, Break), (MuNumber 2, Continue)] (MuNumber 1))

      it "parses simple assignment" $ do
        run [text|
          int cantidadDeNumerosImpares(int unosNumeros[]) {
            int cantidadDeImpares;
            for (int indice = 0; unosNumeros[indice] != NULL; indice++) {
              if (esNumeroImpar(c[b])) {
                cantidadDeImpares++;
              }
            }
            return cantidadDeImpares;
          }
          |] `shouldBe` Sequence [
                          TypeSignature "cantidadDeNumerosImpares" (ParameterizedType ["int"] "int" []),
                          Function "cantidadDeNumerosImpares" [Equation [VariablePattern "unosNumeros[]"] (UnguardedBody (
                            Sequence [
                              Sequence [
                                TypeSignature "cantidadDeImpares" (SimpleType "int" []),
                                Variable "cantidadDeImpares" None
                              ],
                              ForLoop
                                (Sequence [
                                  TypeSignature "indice" (SimpleType "int" []),
                                  Variable "indice" (MuNumber 0.0)
                                ])
                                (Application (Primitive NotEqual) [Application (Reference "[]") [Reference "unosNumeros",Reference "indice"],Reference "NULL"])
                                (Application (Primitive Plus) [Reference "indice",MuNumber 1.0])
                                (If
                                  (Application (Reference "esNumeroImpar") [Application (Reference "[]") [Reference "c",Reference "b"]])
                                  (Application (Primitive Plus) [Reference "cantidadDeImpares",MuNumber 1.0])
                                  None
                                ),
                              Return (Reference "cantidadDeImpares")]))]]

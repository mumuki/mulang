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

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module InterpreterSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Interpreter
import qualified Data.Map.Strict as Map
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Python

import           Data.Text (unpack)
import           NeatInterpolation (text)

lastRef result = (\(ref, ctx) -> (Map.! ref) . globalObjects $ ctx) <$> result

run language code = eval defaultContext  . language . unpack $ code
runjs = run js
runpy = run py

spec :: Spec
spec = do
  describe "evalExpr" $ do
    context "javascript" $ do
      it "rejects logic on number" $ do
        lastRef (runjs "1 || 2") `shouldThrow` (errorCall "Exception thrown outside try: Type error: expected two booleans but got (number) 1.0, (number) 2.0")

      it "rejects math on bools" $ do
        lastRef (runjs "true + false") `shouldThrow` (errorCall "Exception thrown outside try: Type error: expected two numbers but got (boolean) true, (boolean) false")

      it "evals addition" $ do
        lastRef (runjs "1 + 2") `shouldReturn` MuNumber 3

      it "evals subtraction" $ do
        lastRef (runjs "2 - 1") `shouldReturn` MuNumber 1

      it "evals multiplication" $ do
        lastRef (runjs "2 * 3") `shouldReturn` MuNumber 6

      it "evals mod" $ do
        lastRef (runjs "7 % 4") `shouldReturn` MuNumber 3

      it "evals and" $ do
        lastRef (runjs "true && true") `shouldReturn` MuBool True

      it "evals or" $ do
        lastRef (runjs "false || false") `shouldReturn` MuBool False

      it "evals not" $ do
        lastRef (runjs "!false") `shouldReturn` MuBool True

      it "evals comparison" $ do
        lastRef (runjs "7 > 4") `shouldReturn` MuBool True

      context "evals equal" $ do
        it "is false when values are different" $ do
          lastRef (runjs "123 == 321") `shouldReturn` MuBool False

        it "is true when values are the same" $ do
          lastRef (runjs "'123' == '123'") `shouldReturn` MuBool True

        it "is false when values are of different types" $ do
          lastRef (runjs "'123' == 123") `shouldReturn` MuBool False

      context "evals if" $ do
        it "condition is true then evals first branch" $ do
          lastRef (runjs [text|
            if(true){
              123
            } else {
              456
            }|]) `shouldReturn` MuNumber 123

        it "condition is false then evals second branch" $ do
          lastRef (runjs [text|
            if(false){
              123
            } else {
              456
            }|]) `shouldReturn` MuNumber 456

        it "condition is non bool, fails" $ do
          lastRef (runjs "if (6) { 123 } else { 456 }") `shouldThrow` (errorCall "Exception thrown outside try: Type error: expected boolean but got (number) 6.0")

      it "evals functions" $ do
        lastRef (runjs [text|
          function a() {
            return 123;
          }
          a()|]) `shouldReturn` MuNumber 123

      it "handles scopes" $ do
        lastRef (runjs [text|
          function a() {
            function b(){}
          }
          b()|]) `shouldThrow` (errorCall "Exception thrown outside try: Reference not found for name 'b'")

      it "handles whiles" $ do
        lastRef (runjs [text|
          var a = 0;

          while(a < 10) a = a + 1;
          a;|]) `shouldReturn` MuNumber 10

      it "handles fors" $ do
        lastRef (runjs [text|
          var a = 0;
          var i = 0;

          for(i = 0; i <= 10; i++) a = a + i;
          a;|]) `shouldReturn` MuNumber 55


    context "python" $ do
      it "evals addition" $ do
        lastRef (runpy "1 + 2") `shouldReturn` MuNumber 3

      it "evals subtraction" $ do
        lastRef (runpy "2 - 1") `shouldReturn` MuNumber 1

      it "evals multiplication" $ do
        lastRef (runpy "2 * 3") `shouldReturn` MuNumber 6
        lastRef (runpy "0 * 3") `shouldReturn` MuNumber 0
        lastRef (runpy "0.5 * 6") `shouldReturn` MuNumber 3

      it "evals multiplication with negative numbers" $ do
        lastRef (runpy "-1 * 5") `shouldReturn` MuNumber (-5)
        lastRef (runpy "(-1) * 5") `shouldReturn` MuNumber (-5)
        lastRef (runpy "(-1) * (-8)") `shouldReturn` MuNumber 8

      it "evals division" $ do
        lastRef (runpy "4 / 2") `shouldReturn` MuNumber 2
        lastRef (runpy "4 / 0.5") `shouldReturn` MuNumber 8

      it "evals multiple multiplications" $ do
        lastRef (runpy "2 * 3 * 10") `shouldReturn` MuNumber 60
        lastRef (runpy "(2 * 3) * 10") `shouldReturn` MuNumber 60
        lastRef (runpy "2 * (3 * 10)") `shouldReturn` MuNumber 60

      it "evals mod" $ do
        lastRef (runpy "7 % 4") `shouldReturn` MuNumber 3

      it "evals and" $ do
        lastRef (runpy "True and True") `shouldReturn` MuBool True

      it "evals or" $ do
        lastRef (runpy "False or False") `shouldReturn` MuBool False

      it "evals comparison" $ do
        lastRef (runpy "7 > 4") `shouldReturn` MuBool True

      context "evals equal" $ do
        it "is false when values are different" $ do
          lastRef (runpy "123 == 321") `shouldReturn` MuBool False

        it "is true when values are the same" $ do
          lastRef (runpy "'123' == '123'") `shouldReturn` MuBool True

        it "is false when values are of different types" $ do
          lastRef (runpy "'123' == 123") `shouldReturn` MuBool False

      context "evals if" $ do
        it "condition is true then evals first branch" $ do
          lastRef (runpy [text|
            if True: 123
            else: 456|]) `shouldReturn` MuNumber 123

        it "condition is false then evals second branch" $ do
          lastRef (runpy [text|
            if False: 123
            else: 456|]) `shouldReturn` MuNumber 456

        it "condition is non bool, fails" $ do
          lastRef (runpy [text|
            if 6: 123
            else: 456|]) `shouldThrow` (errorCall "Exception thrown outside try: Type error: expected boolean but got (number) 6.0")

      it "evals functions" $ do
        lastRef (runpy [text|
          def a():
            return 123
          a()|]) `shouldReturn` MuNumber 123

      it "evals functions with arguments" $ do
        lastRef (runpy [text|
          def double(num):
            return 2 * num

          double(5)
        |]) `shouldReturn` MuNumber 10


      it "evals functions that call other functions" $ do
          lastRef (runpy [text|
            def double(num):
              return 2 * num

            def succ(num):
              return num + 1

            def double_succ(num):
              return double(succ(num))

            double_succ(5)
          |]) `shouldReturn` MuNumber 12

      it "handles whiles" $ do
        lastRef (runpy [text|
          a = 0

          while(a < 10): a = a + 1
          a|]) `shouldReturn` MuNumber 10

      it "evals not" $ do
        lastRef (runpy "not False") `shouldReturn` MuBool True

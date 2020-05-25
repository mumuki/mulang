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

run language code = lastRef . eval defaultContext  . language . unpack $ code
runjs = run js
runpy = run py

spec :: Spec
spec = do
  describe "evalExpr" $ do
    context "javascript" $ do
      it "rejects logic on number" $ do
        (runjs "1 || 2") `shouldThrow` (errorCall "Exception thrown outside try: Type error: {Operator::Or} expected two {Type::Boolean} but got {Value::Number::1.0}, {Value::Number::2.0}")

      it "rejects math on bools" $ do
        (runjs "true + false") `shouldThrow` (errorCall "Exception thrown outside try: Type error: {Operator::Plus} expected two {Type::Number} but got {Value::Boolean::True}, {Value::Boolean::False}")

      it "evals addition" $ do
        (runjs "1 + 2") `shouldReturn` MuNumber 3

      it "evals subtraction" $ do
        (runjs "2 - 1") `shouldReturn` MuNumber 1

      it "evals multiplication" $ do
        (runjs "2 * 3") `shouldReturn` MuNumber 6

      it "evals mod" $ do
        (runjs "7 % 4") `shouldReturn` MuNumber 3

      it "evals and" $ do
        (runjs "true && true") `shouldReturn` MuBool True

      it "evals or" $ do
        (runjs "false || false") `shouldReturn` MuBool False

      it "evals not" $ do
        (runjs "!false") `shouldReturn` MuBool True

      it "evals comparison" $ do
        (runjs "7 > 4") `shouldReturn` MuBool True

      context "evals equal" $ do
        it "is false when values are different" $ do
          (runjs "123 == 321") `shouldReturn` MuBool False

        it "is true when values are the same" $ do
          (runjs "'123' == '123'") `shouldReturn` MuBool True

        it "is false when values are of different types" $ do
          (runjs "'123' == 123") `shouldReturn` MuBool False

      context "evals if" $ do
        it "condition is true then evals first branch" $ do
          (runjs [text|
            if(true){
              123
            } else {
              456
            }|]) `shouldReturn` MuNumber 123

        it "condition is false then evals second branch" $ do
          (runjs [text|
            if(false){
              123
            } else {
              456
            }|]) `shouldReturn` MuNumber 456

        it "condition is non bool, fails" $ do
          (runjs "if (6) { 123 } else { 456 }") `shouldThrow` (errorCall "Exception thrown outside try: Type error: expected {Type::Boolean} but got {Value::Number::6.0}")

      it "evals functions" $ do
        (runjs [text|
          function a() {
            return 123;
          }
          a()|]) `shouldReturn` MuNumber 123

      it "handles scopes" $ do
        (runjs [text|
          function a() {
            function b(){}
          }
          b()|]) `shouldThrow` (errorCall "Exception thrown outside try: Reference not found for name 'b'")

      it "handles whiles" $ do
        (runjs [text|
          var a = 0;

          while(a < 10) a = a + 1;
          a;|]) `shouldReturn` MuNumber 10

      it "handles fors" $ do
        (runjs [text|
          var a = 0;
          var i = 0;

          for(i = 0; i <= 10; i++) a = a + i;
          a;|]) `shouldReturn` MuNumber 55


    context "python" $ do
      it "evals addition" $ do
        (runpy "1 + 2") `shouldReturn` MuNumber 3

      it "evals subtraction" $ do
        (runpy "2 - 1") `shouldReturn` MuNumber 1

      it "evals multiplication" $ do
        (runpy "2 * 3") `shouldReturn` MuNumber 6
        (runpy "0 * 3") `shouldReturn` MuNumber 0
        (runpy "0.5 * 6") `shouldReturn` MuNumber 3

      it "evals multiplication with negative numbers" $ do
        (runpy "-1 * 5") `shouldReturn` MuNumber (-5)
        (runpy "(-1) * 5") `shouldReturn` MuNumber (-5)
        (runpy "(-1) * (-8)") `shouldReturn` MuNumber 8

      it "evals division" $ do
        (runpy "4 / 2") `shouldReturn` MuNumber 2
        (runpy "4 / 0.5") `shouldReturn` MuNumber 8

      it "evals multiple multiplications" $ do
        (runpy "2 * 3 * 10") `shouldReturn` MuNumber 60
        (runpy "(2 * 3) * 10") `shouldReturn` MuNumber 60
        (runpy "2 * (3 * 10)") `shouldReturn` MuNumber 60

      it "evals abs" $ do
        (runpy "abs(7)") `shouldReturn` MuNumber 7
        (runpy "abs(-7)") `shouldReturn` MuNumber (-7)

      it "evals max" $ do
        (runpy "max(4, 7)") `shouldReturn` MuNumber 7
        (runpy "max(10, 3)") `shouldReturn` MuNumber 10

      it "evals min" $ do
        (runpy "min(4, 7)") `shouldReturn` MuNumber 4
        (runpy "min(10, 3)") `shouldReturn` MuNumber 3

      it "evals round" $ do
        (runpy "round(4.7)") `shouldReturn` MuNumber 5
        (runpy "round(10.3)") `shouldReturn` MuNumber 10

      it "evals mod" $ do
        (runpy "7 % 4") `shouldReturn` MuNumber 3

      it "evals and" $ do
        (runpy "True and True") `shouldReturn` MuBool True

      it "evals or" $ do
        (runpy "False or False") `shouldReturn` MuBool False

      it "evals string concatenation" $ do
        (runpy "'hello' + 'world'") `shouldReturn` MuString "helloworld"
        (runpy "'hello ' + 'world'") `shouldReturn` MuString "hello world"

      it "evals string length" $ do
        (runpy "len('hello')") `shouldReturn` MuNumber 5
        (runpy "len('')") `shouldReturn` MuNumber 0

      it "evals booleans within a function" $ do
        (runpy [text|
          def is_between(x, y, z):
            return x > y and x < z

          is_between(16, 14, 20)
        |]) `shouldReturn` MuBool True

        (runpy [text|
          def is_between(x, y, z):
            return x > y and x < z

          is_between(100, 14, 20)
        |]) `shouldReturn` MuBool False

      it "evals comparison" $ do
        (runpy "7 > 4") `shouldReturn` MuBool True

      context "evals equal" $ do
        it "is false when values are different" $ do
          (runpy "123 == 321") `shouldReturn` MuBool False

        it "is true when values are the same" $ do
          (runpy "'123' == '123'") `shouldReturn` MuBool True

        it "is false when values are of different types" $ do
          (runpy "'123' == 123") `shouldReturn` MuBool False

        it "works with non-ascii strings" $ do
          (runpy [text|
            def es_fin_de_semana(dia):
              return dia == "sábado" or dia == "domingo"

            es_fin_de_semana("sábado")
          |]) `shouldReturn`  MuBool True

          (runpy [text|
            def es_fin_de_semana(dia):
              return dia == "sábado" or dia == "domingo"

            es_fin_de_semana("sabado")
          |]) `shouldReturn`  MuBool False

      context "evals if" $ do
        it "condition is true then evals first branch" $ do
          (runpy [text|
            if True: 123
            else: 456|]) `shouldReturn` MuNumber 123

        it "condition is false then evals second branch" $ do
          (runpy [text|
            if False: 123
            else: 456|]) `shouldReturn` MuNumber 456

        it "condition is non bool, fails" $ do
          (runpy [text|
            if 6: 123
            else: 456|]) `shouldThrow` (errorCall "Exception thrown outside try: Type error: expected {Type::Boolean} but got {Value::Number::6.0}")

        it "condition is a non literal expression inside a function" $ do
          (runpy [text|
            def loves_reading(num):
              if num <= 20:
                return False
              else:
                return True

            loves_reading(40)
          |]) `shouldReturn` MuBool True

      it "evals functions" $ do
        (runpy [text|
          def a():
            return 123
          a()|]) `shouldReturn` MuNumber 123

      it "evals functions with arguments" $ do
        (runpy [text|
          def double(num):
            return 2 * num

          double(5)
        |]) `shouldReturn` MuNumber 10


      it "evals functions that call other functions" $ do
          (runpy [text|
            def double(num):
              return 2 * num

            def succ(num):
              return num + 1

            def double_succ(num):
              return double(succ(num))

            double_succ(5)
          |]) `shouldReturn` MuNumber 12

      it "handles whiles" $ do
        (runpy [text|
          a = 0

          while(a < 10): a = a + 1
          a|]) `shouldReturn` MuNumber 10

      it "evals not" $ do
        (runpy "not False") `shouldReturn` MuBool True

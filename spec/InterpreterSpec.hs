{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module InterpreterSpec (spec) where

import           Test.Hspec
import           Interpreter.Mulang
import qualified Data.Map.Strict as Map
import           Language.Mulang.Parsers.JavaScript

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

lastRef result = (\(ref, ctx) -> (Map.! ref) . globalObjects $ ctx) <$> result

run expression = eval defaultContext $ js expression
run' = run . unpack

spec :: Spec
spec = do
  describe "evalExpr" $ do
    it "evals addition" $ do
      lastRef (run "1 + 2") `shouldReturn` MuNumber 3

    it "evals subtraction" $ do
      lastRef (run "2 - 1") `shouldReturn` MuNumber 1

    it "evals multiplication" $ do
      lastRef (run "2 * 3") `shouldReturn` MuNumber 6

    it "evals mod" $ do
      lastRef (run "7 % 4") `shouldReturn` MuNumber 3

    it "evals and" $ do
      lastRef (run "true && true") `shouldReturn` MuBool True

    it "evals or" $ do
      lastRef (run "false || false") `shouldReturn` MuBool False

    it "evals comparison" $ do
      lastRef (run "7 > 4") `shouldReturn` MuBool True

    context "evals equal" $ do
      it "is false when values are different" $ do
        lastRef (run "123 == 321") `shouldReturn` MuBool False

      it "is true when values are the same" $ do
        lastRef (run "'123' == '123'") `shouldReturn` MuBool True

      it "is false when values are of different types" $ do
        lastRef (run "'123' == 123") `shouldReturn` MuBool False

    context "evals if" $ do
      it "condition is true then evals first branch" $ do
        lastRef (run' [text|
          if(true){
            123
          } else {
            456
          }|]) `shouldReturn` MuNumber 123

      it "condition is false then evals second branch" $ do
        lastRef (run' [text|
          if(false){
            123
          } else {
            456
          }|]) `shouldReturn` MuNumber 456

    it "evals functions" $ do
      lastRef (run' [text|
        function a() {
          return 123;
        }
        a()|]) `shouldReturn` MuNumber 123

    it "handles scopes" $ do
      lastRef (run' [text|
        function a() {
          function b(){}
        }
        b()|]) `shouldThrow` (errorCall "Exception thrown outside try: MuString \"Reference not found for name 'b'\"")

    it "handles whiles" $ do
      lastRef (run' [text|
        var a = 0;

        while(a < 10) a = a + 1;
        a;|]) `shouldReturn` MuNumber 10

    it "handles fors" $ do
      lastRef (run' [text|
        var a = 0;
        var i = 0;

        for(i = 0; i <= 10; i++) a = a + i;
        a;|]) `shouldReturn` MuNumber 55

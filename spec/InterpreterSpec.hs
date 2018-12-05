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

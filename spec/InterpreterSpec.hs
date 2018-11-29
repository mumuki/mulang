module InterpreterSpec (spec) where

import           Test.Hspec
import           Interpreter.Mulang
import qualified Language.Mulang.Ast as Mu
import qualified Data.Map.Strict as Map

lastRef result = (\(ref, ctx) -> (Map.! ref) . globalObjects $ ctx) <$> result

run expression = eval defaultContext expression

spec :: Spec
spec = do
  describe "evalExpr" $ do
    it "evals addition" $ do
      let expression = Mu.Application (Mu.Reference "+") [Mu.MuNumber 1, Mu.MuNumber 2]
      lastRef (run expression) `shouldReturn` MuNumber 3

    it "evals subtraction" $ do
      let expression = Mu.Application (Mu.Reference "-") [Mu.MuNumber 2, Mu.MuNumber 1]
      lastRef (run expression) `shouldReturn` MuNumber 1

    it "evals multiplication" $ do
      let expression = Mu.Application (Mu.Reference "*") [Mu.MuNumber 2, Mu.MuNumber 3]
      lastRef (run expression) `shouldReturn` MuNumber 6

    it "evals mod" $ do
      let expression = Mu.Application (Mu.Reference "%") [Mu.MuNumber 7, Mu.MuNumber 4]
      lastRef (run expression) `shouldReturn` MuNumber 3

    it "evals and" $ do
      let expression = Mu.Application (Mu.Reference "&&") [Mu.MuBool True, Mu.MuBool True]
      lastRef (run expression) `shouldReturn` MuBool True

    it "evals or" $ do
      let expression = Mu.Application (Mu.Reference "||") [Mu.MuBool False, Mu.MuBool False]
      lastRef (run expression) `shouldReturn` MuBool False

    it "evals comparison" $ do
      let expression = Mu.Application (Mu.Reference ">") [Mu.MuNumber 7, Mu.MuNumber 4]
      lastRef (run expression) `shouldReturn` MuBool True

    context "evals equal" $ do
      it "is false when values are different" $ do
        let expression = Mu.Application Mu.Equal [Mu.MuString "123", Mu.MuString "321"]
        lastRef (run expression) `shouldReturn` MuBool False

      it "is true when values are the same" $ do
        let expression = Mu.Application Mu.Equal [Mu.MuString "123", Mu.MuString "123"]
        lastRef (run expression) `shouldReturn` MuBool True

      it "is false when values are of different types" $ do
        let expression = Mu.Application Mu.Equal [Mu.MuString "123", Mu.MuNumber 123]
        lastRef (run expression) `shouldReturn` MuBool False

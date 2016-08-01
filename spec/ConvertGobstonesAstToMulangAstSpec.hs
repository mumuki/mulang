module ConvertGobstonesAstToMulangAstSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import			 Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do

describe "translateProgramGobstonesToMulangExpression" $ do
    it "translate simple program Gobstones" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": null,\"from\": 0}]"

      gobstonesAst `shouldBe` Sequence [MuNull]

    it "translate null program Gobstones" $ do
      let gobstonesAst =  parseGobstones "null"

      gobstonesAst `shouldBe` MuNull

    it "translate simple procedure declaration " $ do
      let gobstonesAst =  parseGobstones  "[{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\": []}]"

      gobstonesAst `shouldBe` Sequence  [ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]

    it "translate simple procedure Call" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0}]"
      									
      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []]]

    it "translate simple procedure Application " $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": [{\"alias\": \"ProcedureCall\",\"from\": 14,\"to\": 18,\"arity\": \"routine\",\"name\": \"F\",\"parameters\": []}],\"from\": 0},{\"alias\": \"procedureDeclaration\",\"body\": null,\"from\": 1,\"row\": 1,\"to\": 12,\"value\": \"F\",\"arity\": \"routine\",\"reserved\": false,\"led\": null,\"lbp\": 0,\"name\": \"F\",\"parameters\": []}]"

      gobstonesAst `shouldBe` Sequence [Sequence [Application (Variable "F") []],ProcedureDeclaration "F" [Equation [] (UnguardedBody MuNull)]]



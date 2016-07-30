module ConvertGobstonesAstToMulangAstSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import			 Language.Mulang.Parsers.Gobstones

spec :: Spec
spec = do

describe "translateProgramGobstonesToMulangExpression" $ do
    it "translate simple program Gobstones" $ do
      let gobstonesAst =  parseGobstones "[{\"alias\": \"program\",\"body\": \"null\",\"from\": \"0\"}]"

      gobstonesAst `shouldBe` Program []

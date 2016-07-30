module ConvertGobstonesAstToMulangAstSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import			 Language.Mulang.Parsers.Gobstones
import 			 Language.Mulang.Parsers.Json

spec :: Spec
spec = do

describe "translateProgramGobstonesToMulangExpression" $ do
    it "translate simple program Gobstones" $ do
      let gobstonesAst =  json  "[{\"alias\": \"program\",\"body\": \"null\",\"from\": \"0\"}]"

      (translateGobstonesAst gobstonesAst) `shouldBe` Program []

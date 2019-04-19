module ExpectationsCorrectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Operators
import           Language.Mulang.Operators.Haskell (haskellTokensTable)
import           Language.Mulang.Operators.Ruby (rubyTokensTable)
import           Language.Mulang.Operators.Java (javaTokensTable)
import           Language.Mulang.Operators.Python (pythonTokensTable)
import           Language.Mulang.Ast (PrimitiveOperator (..))

import           Control.Applicative ((<|>))
import           Control.Monad ((>=>))

import           Text.Read (readMaybe)
import           Data.List (stripPrefix)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Inspection = String
type KeywordInspectionsTable = Map Token Inspection

buildUsageInspection :: PrimitiveOperator -> Inspection
buildUsageInspection = ("Uses" ++) . show

unbuildUsageInspection :: Inspection -> Maybe PrimitiveOperator
unbuildUsageInspection = stripPrefix "Uses" >=> readMaybe

unbuildDeclarationInspection :: Inspection -> Maybe PrimitiveOperator
unbuildDeclarationInspection = stripPrefix "Declares" >=> readMaybe

primitiveUsage = unbuildUsageInspection

tokensTable :: Language -> TokensTable
tokensTable Haskell = haskellTokensTable
tokensTable Java = javaTokensTable
tokensTable Ruby = rubyTokensTable
tokensTable Python = pythonTokensTable

keywordInspectionsTable :: Language -> KeywordInspectionsTable
keywordInspectionsTable Haskell = Map.fromList [
  ("type", "DeclaresTypeAlias"),
  ("if", "UsesIf")
 ]
keywordInspectionsTable Java = Map.fromList [
  ("if", "UsesIf"),
  ("class", "DeclaresClass"),
  ("interface", "DeclaresInterface"),
  ("for", "UsesForLoop")
 ]
keywordInspectionsTable Ruby = Map.fromList [
  ("if", "UsesIf"),
  ("class", "DeclaresClass"),
  ("def", "DeclaresComputation"),
  ("for", "UsesForeach"),
  ("include",  "Includes")
 ]
keywordInspectionsTable Python = Map.fromList [
  ("if", "UsesIf"),
  ("class", "DeclaresClass"),
  ("def", "DeclaresComputation"),
  ("for", "UsesForeach")
 ]

operatorInspection :: Language -> Token -> Maybe Inspection
operatorInspection language target = fmap buildUsageInspection . (Map.lookup target) . buildOperatorsTable . tokensTable $ language

keywordInspection :: Language -> Token -> Maybe Inspection
keywordInspection language target = Map.lookup target . keywordInspectionsTable $ language

run :: Language -> Token -> Maybe Inspection
run language  target = operatorInspection language target <|> keywordInspection language target

spec :: Spec
spec = do
  describe "correct primitive usages" $ do
    it "corrects haskell and" $ do
      run Haskell "otherwise"  `shouldBe` (Just "UsesOtherwise")

    it "corrects haskell and" $ do
      run Haskell "and" `shouldBe` Nothing
      run Haskell "&&"  `shouldBe` (Just "UsesAnd")

    it "corrects haskell or" $ do
      run Haskell "or" `shouldBe` Nothing
      run Haskell "||" `shouldBe` (Just "UsesOr")

    it "corrects haskell not" $ do
      run Haskell "not" `shouldBe` (Just "UsesNegation")
      run Haskell "!"   `shouldBe`  Nothing

    it "corrects java and" $ do
      run Java    "and" `shouldBe` Nothing
      run Java    "&&"  `shouldBe` (Just "UsesAnd")

    it "corrects python and" $ do
      run Python  "and" `shouldBe` (Just "UsesAnd")
      run Python  "&&"  `shouldBe` Nothing

    it "corrects ruby and" $ do
      run Ruby    "and" `shouldBe` (Just "UsesAnd")
      run Ruby    "&&"  `shouldBe` (Just "UsesAnd")

  describe "corrects keyword usages" $ do
    it "corrects haskell type" $ do
      run Haskell "type" `shouldBe` (Just "DeclaresTypeAlias")

    it "corrects java if" $ do
      run Java    "if"     `shouldBe` (Just  "UsesIf")

    it "corrects java class" $ do
      run Java    "class"  `shouldBe` (Just "DeclaresClass")

    it "corrects java interface" $ do
      run Java    "interface"  `shouldBe` (Just "DeclaresInterface")

    it "corrects java for" $ do
      run Java    "for"  `shouldBe` (Just "UsesForLoop")

    it "corrects python def" $ do
      run Python  "def" `shouldBe` (Just "DeclaresComputation")

    it "corrects ruby class" $ do
      run Ruby    "class"  `shouldBe` (Just "DeclaresClass")

    it "corrects ruby include" $ do
      run Ruby    "include"  `shouldBe` (Just "Includes")

    it "corrects ruby def" $ do
      run Ruby  "def" `shouldBe` (Just "DeclaresComputation")

-- Module por synthesizing inspections
-- from tokens, keywords and operators
module Language.Mulang.Synthesizer (
  synthesizeInspection,
  encodeUsageInspection,
  decodeUsageInspection,
  decodeDeclarationInspection
) where

import           Language.Mulang.Analyzer.Analysis (Language(..))
import           Language.Mulang.Operators
import           Language.Mulang.Operators.Haskell (haskellTokensTable)
import           Language.Mulang.Operators.Ruby (rubyTokensTable)
import           Language.Mulang.Operators.Java (javaTokensTable)
import           Language.Mulang.Operators.Python (pythonTokensTable)
import           Language.Mulang.Ast (Operator (..))

import           Control.Applicative ((<|>))
import           Control.Monad ((>=>))

import           Text.Read (readMaybe)
import           Data.List (stripPrefix)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Inspection = String
type KeywordInspectionsTable = Map Token Inspection

-- converts an operator into an inspection
encodeUsageInspection :: Operator -> Inspection
encodeUsageInspection = ("Uses" ++) . show

-- extract an operator from an inspection
decodeUsageInspection :: Inspection -> Maybe Operator
decodeUsageInspection = stripPrefix "Uses" >=> readMaybe

decodeDeclarationInspection :: Inspection -> Maybe Operator
decodeDeclarationInspection = stripPrefix "Declares" >=> readMaybe

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

synthesizeInspection :: Language -> Token -> Maybe Inspection
synthesizeInspection language  target = operatorInspection <|> keywordInspection
  where
    operatorInspection :: Maybe Inspection
    operatorInspection = fmap encodeUsageInspection . (Map.lookup target) . buildOperatorsTable . tokensTable $ language

    keywordInspection :: Maybe Inspection
    keywordInspection = Map.lookup target . keywordInspectionsTable $ language

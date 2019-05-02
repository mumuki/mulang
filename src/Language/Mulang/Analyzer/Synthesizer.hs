-- Module por synthesizing inspections
-- from tokens, keywords and operators
module Language.Mulang.Analyzer.Synthesizer (
  encodeUsageInspection,
  encodeDeclarationInspection,
  decodeUsageInspection,
  decodeDeclarationInspection,
) where

import           Language.Mulang.Analyzer.Analysis (Language(..), Inspection)

import           Language.Mulang.Operators (parseOperator, Token, TokensTable)
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

type KeywordInspectionsTable = Map Token Inspection
type Encoder = Operator -> Inspection
type Decoder = Inspection -> Maybe Operator

-- converts an operator into an inspection
encodeUsageInspection :: Encoder
encodeUsageInspection = encodeInspection "Uses"

encodeDeclarationInspection :: Encoder
encodeDeclarationInspection = encodeInspection "Declares"

encodeInspection :: String -> Encoder
encodeInspection prefix = (prefix ++) . show

-- extract an operator from an inspection

decodeUsageInspection :: Decoder
decodeUsageInspection = decodeInspection "Uses"

decodeDeclarationInspection :: Decoder
decodeDeclarationInspection = decodeInspection "Declares"

decodeInspection :: String -> Decoder
decodeInspection prefix = stripPrefix prefix >=> readMaybe

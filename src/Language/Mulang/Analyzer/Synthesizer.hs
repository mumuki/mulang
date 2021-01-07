-- Module por synthesizing inspections
-- from tokens, keywords and operators
module Language.Mulang.Analyzer.Synthesizer (
  encodeUsageInspection,
  encodeDeclarationInspection,
  decodeIsInspection,
  decodeUsageInspection,
  decodeDeclarationInspection,
  generateInspectionEncodingRules,
  generateOperatorEncodingRules
) where

import           Language.Mulang.Analyzer.Analysis (Inspection)

import           Language.Mulang.Operators (Token)
import           Language.Mulang.Ast.Operator (Operator)

import           Control.Monad ((>=>))

import           Text.Read (readMaybe)
import           Data.List (stripPrefix)

type Encoder a = a -> Inspection
type Decoder a = Inspection -> Maybe a

type EncodingRuleGenerator a b = (a, b) -> [(Inspection, Inspection)]

-- converts an operator into an inspection
encodeUsageInspection :: Encoder Operator
encodeUsageInspection = encodeInspection "Uses"

encodeDeclarationInspection :: Encoder Operator
encodeDeclarationInspection = encodeInspection "Declares"

encodeInspection :: String -> Encoder Operator
encodeInspection prefix = (prefix ++) . show

-- extract an operator from an inspection

decodeIsInspection :: Decoder Operator
decodeIsInspection = decodeInspection "Is"

decodeUsageInspection :: Decoder Operator
decodeUsageInspection = decodeInspection "Uses"

decodeDeclarationInspection :: Decoder Operator
decodeDeclarationInspection = decodeInspection "Declares"

decodeInspection :: String -> Decoder Operator
decodeInspection prefix = stripPrefix prefix >=> readMaybe

generateInspectionEncodingRules :: EncodingRuleGenerator Token Inspection
generateInspectionEncodingRules = generateEncodingRules id id

generateOperatorEncodingRules :: EncodingRuleGenerator Token Operator
generateOperatorEncodingRules = generateEncodingRules encodeUsageInspection encodeDeclarationInspection

generateEncodingRules :: Encoder a -> Encoder a -> EncodingRuleGenerator Token a
generateEncodingRules usageEncoder declarationEncoder (k, v) = concatMap generateEncodingNegationRules baseEncodings
  where
    baseEncodings = [("Uses:" ++ k, usageEncoder v), ("Declares:" ++ k, declarationEncoder v)]

    generateEncodingNegationRules :: EncodingRuleGenerator Inspection Inspection
    generateEncodingNegationRules (k, v) = [(k, v), ("Not:" ++ k, "Not:" ++ v)]

module Language.Mulang.Analyzer.Autocorrector (autocorrect) where

import           Language.Mulang.Analyzer.Analysis
import           Language.Mulang.Analyzer.Synthesizer (generateOperatorEncodingRules, generateInspectionEncodingRules)

import           Language.Mulang.Operators (Token, OperatorsTable, buildOperatorsTable)
import           Language.Mulang.Operators.Haskell (haskellTokensTable)
import           Language.Mulang.Operators.Ruby (rubyTokensTable)
import           Language.Mulang.Operators.Java (javaTokensTable)
import           Language.Mulang.Operators.Python (pythonTokensTable)

import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map


-- | Computes a derived Analysis infering missing values from context
--
-- It performs the following corrections:
--
--  1. fills originalLanguage when it is not present but can be inferred from the code sample
--  2. fills the autocorrectionRules when they are not present but can be inferred from the originalLanguage
--  3. aguments the autocorrectionRules with operators-based rules when they can be inferred from the originalLanguage
--  4. corrects the expectations' inspections using the autocorrectionRules
--  5. fills the domainLanguage rules when it is not present but can be inferred from the originalLanguage
--  6. fills the domainLanguage's caseStyle when it is not present but can be inferred from the originalLanguage
autocorrect :: Analysis -> Analysis
autocorrect (Analysis f s@(AnalysisSpec { originalLanguage = Just _ })) = Analysis f (autocorrectSpec s)
autocorrect (Analysis f@(CodeSample { language = l } ) s)               = autocorrect (Analysis f s { originalLanguage = Just l }) -- (1)
autocorrect a                                                           = a

autocorrectSpec :: AnalysisSpec -> AnalysisSpec
autocorrectSpec s = runFixes [rulesFix, rulesAgumentationFix, expectationsFix, emptyDomainLanguageFix, domainLanguageCaseStyleFix]
  where
    runFixes = foldl combine s . map ($ (justOriginalLanguage s))
    combine s f | Just s' <- f s = s'
                | otherwise = s

-- Fixes

type Fix = Language -> AnalysisSpec -> Maybe AnalysisSpec

rulesFix :: Fix  -- (2)
rulesFix l s = do
  AnalysisSpec { autocorrectionRules = Nothing } <- Just s
  return s { autocorrectionRules = Just (inferAutocorrectionRules l) }

rulesAgumentationFix :: Fix -- (3)
rulesAgumentationFix l s = do
  AnalysisSpec { autocorrectionRules = Just rules } <- Just s
  return s { autocorrectionRules = Just (augmentRules rules (inferOperatorsTable l)) }
  where
    augmentRules :: AutocorrectionRules -> OperatorsTable -> AutocorrectionRules
    augmentRules rules tokens = Map.fromList (Map.toList rules ++ (concatMap generateOperatorEncodingRules . Map.toList) tokens)

expectationsFix :: Fix -- (4)
expectationsFix _ s = do
  AnalysisSpec { expectations = Just es } <- Just s
  return s { expectations = Just (map autocorrectExpectation es) }
  where
    autocorrectExpectation :: Expectation -> Expectation
    autocorrectExpectation (Expectation b i) = Expectation b . fromMaybe i . Map.lookup i . justAutocorrectionRules $ s

emptyDomainLanguageFix :: Fix  -- (5)
emptyDomainLanguageFix _ s = do
  AnalysisSpec { domainLanguage = Nothing } <- Just s
  return s { domainLanguage = Just emptyDomainLanguage }

domainLanguageCaseStyleFix :: Fix -- (6)
domainLanguageCaseStyleFix l s = do
  AnalysisSpec { domainLanguage = Just dl } <- Just s
  DomainLanguage { caseStyle = Nothing } <- Just dl
  return s { domainLanguage = Just (dl { caseStyle = Just (inferCaseStyle l) }) }

-- Inferences

type Inference a = Language -> a

inferOperatorsTable :: Inference OperatorsTable
inferOperatorsTable = buildOperatorsTable . table
  where
    table Haskell = haskellTokensTable
    table Java    = javaTokensTable
    table Ruby    = rubyTokensTable
    table Python  = pythonTokensTable
    table Python2 = pythonTokensTable
    table Python3 = pythonTokensTable
    table _       = Map.empty

inferCaseStyle :: Inference CaseStyle
inferCaseStyle Python  = RubyCase
inferCaseStyle Python2 = RubyCase
inferCaseStyle Python3 = RubyCase
inferCaseStyle Ruby    = RubyCase
inferCaseStyle _       = CamelCase

inferAutocorrectionRules :: Inference AutocorrectionRules
inferAutocorrectionRules = buildRules . rules
  where
    buildRules :: [(Token, Inspection)] -> AutocorrectionRules
    buildRules = Map.fromList . concatMap generateInspectionEncodingRules

    rules Haskell = [
        ("type", "DeclaresTypeAlias"),
        ("if", "UsesIf")
      ]
    rules Java = [
        ("if", "UsesIf"),
        ("class", "DeclaresClass"),
        ("interface", "DeclaresInterface"),
        ("for", "UsesForLoop")
      ]
    rules Ruby = [
        ("if", "UsesIf"),
        ("class", "DeclaresClass"),
        ("def", "DeclaresComputation"),
        ("for", "UsesForeach"),
        ("include",  "Includes")
      ]
    rules Python = [
        ("if", "UsesIf"),
        ("class", "DeclaresClass"),
        ("def", "DeclaresComputation"),
        ("for", "UsesForeach")
      ]
    rules Python2 = rules Python
    rules Python3 = rules Python
    rules _ = []

-- Misc

justOriginalLanguage = fromJust . originalLanguage
justAutocorrectionRules = fromJust . autocorrectionRules

module Language.Mulang.Analyzer.Autocorrector (autocorrect) where

import           Language.Mulang.Analyzer.Analysis

import           Language.Mulang.Operators (Token)

import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map


-- | Computes a derived Analysis infering missing values from context
--
-- It performs the following corrections:
--
--  1. fills originalLanguage when it is not present but can be inferred from the code sample
--  2. fills the autocorrectionRules when they are not present but can be inferred from the originalLanguage
--  3. corrects the expectations' inspections using the autocorrectionRules
--  4. fills the domainLanguage rules when it is not present but can be inferred from the originalLanguage
--  5. fills the domainLanguage's caseStyle when it is not present but can be inferred from the originalLanguage
autocorrect :: Analysis -> Analysis
autocorrect (Analysis f s@(AnalysisSpec { originalLanguage = Just l })) = Analysis f (autocorrectSpec l s)
autocorrect (Analysis f@(CodeSample { language = l } ) s)               = autocorrect (Analysis f s { originalLanguage = Just l }) -- (1)
autocorrect a                                                           = a

autocorrectSpec :: Language -> AnalysisSpec -> AnalysisSpec
autocorrectSpec l s = runFixes [rulesFix, expectationsFix, emptyDomainLanguageFix, domainLanguageCaseStyleFix]
  where
    runFixes = foldl combine s . map ($l)
    combine s f | Just s' <- f s = s'
                | otherwise = s

-- Fixes

type Fix = Language -> AnalysisSpec -> Maybe AnalysisSpec

rulesFix :: Fix  -- (2)
rulesFix l s = do
  AnalysisSpec { autocorrectionRules = Nothing } <- Just s
  return s { autocorrectionRules = Just (inferAutocorrectionRules l) }

expectationsFix :: Fix -- (3)
expectationsFix l s = do
  AnalysisSpec { expectations = Just es } <- Just s
  return s { expectations = Just (autocorrectExpectations l es) }

emptyDomainLanguageFix :: Fix  -- (4)
emptyDomainLanguageFix l s = do
  AnalysisSpec { domainLanguage = Nothing } <- Just s
  return s { domainLanguage = Just emptyDomainLanguage }

domainLanguageCaseStyleFix :: Fix -- (5)
domainLanguageCaseStyleFix l s = do
  AnalysisSpec { domainLanguage = Just dl } <- Just s
  DomainLanguage { caseStyle = Nothing } <- Just dl
  return s { domainLanguage = Just (dl { caseStyle = Just (inferCaseStyle l) }) }


-- Misc

autocorrectExpectations :: Language -> [Expectation] -> [Expectation]
autocorrectExpectations l = map (autocorrectExpectation l)

autocorrectExpectation :: Language -> Expectation -> Expectation
autocorrectExpectation l (Expectation b i) = Expectation b . fromMaybe i . Map.lookup i . inferAutocorrectionRules $ l

inferCaseStyle :: Language -> CaseStyle
inferCaseStyle Python  = RubyCase
inferCaseStyle Python2 = RubyCase
inferCaseStyle Python3 = RubyCase
inferCaseStyle Ruby    = RubyCase
inferCaseStyle _       = CamelCase

buildAutocorrectorRules :: [(Token, Inspection)] -> AutocorrectionRules
buildAutocorrectorRules = Map.fromList . concatMap encodeEntry
  where
    encodeEntry :: (Token, Inspection) -> [(Inspection, Inspection)]
    encodeEntry (token, inspection) = map (\e -> (e token, inspection)) [("Uses:" ++), ("Declares:"++)]

inferAutocorrectionRules :: Language -> AutocorrectionRules
inferAutocorrectionRules Haskell =
  buildAutocorrectorRules [
    ("type", "DeclaresTypeAlias"),
    ("if", "UsesIf")
  ]
inferAutocorrectionRules Java =
  buildAutocorrectorRules [
    ("if", "UsesIf"),
    ("class", "DeclaresClass"),
    ("interface", "DeclaresInterface"),
    ("for", "UsesForLoop")
  ]
inferAutocorrectionRules Ruby =
  buildAutocorrectorRules [
    ("if", "UsesIf"),
    ("class", "DeclaresClass"),
    ("def", "DeclaresComputation"),
    ("for", "UsesForeach"),
    ("include",  "Includes")
  ]
inferAutocorrectionRules Python =
  buildAutocorrectorRules [
    ("if", "UsesIf"),
    ("class", "DeclaresClass"),
    ("def", "DeclaresComputation"),
    ("for", "UsesForeach")
  ]
inferAutocorrectionRules _ = Map.empty


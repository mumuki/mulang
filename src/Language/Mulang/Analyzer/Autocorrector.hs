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
autocorrectSpec l s@(AnalysisSpec { autocorrectionRules = Nothing }) = autocorrectSpec l s { autocorrectionRules = Just (inferAutocorrectionRules l) } -- (2)
-- autocorrectSpec l s@(AnalysisSpec { expectations = Just es })        = autocorrectSpec l s { expectations = Just (autocorrectExpectations l es) }
autocorrectSpec l s@(AnalysisSpec { domainLanguage = Nothing })      = autocorrectSpec l s { domainLanguage = Just emptyDomainLanguage } -- (4)
autocorrectSpec l s@(AnalysisSpec { domainLanguage = Just dl })      = s { domainLanguage = Just (autocorrectDomainLanguage l dl) }

autocorrectDomainLanguage :: Language -> DomainLanguage -> DomainLanguage
autocorrectDomainLanguage l dl@(DomainLanguage { caseStyle = Nothing }) = dl { caseStyle = Just (inferCaseStyle l) } -- (5)
autocorrectDomainLanguage _ dl                                          = dl

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


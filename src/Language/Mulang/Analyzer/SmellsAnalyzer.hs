module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.DomainLanguage
import Language.Mulang.Analyzer.Analysis hiding (DomainLanguage)
import Data.List
import Data.Maybe (fromMaybe)

analyseSmells :: Expression -> DomainLanguage -> SmellsSet -> [Expectation]
analyseSmells ast language set = concatMap (\smell -> detectSmell smell language ast) (smellsFor set)

detectSmell :: Smell -> DomainLanguage -> Expression -> [Expectation]
detectSmell smell language =  map (exectationFor smell) . detectionFor smell language

smellsFor :: SmellsSet -> [Smell]
smellsFor (NoSmells included)    = fromMaybe [] included
smellsFor (AllSmells excluded)   = allSmells \\ (fromMaybe [] excluded)

allSmells :: [Smell]
allSmells = [
  "DiscardsExceptions",
  "DoesConsolePrint",
  "DoesNullTest",
  "DoesTypeTest",
  "HasAssignmentReturn",
  "HasCodeDuplication",
  "HasMisspelledIdentifiers",
  "HasRedundantBooleanComparison",
  "HasRedundantGuards",
  "HasRedundantIf",
  "HasRedundantLambda",
  "HasRedundantLocalVariableReturn",
  "HasRedundantParameter",
  "HasRedundantReduction",
  "HasTooShortIdentifiers",
  "HasWrongCaseIdentifiers",
  "IsLongCode",
  "ReturnsNull",
  "UsesCut",
  "UsesFail",
  "UsesUnificationOperator" ]

detectionFor :: Smell -> Detection
detectionFor "DiscardsExceptions"              = simple discardsExceptions
detectionFor "DoesConsolePrint"                = simple doesConsolePrint
detectionFor "DoesNullTest"                    = simple doesNullTest
detectionFor "DoesTypeTest"                    = simple doesTypeTest
detectionFor "HasAssignmentReturn"             = simple hasAssignmentReturn
detectionFor "HasCodeDuplication"              = unsupported
detectionFor "HasMisspelledIdentifiers"        = withLanguage hasMisspelledIdentifiers
detectionFor "HasMisspelledBindings"           = withLanguage hasMisspelledIdentifiers
detectionFor "HasRedundantBooleanComparison"   = simple hasRedundantBooleanComparison
detectionFor "HasRedundantGuards"              = simple hasRedundantGuards
detectionFor "HasRedundantIf"                  = simple hasRedundantIf
detectionFor "HasRedundantLambda"              = simple hasRedundantLambda
detectionFor "HasRedundantLocalVariableReturn" = simple hasRedundantLocalVariableReturn
detectionFor "HasRedundantParameter"           = simple hasRedundantParameter
detectionFor "HasRedundantReduction"           = simple hasRedundantReduction
detectionFor "HasTooShortIdentifiers"          = withLanguage hasTooShortIdentifiers
detectionFor "HasTooShortBindings"             = withLanguage hasTooShortIdentifiers
detectionFor "HasWrongCaseIdentifiers"         = withLanguage hasWrongCaseIdentifiers
detectionFor "HasWrongCaseBinding"             = withLanguage hasWrongCaseIdentifiers
detectionFor "IsLongCode"                      = unsupported
detectionFor "ReturnsNull"                     = simple returnsNull
detectionFor "UsesCut"                         = simple usesCut
detectionFor "UsesFail"                        = simple usesFail
detectionFor "UsesUnificationOperator"         = simple usesUnificationOperator
detectionFor _                                 = unsupported

type Detection = DomainLanguage -> Expression -> [Identifier]
unsupported :: Detection
unsupported _ _ = []

simple :: Inspection -> Detection
simple inspection _ = detect inspection

withLanguage :: (DomainLanguage -> Inspection) -> Detection
withLanguage inspection language = detect (inspection language)

exectationFor :: Smell -> Identifier -> Expectation
exectationFor smell identifier = Expectation identifier smell

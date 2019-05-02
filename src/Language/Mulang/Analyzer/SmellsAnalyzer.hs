module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.DomainLanguage
import Language.Mulang.Analyzer.Analysis hiding (DomainLanguage, Inspection, allSmells)
import Data.List ((\\))
import Data.Maybe (fromMaybe)

analyseSmells :: Expression -> DomainLanguage -> Maybe SmellsSet -> [Expectation]
analyseSmells ast language = concatMap (\smell -> detectSmell smell language ast) . smellsFor . fromMaybe (NoSmells Nothing)

detectSmell :: Smell -> DomainLanguage -> Expression -> [Expectation]
detectSmell smell language =  map (exectationFor smell) . detectionFor smell language

smellsFor :: SmellsSet -> [Smell]
smellsFor (NoSmells included)    = fromMaybe [] included
smellsFor (AllSmells excluded)   = allSmells \\ (fromMaybe [] excluded)

allSmells :: [Smell]
allSmells = [
  "DiscardsExceptions",
  "DoesConsolePrint",
  "DoesNilTest",
  "DoesTypeTest",
  "HasAssignmentReturn",
  "HasCodeDuplication",
  "HasEmptyIfBranches",
  "HasLongParameterList",
  "HasMisspelledIdentifiers",
  "HasRedundantBooleanComparison",
  "HasRedundantGuards",
  "ShouldUseOtherwise",
  "HasRedundantIf",
  "HasRedundantLambda",
  "HasRedundantLocalVariableReturn",
  "HasRedundantParameter",
  "HasRedundantReduction",
  "HasTooManyMethods",
  "HasTooShortIdentifiers",
  "HasUnreachableCode",
  "HasWrongCaseIdentifiers",
  "IsLongCode",
  "OverridesEqualOrHashButNotBoth",
  "ReturnsNil",
  "UsesCut",
  "UsesFail",
  "UsesUnificationOperator" ]

detectionFor :: Smell -> Detection
detectionFor "DiscardsExceptions"              = simple discardsExceptions
detectionFor "DoesConsolePrint"                = simple doesConsolePrint
detectionFor "DoesNilTest"                     = simple doesNilTest
detectionFor "DoesNullTest"                    = simple doesNilTest
detectionFor "DoesTypeTest"                    = simple doesTypeTest
detectionFor "HasAssignmentReturn"             = simple hasAssignmentReturn
detectionFor "HasCodeDuplication"              = unsupported
detectionFor "HasEmptyIfBranches"              = simple hasEmptyIfBranches
detectionFor "HasLongParameterList"            = simple hasLongParameterList
detectionFor "HasMisspelledBindings"           = withLanguage hasMisspelledIdentifiers
detectionFor "HasMisspelledIdentifiers"        = withLanguage hasMisspelledIdentifiers
detectionFor "HasRedundantBooleanComparison"   = simple hasRedundantBooleanComparison
detectionFor "HasRedundantGuards"              = simple hasRedundantGuards
detectionFor "ShouldUseOtherwise"              = simple shouldUseOtherwise
detectionFor "HasRedundantIf"                  = simple hasRedundantIf
detectionFor "HasRedundantLambda"              = simple hasRedundantLambda
detectionFor "HasRedundantLocalVariableReturn" = simple hasRedundantLocalVariableReturn
detectionFor "HasRedundantParameter"           = simple hasRedundantParameter
detectionFor "HasRedundantReduction"           = simple hasRedundantReduction
detectionFor "HasTooManyMethods"               = simple hasTooManyMethods
detectionFor "HasTooShortBindings"             = withLanguage hasTooShortIdentifiers
detectionFor "HasTooShortIdentifiers"          = withLanguage hasTooShortIdentifiers
detectionFor "HasUnreachableCode"              = simple hasUnreachableCode
detectionFor "HasWrongCaseBinding"             = withLanguage hasWrongCaseIdentifiers
detectionFor "HasWrongCaseIdentifiers"         = withLanguage hasWrongCaseIdentifiers
detectionFor "IsLongCode"                      = unsupported
detectionFor "OverridesEqualOrHashButNotBoth"  = simple overridesEqualOrHashButNotBoth
detectionFor "ReturnsNil"                      = simple returnsNil
detectionFor "ReturnsNull"                     = simple returnsNil
detectionFor "UsesCut"                         = simple usesCut
detectionFor "UsesFail"                        = simple usesFail
detectionFor "UsesUnificationOperator"         = simple usesUnificationOperator
detectionFor _                                 = unsupported

type Detection = DomainLanguage -> Expression -> [Identifier]
unsupported :: Detection
unsupported _ _ = []

simple :: Inspection -> Detection
simple inspection _ = locate' inspection

withLanguage :: (DomainLanguage -> Inspection) -> Detection
withLanguage inspection language = locate' (inspection language)

exectationFor :: Smell -> Identifier -> Expectation
exectationFor smell identifier = Expectation identifier smell

locate' inspection = identifiers . locate inspection
  where identifiers Nowhere      = []
        identifiers TopLevel     = ["*"]
        identifiers (Nested ids) = ids

{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang.Ast (Expression, Identifier)
import Language.Mulang.DomainLanguage
import Language.Mulang.Inspector.Primitive
import Language.Mulang.Inspector.Logic
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.Inspector.Combiner (Location(..), locate)
import Language.Mulang.Edl.Expectation (Query (..), CQuery (..), Matcher(..), Predicate(..))

import Language.Mulang.Analyzer.Analysis hiding (DomainLanguage, Inspection, allSmells)

import Data.List ((\\))
import Data.Maybe (fromMaybe, mapMaybe)

type SmellsContext = ([QueryResult], DomainLanguage)

analyseSmells :: Expression -> SmellsContext -> Maybe SmellsSet -> [Expectation]
analyseSmells ast context = concatMap (\smell -> detectSmell smell context ast) . smellsFor . fromMaybe (NoSmells Nothing)

detectSmell :: Smell -> SmellsContext -> Expression -> [Expectation]
detectSmell smell context =  map (exectationFor smell) . detectionFor smell context

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
  "HasEmptyRepeat",
  "HasLongParameterList",
  "HasMisspelledIdentifiers",
  "HasRedundantBooleanComparison",
  "HasRedundantGuards",
  "HasRedundantIf",
  "HasRedundantLambda",
  "HasRedundantLocalVariableReturn",
  "HasRedundantParameter",
  "HasRedundantReduction",
  "HasRedundantRepeat",
  "HasTooManyMethods",
  "HasTooShortIdentifiers",
  "HasUnreachableCode",
  "HasWrongCaseIdentifiers",
  "IsLongCode",
  "OverridesEqualOrHashButNotBoth",
  "ReturnsNil",
  "ShouldInvertIfCondition",
  "ShouldUseOtherwise",
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
detectionFor "HasEmptyRepeat"                  = simple hasEmptyRepeat
detectionFor "HasLongParameterList"            = simple hasLongParameterList
detectionFor "HasMisspelledBindings"           = withLanguage hasMisspelledIdentifiers
detectionFor "HasMisspelledIdentifiers"        = withLanguage hasMisspelledIdentifiers
detectionFor "HasRedundantBooleanComparison"   = simple hasRedundantBooleanComparison
detectionFor "HasRedundantGuards"              = simple hasRedundantGuards
detectionFor "HasRedundantIf"                  = simple hasRedundantIf
detectionFor "HasRedundantLambda"              = simple hasRedundantLambda
detectionFor "HasRedundantLocalVariableReturn" = simple hasRedundantLocalVariableReturn
detectionFor "HasRedundantParameter"           = simple hasRedundantParameter
detectionFor "HasRedundantReduction"           = simple hasRedundantReduction
detectionFor "HasRedundantRepeat"              = simple hasRedundantRepeat
detectionFor "HasTooManyMethods"               = simple hasTooManyMethods
detectionFor "HasTooShortBindings"             = withLanguage hasTooShortIdentifiers
detectionFor "HasTooShortIdentifiers"          = withLanguage hasTooShortIdentifiers
detectionFor "HasUnreachableCode"              = simple hasUnreachableCode
detectionFor "HasWrongCaseBinding"             = withLanguage hasWrongCaseIdentifiers
detectionFor "HasWrongCaseIdentifiers"         = withLanguage hasWrongCaseIdentifiers
detectionFor "IsLongCode"                      = unsupported
detectionFor "OverridesEqualOrHashButNotBoth"  = simple overridesEqualOrHashButNotBoth
detectionFor "ShouldInvertIfCondition"         = simple shouldInvertIfCondition
detectionFor "ReturnsNil"                      = simple returnsNil
detectionFor "ReturnsNull"                     = simple returnsNil
detectionFor "ShouldUseOtherwise"              = simple shouldUseOtherwise
detectionFor "UsesCut"                         = simple usesCut
detectionFor "UsesFail"                        = simple usesFail
detectionFor "UsesUnificationOperator"         = simple usesUnificationOperator
detectionFor "HasDeclarationTypos"             = failedDeclarations detectDeclarationTypos
detectionFor _                                 = unsupported

type Detection = SmellsContext -> Expression -> [Identifier]

unsupported :: Detection
unsupported _ _ = []

simple :: Inspection -> Detection
simple inspection _ = locate' inspection

withLanguage :: (DomainLanguage -> Inspection) -> Detection
withLanguage inspection context = locate' (inspection (snd context))

failedDeclarations :: (Identifier -> Expression -> [Identifier]) -> Detection
failedDeclarations detection = undefined -- map (detection . missingDeclarations . fst
-- failedDeclarations inspection context = filter (\x -> y) . fst $ context

missingDeclarations :: [QueryResult] -> [Identifier]
missingDeclarations = mapMaybe missingDeclaration
  where
    missingDeclaration (inspection -> Just ("Declares", name), (ExpectationResult _ False)) = Just name
    missingDeclaration _                                                                    = Nothing

    inspection (cQuery -> Just (Inspection name (Named arg) Unmatching)) = Just (name, arg)
    inspection _                                                         = Nothing

    cQuery (Decontextualize q) = Just q
    cQuery (Within _ q)        = Just q
    cQuery (Through _ q)       = Just q
    cQuery _                   = Nothing

exectationFor :: Smell -> Identifier -> Expectation
exectationFor smell identifier = Expectation identifier smell

locate' inspection = identifiers . locate inspection
  where identifiers Nowhere      = []
        identifiers TopLevel     = ["*"]
        identifiers (Nested ids) = ids

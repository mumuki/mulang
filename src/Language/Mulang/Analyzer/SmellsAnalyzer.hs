{-# LANGUAGE ViewPatterns, TupleSections #-}

module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang.Ast (Expression, Identifier)
import Language.Mulang.DomainLanguage
import Language.Mulang.Inspector.Primitive
import Language.Mulang.Inspector.Logic
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.Inspector.Combiner (Location(..), locate)
import Language.Mulang.Edl.Expectation (cQuery, Query(..), CQuery (..), Matcher(..), Predicate(..))

import Language.Mulang.Analyzer.Analysis hiding (DomainLanguage, Inspection, allSmells)

import Data.List ((\\), intersect, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)

-- the runtime context of a smell analysis,
-- which is composed of the previous expectations evaluation results and the domain language
-- this context will be internally passed to core functions of this module
type SmellsContext = ([QueryResult], DomainLanguage)

-- An SmellInstance is an Smell type plus an optional target
-- Most Smells have not target, but a few context-dependent smells
-- may be instantiated multiple times with different targets
-- before being evaluated
type SmellInstance = (Smell, Maybe Identifier)

-- A Detection is an executable representation of an SmellInstance
type Detection = SmellsContext -> Expression -> [Identifier]

-- Smell detection has three phases:
--
-- 1. Smells selection: dependening on the given SmellSet, differents lists of smells may be generated
-- 2. Smells instantiation: for each selected smell, one ore more concrete smells will be instantiated
-- 3. Smells evaluation: for each smell instance, it will be evaluated and zero or more Expectations will be synthesized
analyseSmells :: Expression -> SmellsContext -> Maybe SmellsSet -> [Expectation]
analyseSmells ast context = concatMap (evalSmellInstance context ast) . concatMap (instantiateSmell context) . smellsFor

---
--- Selection
---

smellsFor :: Maybe SmellsSet -> [Smell]
smellsFor (Just (NoSmells included))    = intersect allSmells (fromMaybe [] included)
smellsFor (Just (AllSmells excluded))   = allSmells \\ (fromMaybe [] excluded)
smellsFor _                             = []

allSmells :: [Smell]
allSmells = [
  "DiscardsExceptions",
  "DoesConsolePrint",
  "DoesNilTest",
  "DoesTypeTest",
  "HasAssignmentCondition",
  "HasAssignmentReturn",
  "HasCodeDuplication",
  "HasDeclarationTypos",
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
  "HasUsageTypos",
  "HasWrongCaseIdentifiers",
  "IsLongCode",
  "OverridesEqualOrHashButNotBoth",
  "ReturnsNil",
  "ShouldInvertIfCondition",
  "ShouldUseOtherwise",
  "UsesCut",
  "UsesFail",
  "UsesUnificationOperator" ]

---
--- Instantiation
---

instantiateSmell :: SmellsContext -> Smell -> [SmellInstance]
instantiateSmell context smell = map (smell,) . targetsFor $ smell
  where
    targetsFor :: Smell -> [Maybe Identifier]
    targetsFor "HasDeclarationTypos" = justTargets missingDeclaration
    targetsFor "HasUsageTypos"       = justTargets missingUsage
    targetsFor _                     = [Nothing]

    justTargets f = map Just . mapMaybe f . fst $ context

    missingDeclaration :: QueryResult -> Maybe Identifier
    missingDeclaration (failedQuery -> Just (name, target)) | isDeclaration name = Just target
    missingDeclaration _                                    = Nothing

    missingUsage :: QueryResult -> Maybe Identifier
    missingUsage (failedQuery -> Just ("Uses", target)) = Just target
    missingUsage _                                      = Nothing

    isDeclaration "Delegates"                  = True
    isDeclaration "SubordinatesDeclarationsTo" = True
    isDeclaration name                         = isPrefixOf "Declares" name

failedQuery :: QueryResult -> Maybe (String, Identifier)
failedQuery (plainInspection -> Just (name, target), (ExpectationResult _ False)) = Just (name, target)
failedQuery _                                                                     = Nothing

plainInspection :: Query -> Maybe (String, Identifier)
plainInspection (cQuery -> Just (Inspection name (Named arg) Unmatching)) = Just (name, arg)
plainInspection _                                                         = Nothing

---
--- Evaluation
---

evalSmellInstance :: SmellsContext -> Expression -> SmellInstance -> [Expectation]
evalSmellInstance context expression smellInstance =  map (expectationFor smellInstance) . detectionFor smellInstance context $ expression

detectionFor :: SmellInstance -> Detection
detectionFor ("DiscardsExceptions", Nothing)              = simple discardsExceptions
detectionFor ("DoesConsolePrint", Nothing)                = simple doesConsolePrint
detectionFor ("DoesNilTest", Nothing)                     = simple doesNilTest
detectionFor ("DoesNullTest", Nothing)                    = simple doesNilTest
detectionFor ("DoesTypeTest", Nothing)                    = simple doesTypeTest
detectionFor ("HasAssignmentCondition", Nothing)          = simple hasAssignmentCondition
detectionFor ("HasAssignmentReturn", Nothing)             = simple hasAssignmentReturn
detectionFor ("HasCodeDuplication", Nothing)              = unsupported
detectionFor ("HasDeclarationTypos", Just target)         = raw (detectDeclarationTypos target)
detectionFor ("HasEmptyIfBranches", Nothing)              = simple hasEmptyIfBranches
detectionFor ("HasEmptyRepeat", Nothing)                  = simple hasEmptyRepeat
detectionFor ("HasLongParameterList", Nothing)            = simple hasLongParameterList
detectionFor ("HasMisspelledBindings", Nothing)           = withLanguage hasMisspelledIdentifiers
detectionFor ("HasMisspelledIdentifiers", Nothing)        = withLanguage hasMisspelledIdentifiers
detectionFor ("HasRedundantBooleanComparison", Nothing)   = simple hasRedundantBooleanComparison
detectionFor ("HasRedundantGuards", Nothing)              = simple hasRedundantGuards
detectionFor ("HasRedundantIf", Nothing)                  = simple hasRedundantIf
detectionFor ("HasRedundantLambda", Nothing)              = simple hasRedundantLambda
detectionFor ("HasRedundantLocalVariableReturn", Nothing) = simple hasRedundantLocalVariableReturn
detectionFor ("HasRedundantParameter", Nothing)           = simple hasRedundantParameter
detectionFor ("HasRedundantReduction", Nothing)           = simple hasRedundantReduction
detectionFor ("HasRedundantRepeat", Nothing)              = simple hasRedundantRepeat
detectionFor ("HasTooManyMethods", Nothing)               = simple hasTooManyMethods
detectionFor ("HasTooShortBindings", Nothing)             = withLanguage hasTooShortIdentifiers
detectionFor ("HasTooShortIdentifiers", Nothing)          = withLanguage hasTooShortIdentifiers
detectionFor ("HasUnreachableCode", Nothing)              = simple hasUnreachableCode
detectionFor ("HasUsageTypos", Just target)               = raw (detectUsageTypos target)
detectionFor ("HasWrongCaseBinding", Nothing)             = withLanguage hasWrongCaseIdentifiers
detectionFor ("HasWrongCaseIdentifiers", Nothing)         = withLanguage hasWrongCaseIdentifiers
detectionFor ("IsLongCode", Nothing)                      = unsupported
detectionFor ("OverridesEqualOrHashButNotBoth", Nothing)  = simple overridesEqualOrHashButNotBoth
detectionFor ("ReturnsNil", Nothing)                      = simple returnsNil
detectionFor ("ReturnsNull", Nothing)                     = simple returnsNil
detectionFor ("ShouldInvertIfCondition", Nothing)         = simple shouldInvertIfCondition
detectionFor ("ShouldUseOtherwise", Nothing)              = simple shouldUseOtherwise
detectionFor ("UsesCut", Nothing)                         = simple usesCut
detectionFor ("UsesFail", Nothing)                        = simple usesFail
detectionFor ("UsesUnificationOperator", Nothing)         = simple usesUnificationOperator
detectionFor _                                      = unsupported

unsupported :: Detection
unsupported _ _ = []

simple :: Inspection -> Detection
simple inspection _ = locate' inspection

withLanguage :: (DomainLanguage -> Inspection) -> Detection
withLanguage inspection context = locate' (inspection (snd context))

raw :: (Expression -> [Identifier]) -> Detection
raw detection _ = detection

expectationFor :: SmellInstance -> Identifier -> Expectation
expectationFor (smell, Just target) identifier  = Expectation identifier (smell ++ ":" ++ target)
expectationFor (smell, _          ) identifier  = Expectation identifier smell

locate' inspection = identifiers . locate inspection
  where identifiers Nowhere      = []
        identifiers TopLevel     = ["*"]
        identifiers (Nested ids) = ids

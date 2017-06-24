module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.DomainLanguage
import Language.Mulang.Analyzer.Analysis hiding (DomainLanguage)
import Data.List

analyseSmells :: Expression -> DomainLanguage -> SmellsSet -> [Expectation]
analyseSmells ast language set = concatMap (\smell -> detectSmell smell language ast) (smellsFor set)

detectSmell :: Smell -> DomainLanguage -> Expression -> [Expectation]
detectSmell smell language =  map (exectationFor smell) . detectionFor smell language

smellsFor :: SmellsSet -> [Smell]
smellsFor (NoSmells)             = []
smellsFor (OnlySmells included)  = included
smellsFor (AllSmells excluded)   = allSmells \\ excluded

allSmells :: [Smell]
allSmells = enumFrom minBound

detectionFor :: Smell -> Detection
detectionFor HasRedundantIf                  = simple hasRedundantIf
detectionFor HasRedundantLambda              = simple hasRedundantLambda
detectionFor HasRedundantBooleanComparison   = simple hasRedundantBooleanComparison
detectionFor HasRedundantGuards              = simple hasRedundantGuards
detectionFor HasRedundantLocalVariableReturn = simple hasRedundantLocalVariableReturn
detectionFor HasAssignmentReturn             = simple hasAssignmentReturn
detectionFor HasRedundantParameter           = simple hasRedundantParameter
detectionFor DoesNullTest                    = simple doesNullTest
detectionFor DoesTypeTest                    = simple doesTypeTest
detectionFor ReturnsNull                     = simple returnsNull
detectionFor HasTooShortBindings             = withLanguage hasTooShortBindings
detectionFor HasWrongCaseBindings            = withLanguage hasWrongCaseBindings
detectionFor HasMisspelledBindings           = withLanguage hasMisspelledBindings
detectionFor IsLongCode                      = unsupported
detectionFor HasCodeDuplication              = unsupported


type Detection = DomainLanguage -> Expression -> [Binding]
unsupported :: Detection
unsupported _ _ = []

simple :: Inspection -> Detection
simple inspection _ = detectAll inspection

withLanguage :: (DomainLanguage -> Inspection) -> Detection
withLanguage inspection language = detect mainExpressions (inspection language)

exectationFor :: Smell -> Binding -> Expectation
exectationFor smell binding = Basic binding (show smell)

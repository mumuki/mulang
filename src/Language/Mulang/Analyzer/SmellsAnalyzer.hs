module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.Analyzer.Analysis
import Data.List

analyseSmells :: Expression -> SmellsSet -> [Expectation]
analyseSmells code set = concatMap (`detectSmell` code) (smellsFor set)

detectSmell :: Smell -> Expression -> [Expectation]
detectSmell smell =  map (exectationFor smell) . detectAll (inspectionFor smell)

smellsFor :: SmellsSet -> [Smell]
smellsFor (NoSmells)             = []
smellsFor (OnlySmells included)  = included
smellsFor (AllSmells excluded)   = allSmells \\ excluded

allSmells :: [Smell]
allSmells = enumFrom minBound

inspectionFor :: Smell -> Inspection
inspectionFor HasRedundantIf                  = hasRedundantIf
inspectionFor HasRedundantLambda              = hasRedundantLambda
inspectionFor HasRedundantBooleanComparison   = hasRedundantBooleanComparison
inspectionFor HasRedundantGuards              = hasRedundantGuards
inspectionFor HasRedundantLocalVariableReturn = hasRedundantLocalVariableReturn
inspectionFor HasAssignmentReturn             = hasAssignmentReturn
inspectionFor HasRedundantParameter           = hasRedundantParameter
inspectionFor DoesNullTest                    = doesNullTest
inspectionFor DoesTypeTest                    = doesTypeTest
inspectionFor ReturnsNull                     = returnsNull
inspectionFor IsLongCode                      = const False
inspectionFor HasCodeDuplication              = const False

exectationFor :: Smell -> Binding -> Expectation
exectationFor smell binding = Basic binding (show smell)

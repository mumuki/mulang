module Language.Mulang.Analyzer.SmellsAnalyzer (
  analyseSmells) where

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell
import Language.Mulang.Analyzer.Analysis

type NamedSmell = (String, Inspection)

analyseSmells :: Expression -> [Expectation]
analyseSmells code = concatMap (`runSingleSmellDetection` code) namedSmells

runSingleSmellDetection :: NamedSmell -> Expression -> [Expectation]
runSingleSmellDetection (name, inspection) =
  map (smellyBindingToResult name) . detect inspection

namedSmells :: [NamedSmell]
namedSmells = [
  ("HasRedundantIf", hasRedundantIf),
  ("HasRedundantLambda", hasRedundantLambda),
  ("HasRedundantBooleanComparison", hasRedundantBooleanComparison),
  ("HasRedundantGuards", hasRedundantGuards),
  ("HasRedundantLocalVariableReturn", hasRedundantLocalVariableReturn),
  ("hasAssignmentReturn", hasAssignmentReturn),
  ("DoesNullTest", doesNullTest),
  ("DoesTypeTest", doesTypeTest),
  ("ReturnsNull", returnsNull)]

smellyBindingToResult smellName binding = Basic binding smellName
module Language.Mulang.Analyzer.ExpectationsAnalyzer (
  analyseExpectations) where


import Language.Mulang
import Language.Mulang.Analyzer.Analysis (Expectation, ExpectationResult(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)

analyseExpectations :: Expression -> [Expectation]-> [ExpectationResult]
analyseExpectations content = map (analyseExpectation content)

analyseExpectation :: Expression -> Expectation -> ExpectationResult
analyseExpectation ast e = ExpectationResult e (compileAndEval e)
  where compileAndEval e = (compileExpectation e) ast


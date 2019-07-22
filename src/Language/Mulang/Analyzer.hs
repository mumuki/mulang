module Language.Mulang.Analyzer (
  analyse,
  module Language.Mulang.Analyzer.Analysis) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis hiding (Inspection)
import Language.Mulang.Analyzer.DomainLanguageCompiler (compileDomainLanguage)
import Language.Mulang.Analyzer.ExpectationsAnalyzer (analyseExpectations)
import Language.Mulang.Analyzer.CustomExpectationsAnalyzer (analyseCustomExpectations)
import Language.Mulang.Analyzer.FragmentParser (parseFragment)
import Language.Mulang.Analyzer.SignaturesAnalyzer  (analyseSignatures)
import Language.Mulang.Analyzer.SmellsAnalyzer (analyseSmells)
import Language.Mulang.Analyzer.TestsAnalyzer  (analyseTests)
import Language.Mulang.Analyzer.Autocorrector  (autocorrect)
import Data.Maybe (fromMaybe)


--
-- Analysis running
--

analyse, analyse' :: Analysis -> IO AnalysisResult
analyse = analyse' . autocorrect

analyse' (Analysis sample spec) = analyseSample . parseFragment $ sample
  where analyseSample (Right ast)    = analyseAst ast spec
        analyseSample (Left message) = return $ AnalysisFailed message

analyseAst :: Expression -> AnalysisSpec -> IO AnalysisResult
analyseAst ast spec = do
  domaingLang <- compileDomainLanguage (domainLanguage spec)
  testResults <- analyseTests ast (testAnalysisType spec)
  return $ AnalysisCompleted (analyseExpectations ast (expectations spec) ++ analyseCustomExpectations ast (customExpectations spec))
                             (analyseSmells ast domaingLang (smellsSet spec))
                             (analyseSignatures ast (signatureAnalysisType spec))
                             testResults
                             (analyzeIntermediateLanguage ast spec)

analyzeIntermediateLanguage :: Expression -> AnalysisSpec -> Maybe Expression
analyzeIntermediateLanguage ast spec
  | fromMaybe False (includeIntermediateLanguage spec) = Just ast
  | otherwise = Nothing

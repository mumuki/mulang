module Language.Mulang.Analyzer (
  analyse,
  genericAnalyse,
  genericAnalyseMany,
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
import Language.Mulang.Analyzer.Transformer  (transformMany')
import Data.Maybe (fromMaybe)

--
-- Analysis running
--

genericAnalyseMany :: (Expression -> a) -> [Analysis] -> IO [GenericAnalysisResult a]
genericAnalyseMany f = mapM (genericAnalyse f)

genericAnalyse :: (Expression -> a) -> Analysis -> IO (GenericAnalysisResult a)
genericAnalyse f analysis = fmap serialize (analyse analysis)
  where
    serialize r@(AnalysisCompleted { transformedAsts = asts, outputAst = out }) = r { transformedAsts = fmap (map f) asts, outputAst = fmap f out }
    serialize (AnalysisFailed m) = (AnalysisFailed m)

analyse, analyse' :: Analysis -> IO AnalysisResult
analyse = analyse' . autocorrect

analyse' (Analysis sample spec) = analyseSample . parseFragment (normalizationOptions spec) $ sample
  where analyseSample (Right ast)    = analyseAst ast spec
        analyseSample (Left message) = return $ AnalysisFailed message

analyseAst :: Expression -> AnalysisSpec -> IO AnalysisResult
analyseAst ast spec = do
  domainLang <- compileDomainLanguage (domainLanguage spec)
  testResults <- analyseTests ast (testAnalysisType spec) (normalizationOptions spec)

  let queryResults = (analyseExpectations ast (expectations spec) ++ analyseCustomExpectations ast (customExpectations spec))
  let expectationResults = map snd queryResults
  let context = (queryResults, domainLang)

  return $ AnalysisCompleted expectationResults
                             (analyseSmells ast context (smellsSet spec))
                             (analyseSignatures ast (signatureAnalysisType spec))
                             testResults
                             (analyzeOutputAst ast spec)
                             (analyzeOutputIdentifiers ast spec)
                             (transformMany' ast (transformationSpecs spec))

analyzeOutputAst :: Expression -> AnalysisSpec -> Maybe Expression
analyzeOutputAst ast spec
  | fromMaybe False (includeOutputAst spec) = Just ast
  | otherwise = Nothing

analyzeOutputIdentifiers :: Expression -> AnalysisSpec -> Maybe ([String], [String])
analyzeOutputIdentifiers ast spec
  | fromMaybe False (includeOutputIdentifiers spec) = Just (declaredIdentifiers ast, referencedIdentifiers ast)
  | otherwise = Nothing

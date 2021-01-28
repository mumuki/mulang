module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseCustomExpectations) where

import Data.Maybe (fromMaybe)

import           Data.Map (Map)
import qualified Data.Map as Map

import Language.Mulang.Ast
import Language.Mulang.Analyzer.Analysis (customExpectationResult, QueryResult, AutocorrectionRules)
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery')

import Language.Mulang.Analyzer.ExpectationsCompiler (compileBaseQueryAndNegator)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseCustomExpectations :: Maybe AutocorrectionRules -> Expression -> Maybe String -> [QueryResult]
analyseCustomExpectations Nothing      ast = analyseCustomExpectations (Just Map.empty) ast
analyseCustomExpectations (Just rules) ast = map (runExpectation rules ast) . fromMaybe [] . fmap parseExpectations

runExpectation :: AutocorrectionRules -> Expression -> Expectation -> QueryResult
runExpectation rules ast (Expectation name q) = (q, customExpectationResult name (compileTopQuery' compiledRules q ast))
  where
    compiledRules = Map.mapKeys compile . Map.map compile $ rules
    compile s | (query, _) <- compileBaseQueryAndNegator s = query

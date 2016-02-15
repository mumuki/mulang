{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Interpreter (
            evaluate,
            Input(..),
            Output(..),
            Expectation(..),
            ExpectationResult(..)) where

import Language.Mulang.Cli.Code
import Language.Mulang.Cli.Compiler

import GHC.Generics
import Data.Aeson

import Language.Mulang
import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner
import Language.Mulang.Inspector.Smell

data Input = Input {
  code :: Code,
  expectations :: [Expectation]
} deriving (Show, Eq, Generic)


data Output = Output {
  results :: [ExpectationResult],
  smells :: [Expectation]
}  deriving (Show, Eq, Generic)

data ExpectationResult = ExpectationResult {
  expectation :: Expectation,
  result :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Input
instance FromJSON Output
instance FromJSON ExpectationResult

instance ToJSON Output
instance ToJSON Input
instance ToJSON ExpectationResult

evaluate :: Input -> Output
evaluate (Input code expectations)
      | Just ast <- parseCode code = Output (evaluateExpectations expectations ast) (detectSmells ast)
      | otherwise = Output [] []

evaluateExpectations :: [Expectation] ->  Expression -> [ExpectationResult]
evaluateExpectations es content = map run es
  where
    run e = ExpectationResult e (compileAndEval e)
    compileAndEval e = (compile e) content

type NamedSmell = (String, Inspection)

detectSmells :: Expression -> [Expectation]
detectSmells code = concatMap (`runSingleSmellDetection` code) namedSmells

runSingleSmellDetection :: NamedSmell -> Expression -> [Expectation]
runSingleSmellDetection (name, inspection) code =
  map (smellyBindingToResult name) $ detect inspection code

namedSmells :: [NamedSmell]
namedSmells = [
  ("HasRedundantIf", hasRedundantIf),
  ("HasRedundantLambda", hasRedundantLambda),
  ("HasRedundantBooleanComparison", hasRedundantBooleanComparison),
  ("HasRedundantGuards", hasRedundantGuards),
  ("HasRedundantParameter", hasRedundantParameter)]

smellyBindingToResult smellName binding = Expectation [binding] smellName Anyone True False



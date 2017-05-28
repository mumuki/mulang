{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Interpreter (
  newSample,
  expectationsSample,
  evaluate,
  Input(..),
  Output(..),
  Expectation(..),
  ExpectationResult(..)) where

import Control.Monad
import Language.Mulang.Signature
import Language.Mulang.Cli.Code
import Language.Mulang.Cli.Compiler

import GHC.Generics
import Data.Aeson

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell

data Input = Input {
  code :: Code,
  expectations :: [Expectation],
  analyseSignatures :: Bool
} deriving (Show, Eq, Generic)

data Output = Output {
  results :: ExpectationAnalysysResult,
  smells :: SmellAnalysysResult,
  signatures :: SignatureAnalysysResult
}  deriving (Show, Eq, Generic)

type ExpectationAnalysysResult = [ExpectationResult]
type SmellAnalysysResult = [Expectation]
type SignatureAnalysysResult = [String]

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

newSample :: Code -> Input
newSample code = Input code [] False

expectationsSample :: Code -> [Expectation] -> Input
expectationsSample code es = (newSample code) { expectations = es }

evaluate :: Input -> Output
evaluate (Input code expectations analyseSignatures)
      | Just ast <- parseCode code = evaluateAst ast expectations analyseSignatures
      | otherwise = Output [] [] []

evaluateAst ast expectations analyseSignatures =
    Output (evaluateExpectations expectations ast)
           (detectSmells ast)
           (generateSignatures analyseSignatures ast)

generateSignatures :: Bool -> Expression -> SignatureAnalysysResult
generateSignatures analyseSignatures e = onlyIf' analyseSignatures (codeSignaturesOf e)

evaluateExpectations :: [Expectation] ->  Expression -> [ExpectationResult]
evaluateExpectations es content = map run es
  where
    run e = ExpectationResult e (compileAndEval e)
    compileAndEval e = (compile e) content

type NamedSmell = (String, Inspection)

detectSmells :: Expression -> [Expectation]
detectSmells code = concatMap (`runSingleSmellDetection` code) namedSmells

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

onlyIf' :: MonadPlus m => Bool -> m a -> m a
onlyIf' True x = x
onlyIf' _    _ = mzero

onlyIf :: MonadPlus m => Bool -> a -> m a
onlyIf condition = onlyIf' condition . return
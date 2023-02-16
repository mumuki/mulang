module Language.Mulang.Normalizers.JavaScript (javaScriptNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (unnormalized, NormalizationOptions(..), SequenceSortMode(..))

javaScriptNormalizationOptions :: NormalizationOptions
javaScriptNormalizationOptions = unnormalized {
  convertObjectVariableIntoObject = True,
  convertLambdaVariableIntoFunction = True,
  convertObjectLevelFunctionIntoMethod = True,
  convertObjectLevelLambdaVariableIntoMethod = True,
  convertObjectLevelVariableIntoAttribute = True,
  sortSequenceDeclarations = SortUniqueNonVariables,
  convertPartialProcedureIntoFunction = True
}

module Language.Mulang.Normalizers.JavaScript (javaScriptNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))

javaScriptNormalizationOptions :: NormalizationOptions
javaScriptNormalizationOptions = defaultNormalizationOptions {
  convertObjectVariableIntoObject = True,
  convertLambdaVariableIntoFunction = True,
  convertObjectLevelFunctionIntoMethod = True,
  convertObjectLevelLambdaVariableIntoMethod = True,
  convertObjectLevelVariableIntoAttribute = True,
  sortSequenceDeclarations = SortUniqueNonVariables
}

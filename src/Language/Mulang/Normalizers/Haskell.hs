module Language.Mulang.Normalizers.Haskell (haskellNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))

haskellNormalizationOptions :: NormalizationOptions
haskellNormalizationOptions = defaultNormalizationOptions {
  convertLambdaVariableIntoFunction = True,
  sortSequenceDeclarations = SortAll
}

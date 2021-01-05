module Language.Mulang.Normalizers.Haskell (haskellNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (unnormalized, NormalizationOptions(..), SequenceSortMode(..))

haskellNormalizationOptions :: NormalizationOptions
haskellNormalizationOptions = unnormalized {
  convertLambdaVariableIntoFunction = True,
  sortSequenceDeclarations = SortAll
}

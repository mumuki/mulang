module Language.Mulang.Normalizers.C (cNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (unnormalized, NormalizationOptions(..), SequenceSortMode(..))

cNormalizationOptions :: NormalizationOptions
cNormalizationOptions = unnormalized {
  sortSequenceDeclarations = SortAllNonVariables
}

module Language.Mulang.Normalizers.C (cNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))

cNormalizationOptions :: NormalizationOptions
cNormalizationOptions = defaultNormalizationOptions {
  sortSequenceDeclarations = SortAllNonVariables
}

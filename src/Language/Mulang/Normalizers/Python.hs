module Language.Mulang.Normalizers.Python (pythonNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (unnormalized, NormalizationOptions(..), SequenceSortMode(..))

pythonNormalizationOptions :: NormalizationOptions
pythonNormalizationOptions = unnormalized {
  sortSequenceDeclarations = SortAllNonVariables
}

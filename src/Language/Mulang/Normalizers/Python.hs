module Language.Mulang.Normalizers.Python (pythonNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))

pythonNormalizationOptions :: NormalizationOptions
pythonNormalizationOptions = defaultNormalizationOptions {
  sortSequenceDeclarations = SortAllNonVariables
}

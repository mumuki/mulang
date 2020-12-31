module Language.Mulang.Normalizers.Java (javaNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))

javaNormalizationOptions :: NormalizationOptions
javaNormalizationOptions = defaultNormalizationOptions {
  sortSequenceDeclarations = SortAllNonVariables
}

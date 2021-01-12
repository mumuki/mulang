module Language.Mulang.Normalizers.Java (javaNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (unnormalized, NormalizationOptions(..), SequenceSortMode(..))

javaNormalizationOptions :: NormalizationOptions
javaNormalizationOptions = unnormalized {
  sortSequenceDeclarations = SortAllNonVariables
}

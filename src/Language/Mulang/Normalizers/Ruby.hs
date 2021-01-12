module Language.Mulang.Normalizers.Ruby (rubyNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (unnormalized, NormalizationOptions(..), SequenceSortMode(..))

rubyNormalizationOptions :: NormalizationOptions
rubyNormalizationOptions = unnormalized {
  sortSequenceDeclarations = SortAllNonVariables,
  insertImplicitReturn = True
}

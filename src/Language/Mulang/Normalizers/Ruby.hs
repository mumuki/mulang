module Language.Mulang.Normalizers.Ruby (rubyNormalizationOptions) where

import Language.Mulang.Transform.Normalizer (defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))

rubyNormalizationOptions :: NormalizationOptions
rubyNormalizationOptions = defaultNormalizationOptions {
  sortSequenceDeclarations = SortAllNonVariables,
  insertImplicitReturn = True
}

module Language.Mulang.Transform.Cropper (
    crop) where

import Language.Mulang.Ast
import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Transform.Normalizer (normalize, unnormalized, NormalizationOptions(..))
import Language.Mulang.Transform.Replacer (Replacer)

crop :: Replacer -> Inspection -> Expression -> Expression
crop replace i = normalize (unnormalized { trimSequences = True, compactSequences = True }) . replace i None

module Language.Mulang.Transform.Cropper (
    crop) where

import Language.Mulang.Ast
import Language.Mulang.Ast.Visitor
import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Transform.Normalizer (normalize, unnormalized, NormalizationOptions(..))
import Language.Mulang.Transform.Replacer (replace)

crop :: Inspection -> Expression -> Expression
crop i = normalize (unnormalized { trimSequences = True, compactSequences = True }) . replace i None
module Language.Mulang.Analyzer.Transformer (
  transformMany',
  transformMany,
  transform) where

import Language.Mulang.Analyzer.Analysis (Expectation(..), TransformationSpec, TransformationOperation(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Transform.Aliaser (alias)
import Language.Mulang.Transform.Cropper (crop)
import Language.Mulang.Transform.Normalizer (normalize)
import Language.Mulang.Transform.Renamer (rename)
import Language.Mulang.Transform.Replacer (replace)

transformMany' :: Expression -> Maybe [TransformationSpec] -> Maybe [Expression]
transformMany' e =  fmap (transformMany e)

transformMany :: Expression -> [TransformationSpec] -> [Expression]
transformMany e specs = map (transform e) specs

transform :: Expression -> TransformationSpec -> Expression
transform e ops = foldl f e ops
  where
    f e (Aliase m)          = alias m e
    f e (Crop i)            = crop (compileInspection i) e
    f e (Normalize options) = normalize options e
    f e (Replace i o)       = replace (compileInspection i) o e
    f e RenameVariables     = rename e

    compileInspection i = snd . compileExpectation $ (Expectation "*" i)
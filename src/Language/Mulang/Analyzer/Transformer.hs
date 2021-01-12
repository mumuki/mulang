module Language.Mulang.Analyzer.Transformer (
  transformMany',
  transformMany,
  transform) where

import Language.Mulang.Analyzer.Analysis (Expectation(..), TransformationSpec, TransformationOperation(..), TransformationScope(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Transform.Aliaser (alias)
import Language.Mulang.Transform.Cropper (crop)
import Language.Mulang.Transform.Normalizer (normalize)
import Language.Mulang.Transform.Renamer (rename)
import Language.Mulang.Transform.Replacer (replace, globalReplace, localReplace)

transformMany' :: Expression -> Maybe [TransformationSpec] -> Maybe [Expression]
transformMany' e =  fmap (transformMany e)

transformMany :: Expression -> [TransformationSpec] -> [Expression]
transformMany e specs = map (transform e) specs

transform :: Expression -> TransformationSpec -> Expression
transform e ops = foldl f e ops
  where
    f e (Alias m)              = alias m e
    f e (Crop i)               = crop replace (compileInspection i) e
    f e (CropAt s i)           = crop (compileReplaceScope s) (compileInspection i) e
    f e (Normalize options)    = normalize options e
    f e (Replace i o)          = replace (compileInspection i) o e
    f e (ReplaceAt s i o)      = (compileReplaceScope s) (compileInspection i) o e
    f e RenameVariables        = rename e

    compileInspection i = snd . compileExpectation $ (Expectation "*" i)

    compileReplaceScope LocalScope  = localReplace
    compileReplaceScope GlobalScope = globalReplace
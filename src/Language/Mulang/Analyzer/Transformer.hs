module Language.Mulang.Analyzer.Transformer (
  transformMany',
  transformMany,
  transform) where

import Language.Mulang.Ast (Expression)
import Language.Mulang.Transform.Renamer (rename)
import Language.Mulang.Transform.Normalizer (normalize)
import Language.Mulang.Analyzer.Analysis (Expectation, TransformationSpec, TransformationOperation(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)

transformMany' :: Expression -> Maybe [TransformationSpec] -> Maybe [Expression]
transformMany' e =  fmap (transformMany e)

transformMany :: Expression -> [TransformationSpec] -> [Expression]
transformMany e specs = map (transform e) specs

transform :: Expression -> TransformationSpec -> Expression
transform e ops = foldl f e ops
  where
    f e RenameVariables = rename e
    f e (Normalize options) = normalize options e
    f e (Crop expectation)  = seq (compileExpectation expectation) e -- TODO
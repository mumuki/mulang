module Language.Mulang.Analyzer.ExpectationsEvaluator (
  lenientMode,
  strictMode,
  ExpectationsEvaluator) where

import Language.Mulang.Analyzer.Finding (Finding, mock)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Inspector.Primitive (Inspection, lenient)


type ExpectationsEvaluator = Expression -> Finding Inspection -> Finding Bool

lenientMode, strictMode :: ExpectationsEvaluator
lenientMode ast = strictMode ast . mock lenient
strictMode  ast = fmap ($ ast)

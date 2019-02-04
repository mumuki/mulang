module Language.Mulang.Inspector.Multi (
  anyExpressions,
  allMatch,
  anyMatch,
  MultiInspection) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Primitive (Inspection)

type MultiInspection = [Expression] -> Bool

anyExpressions :: MultiInspection
anyExpressions = const True

allMatch :: [Inspection] -> MultiInspection
allMatch inspections expressions = and (zipWith ($) inspections expressions)

anyMatch :: [Inspection] -> MultiInspection
anyMatch inspections expressions = and (zipWith ($) inspections expressions)

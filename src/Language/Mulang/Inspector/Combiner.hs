module Language.Mulang.Inspector.Combiner (
  detect,
  detectAll,
  negative,
  alternative,
  scoped,
  scopedList,
  transitive,
  transitiveList) where

import Language.Mulang.Ast
import Language.Mulang.Unfold (Unfold, allExpressions)
import Language.Mulang.Inspector.Generic
import Language.Mulang.Explorer

detectAll :: Inspection -> Expression -> [Identifier]
detectAll = detect allExpressions

detect :: Unfold -> Inspection -> Expression -> [Identifier]
detect unfold i expression = filter (`inspection` expression) $ declaredBindingsOf unfold expression
                              where inspection = scoped i


alternative :: Inspection -> Inspection -> Inspection
alternative i1 i2 expression = i1 expression || i2 expression

negative :: Inspection -> Inspection
negative f = not . f

scoped :: Inspection -> Identifier -> Inspection
scoped inspection scope =  any inspection . bindedDeclarationsOf scope

scopedList :: Inspection -> [Identifier] -> Inspection
scopedList i =  foldl scoped i . reverse

transitive :: Inspection -> Identifier -> Inspection
transitive inspection identifier code = any (`scopedInspection` code) . transitiveReferencedBindingsOf identifier $ code
  where scopedInspection = scoped inspection

transitiveList :: Inspection -> [Identifier] -> Inspection
transitiveList i identifiers = transitive (scopedList i (init identifiers)) (last identifiers)

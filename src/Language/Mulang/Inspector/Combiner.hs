module Language.Mulang.Inspector.Combiner (
  detect,
  negative,
  alternative,
  scoped,
  scopedList,
  transitive,
  transitiveList,
  Modifier) where

import Language.Mulang.Ast
import Language.Mulang.Generator (transitiveReferencedIdentifiers, declarationsOf, declaredIdentifiers)
import Language.Mulang.Inspector.Primitive

type Modifier = Inspection -> Inspection

detect :: Inspection -> Expression -> [Identifier]
detect i expression =
  filter (`inspection` expression) $ declaredIdentifiers expression
    where inspection = scoped' i

alternative :: Inspection -> Modifier
alternative i1 i2 expression = i1 expression || i2 expression

negative :: Modifier
negative f = not . f

scoped :: Identifier -> Modifier
scoped scope inspection =  any inspection . declarationsOf scope

scopedList :: [Identifier] -> Modifier
scopedList scopes i =  foldl scoped' i . reverse $ scopes

transitive :: Identifier -> Modifier
transitive identifier inspection code = any (`scopedInspection` code) . transitiveReferencedIdentifiers identifier $ code
  where scopedInspection = scoped' inspection

transitiveList :: [Identifier] -> Modifier
transitiveList identifiers i = transitive (last identifiers) (scopedList (init identifiers) i)

scoped' = flip scoped

module Language.Mulang.Inspector.Combiner (
  detect,
  locate,
  negative,
  alternative,
  combined,
  scoped,
  scopedList,
  transitive,
  transitiveList,
  Location (..),
  Modifier) where

import Language.Mulang.Ast
import Language.Mulang.Generator (transitiveReferencedIdentifiers, declarationsOf, declaredIdentifiers)
import Language.Mulang.Inspector.Primitive

type Modifier = Inspection -> Inspection

data Location
  = Nowhere                   -- ^ No subexpression match, nor the whole expression
  | TopLevel                  -- ^ No subexpression match, but the whole expression
  | Nested [Identifier]       -- ^ One ore more subexpressions match
  deriving (Show, Eq)

-- | Answers those identifiers that are bound to expressions that match the given inspection
detect :: Inspection -> Expression -> [Identifier]
detect i expression =
  filter (`inspection` expression) $ declaredIdentifiers expression
    where inspection = scoped' i

-- | Answers the Locations of those expressions that match the given inspection
locate :: Inspection -> Expression -> Location
locate inspection expression
  | identifiers@(_:_) <- detect inspection expression = Nested identifiers
  | inspection expression = TopLevel
  | otherwise = Nowhere

alternative :: Inspection -> Modifier
alternative i1 i2 expression = i1 expression || i2 expression

combined :: Inspection -> Modifier
combined i1 i2 expression = i1 expression && i2 expression

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

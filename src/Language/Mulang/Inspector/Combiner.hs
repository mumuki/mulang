module Language.Mulang.Inspector.Combiner (
  detect,
  locate,
  scoped,
  scopedList,
  transitive,
  transitiveList,
  Location (..)) where

import Language.Mulang.Ast
import Language.Mulang.Generator (transitiveReferencedIdentifiers, declarationsOf, declaredIdentifiers)
import Language.Mulang.Inspector.Primitive

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

scoped :: Identifier -> Inspection -> Inspection
scoped scope inspection =  any inspection . declarationsOf scope

scopedList :: [Identifier] -> Inspection -> Inspection
scopedList scopes i =  foldl scoped' i . reverse $ scopes

transitive :: Identifier -> Inspection -> Inspection
transitive identifier inspection code = any (`scopedInspection` code) . transitiveReferencedIdentifiers identifier $ code
  where scopedInspection = scoped' inspection

transitiveList :: [Identifier] -> Inspection -> Inspection
transitiveList [identifier] i = transitive identifier i
transitiveList identifiers i  = scopedList identifiers i

scoped' = flip scoped

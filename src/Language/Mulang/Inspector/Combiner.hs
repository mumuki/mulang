module Language.Mulang.Inspector.Combiner (
  detect,
  locate,
  scoped,
  scopedList,
  transitive,
  transitiveList,
  deriveUses,
  InspectionFamily,
  Location (..)) where

import Language.Mulang.Ast
import Language.Mulang.Generator (transitiveReferencedIdentifiers, declarationsOf, declaredIdentifiers)
import Language.Mulang.Inspector.Primitive
import Language.Mulang.Inspector.Matcher (unmatching, Matcher)
import Language.Mulang.Inspector.Bound (countBoundDeclarations, uncounting, BoundCounter, BoundInspection)

type InspectionFamily = (Inspection, Matcher -> Inspection, Matcher -> Counter)
type BoundInspectionFamily = (BoundInspection, Matcher -> BoundInspection, Matcher -> BoundCounter)

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


-- | Derives a family of inspections from a primal inspection,
-- which consist of a simple inspection, a matching inspection and a counter
deriveUses :: (Matcher -> Inspection) -> InspectionFamily
deriveUses f = (uses, usesMatching, countUses)
  where
    uses :: Inspection
    uses = unmatching usesMatching

    usesMatching :: Matcher -> Inspection
    usesMatching matcher = positive (countUses matcher)

    countUses :: Matcher -> Counter
    countUses matcher = countExpressions (f matcher)

deriveDeclares :: (Matcher -> Inspection) -> BoundInspectionFamily
deriveDeclares f = (declares, declaresMatching, countDeclares)
  where
    declares :: BoundInspection
    declares = unmatching declaresMatching

    declaresMatching :: Matcher -> BoundInspection
    declaresMatching = uncounting countDeclares

    countDeclares :: Matcher -> BoundCounter
    countDeclares matcher = countBoundDeclarations (f matcher)

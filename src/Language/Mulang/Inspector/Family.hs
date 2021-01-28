{-# LANGUAGE LiberalTypeSynonyms #-}

module Language.Mulang.Inspector.Family (
  deriveSpecial,
  deriveUses,
  deriveDeclares,
  InspectionFamily,
  BoundInspectionFamily) where

import Language.Mulang.Consult (Consult)
import Language.Mulang.Counter (Count, Counter)
import Language.Mulang.Inspector.Matcher (unmatching, Matcher)
import Language.Mulang.Inspector.Primitive (countExpressions, positive, Inspection)
import Language.Mulang.Inspector.Bound (countBoundDeclarations, uncounting, BoundConsult, BoundCounter, BoundInspection)

type Family a = ((a Bool), Matcher -> (a Bool), Matcher -> (a Count))
type InspectionFamily = Family Consult
type BoundInspectionFamily = Family BoundConsult

-- | Derives a non-standard family of inspections from a primal inspection that takes a generic argument,
-- which consist of just an inspection and a counter
deriveSpecial :: (a -> Inspection) -> (a -> Inspection, a -> Counter)
deriveSpecial f = (positive . f', f')
  where f' = countExpressions . f

-- | Derives a family of usage inspections from a primal inspection,
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

-- | Derives a family of declaration inspections from a primal inspection,
-- which consist of a simple inspection, a matching inspection and a counter
deriveDeclares :: (Matcher -> Inspection) -> BoundInspectionFamily
deriveDeclares f = (declares, declaresMatching, countDeclares)
  where
    declares :: BoundInspection
    declares = unmatching declaresMatching

    declaresMatching :: Matcher -> BoundInspection
    declaresMatching = uncounting countDeclares

    countDeclares :: Matcher -> BoundCounter
    countDeclares matcher = countBoundDeclarations (f matcher)

module Language.Mulang.Inspector.Procedural (
  countProcedures,
  usesRepeat,
  usesWhile,
  usesSwitch,
  usesForEach,
  usesForLoop,
  usesLoop,
  declaresProcedure,
  declaresProcedureMatching) where

import Language.Mulang.Ast
import Language.Mulang.Generator (equationsExpressions)
import Language.Mulang.Inspector.Matcher (Matcher, unmatching, matches)
import Language.Mulang.Inspector.Primitive (Inspection, positive, containsExpression)
import Language.Mulang.Inspector.Bound (BoundCounter, BoundInspection, bound, containsBoundDeclaration, countBoundDeclarations, uncounting)
import Language.Mulang.Inspector.Generic (usesYield)

declaresProcedure :: BoundInspection
declaresProcedure = unmatching declaresProcedureMatching

declaresProcedureMatching :: Matcher -> BoundInspection
declaresProcedureMatching = uncounting countProcedures

countProcedures :: Matcher -> BoundCounter
countProcedures matcher = countBoundDeclarations f
  where f (Procedure _ equations) = matches matcher equationsExpressions $ equations
        f _                       = False

-- | Inspection that tells whether an expression uses while
-- in its definition
usesWhile :: Inspection
usesWhile = containsExpression f
  where f (While _ _) = True
        f _ = False
-- | Inspection that tells whether an expression uses Switch
-- in its definition
usesSwitch :: Inspection
usesSwitch = containsExpression f
  where f (Switch _ _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses reoeat
-- in its definition
usesRepeat :: Inspection
usesRepeat = containsExpression f
  where f (Repeat _ _) = True
        f _ = False

usesForEach :: Inspection
usesForEach = containsExpression f
  where f (For _ e) = not $ usesYield e
        f _         = False

usesForLoop :: Inspection
usesForLoop = containsExpression f
  where f (ForLoop _ _ _ _) = True
        f _                 = False

usesLoop :: Inspection
usesLoop e = usesRepeat e || usesWhile e || usesForLoop e || usesForEach e

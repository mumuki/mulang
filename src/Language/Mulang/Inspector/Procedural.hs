module Language.Mulang.Inspector.Procedural (
  countProcedures,
  countWhiles,
  countForLoops,
  usesRepeat,
  countRepeats,
  usesRepeatMatching,
  usesWhile,
  usesWhileMatching,
  usesSwitch,
  usesForEach,
  usesForEachMatching,
  usesForLoop,
  usesForLoopMatching,
  usesLoop,
  declaresProcedure,
  declaresProcedureMatching,
  usesIterationMatching) where

import Language.Mulang.Ast
import Language.Mulang.Generator (equationsExpandedExpressions, statementsExpressions)
import Language.Mulang.Inspector.Matcher (Matcher, unmatching, matches)
import Language.Mulang.Inspector.Primitive (Inspection, Counter, containsExpression, positive, countExpressions)
import Language.Mulang.Inspector.Bound (BoundCounter, BoundInspection, countBoundDeclarations, uncounting)
import Language.Mulang.Inspector.Generic (usesYield)

declaresProcedure :: BoundInspection
declaresProcedure = unmatching declaresProcedureMatching

declaresProcedureMatching :: Matcher -> BoundInspection
declaresProcedureMatching = uncounting countProcedures

countProcedures :: Matcher -> BoundCounter
countProcedures matcher = countBoundDeclarations f
  where f (Procedure _ equations) = matches matcher equationsExpandedExpressions $ equations
        f _                       = False

-- | Inspection that tells whether an expression uses while
-- in its definition
usesWhile :: Inspection
usesWhile = unmatching usesWhileMatching

-- | Inspection that tells whether an expression uses while
-- in its definition
usesWhileMatching :: Matcher -> Inspection
usesWhileMatching matcher = positive (countWhiles matcher)

countWhiles :: Matcher -> Counter
countWhiles matcher = countExpressions f
  where f (While e _) = matcher [e]
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
usesRepeat = unmatching usesRepeatMatching

usesRepeatMatching :: Matcher -> Inspection
usesRepeatMatching matcher = positive (countRepeats matcher)

countRepeats :: Matcher -> Counter
countRepeats matcher = countExpressions f
  where f (Repeat e _) = matcher [e]
        f _ = False

usesForEach :: Inspection
usesForEach = unmatching usesForEachMatching

usesForEachMatching :: Matcher -> Inspection
usesForEachMatching matcher = containsExpression f
  where f (For ss e) = not (usesYield e) && any (matcher.(:[])) (statementsExpressions ss)
        f _          = False

usesForLoop :: Inspection
usesForLoop = unmatching usesForLoopMatching

usesForLoopMatching :: Matcher -> Inspection
usesForLoopMatching matcher = positive (countForLoops matcher)

countForLoops :: Matcher -> Counter
countForLoops matcher = countExpressions f
  where f (ForLoop i c incr _) = matcher [i, c, incr]
        f _                    = False

usesIterationMatching :: Matcher -> Inspection
usesIterationMatching matcher = containsExpression f
  where f (Repeat _ e)      = matcher [e]
        f (While _ e)       = matcher [e]
        f (For _ e)         = not (usesYield e) && matcher [e]
        f (ForLoop _ _ _ e) = matcher [e]
        f _                 = False

usesLoop :: Inspection
usesLoop e = usesRepeat e || usesWhile e || usesForLoop e || usesForEach e

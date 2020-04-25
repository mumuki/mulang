module Language.Mulang.Inspector.Procedural (
  countForEaches,
  countForLoops,
  countProcedures,
  countRepeats,
  countWhiles,
  declaresProcedure,
  declaresProcedureMatching,
  usesForEach,
  usesForEachMatching,
  usesForLoop,
  usesForLoopMatching,
  usesLoop,
  usesRepeat,
  usesRepeatMatching,
  usesSwitch,
  usesSwitchMatching,
  usesWhile,
  usesWhileMatching) where

import Language.Mulang.Ast
import Language.Mulang.Generator (equationsExpandedExpressions, statementsExpressions)
import Language.Mulang.Inspector.Matcher (Matcher, unmatching, matches)
import Language.Mulang.Inspector.Primitive (Inspection)
import Language.Mulang.Inspector.Bound (BoundCounter, BoundInspection, countBoundDeclarations, uncounting)
import Language.Mulang.Inspector.Generic (usesYield)
import Language.Mulang.Inspector.Combiner (derive, InspectionFamily)

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
(usesWhile, usesWhileMatching, countWhiles) = derive f :: InspectionFamily
  where f matcher (While c a) = matcher [c, a]
        f _        _ = False

-- | Inspection that tells whether an expression uses Switch
-- in its definition
(usesSwitch, usesSwitchMatching, countSwitches) = derive f :: InspectionFamily
  where f matcher (Switch value cases orElse) = matcher [value, (Sequence . map snd $ cases), orElse]
        f _        _                          = False

-- | Inspection that tells whether an expression uses reoeat
-- in its definition
(usesRepeat, usesRepeatMatching, countRepeats) = derive f :: InspectionFamily
  where f matcher (Repeat c a) = matcher [c, a]
        f _       _            = False

(usesForEach, usesForEachMatching, countForEaches) = derive f :: InspectionFamily
  where f matcher (For ss e) = not (usesYield e) && matcher [Sequence (statementsExpressions ss), e]
        f _       _          = False

(usesForLoop, usesForLoopMatching, countForLoops) = derive f :: InspectionFamily
  where f matcher (ForLoop i c incr e) = matcher [i, c, incr, e]
        f _       _                    = False

usesLoop :: Inspection
usesLoop e = usesRepeat e || usesWhile e || usesForLoop e || usesForEach e

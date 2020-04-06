module Language.Mulang.Inspector.Procedural (
  countProcedures,
  countForLoops,
  countRepeats,
  countWhiles,
  declaresProcedure,
  declaresProcedureMatching,
  subordinatesDeclatationsTo,
  usesForEach,
  usesForEachMatching,
  usesForLoop,
  usesForLoopMatching,
  usesLoop,
  usesRepeat,
  usesRepeatMatching,
  usesSwitch,
  usesWhile,
  usesWhileMatching) where

import Language.Mulang.Ast
import Language.Mulang.Generator (Generator, equationsExpandedExpressions, statementsExpressions, declaredIdentifiers, boundDeclarators)
import Language.Mulang.Inspector.Matcher (Matcher, unmatching, matches)
import Language.Mulang.Inspector.Primitive (Inspection, Counter, containsExpression, positive, countExpressions)
import Language.Mulang.Inspector.Bound (BoundCounter, BoundInspection, countBoundDeclarations, uncounting)
import Language.Mulang.Inspector.Generic (usesYield, uses)
import Language.Mulang.Inspector.Combiner (transitive)
import Language.Mulang.Inspector.Query (select, inspect)
import Language.Mulang.Identifier (named)

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
  where f (While c a) = matcher [c, a]
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
  where f (Repeat c a) = matcher [c, a]
        f _ = False

usesForEach :: Inspection
usesForEach = unmatching usesForEachMatching

usesForEachMatching :: Matcher -> Inspection
usesForEachMatching matcher = containsExpression f
  where f (For ss e) = not (usesYield e) && matcher [Sequence (statementsExpressions ss), e]
        f _          = False

usesForLoop :: Inspection
usesForLoop = unmatching usesForLoopMatching

usesForLoopMatching :: Matcher -> Inspection
usesForLoopMatching matcher = positive (countForLoops matcher)

countForLoops :: Matcher -> Counter
countForLoops matcher = countExpressions f
  where f (ForLoop i c incr e) = matcher [i, c, incr, e]
        f _                    = False

usesLoop :: Inspection
usesLoop e = usesRepeat e || usesWhile e || usesForLoop e || usesForEach e


subordinatesDeclatationsTo :: BoundInspection
subordinatesDeclatationsTo main expression = inspect $ do
  (name, _) <- boundDeclarators main expression
  select (all (referencedFrom name) (otherDeclaredIdentifiers name expression))

  where
    otherDeclaredIdentifiers :: Identifier -> Generator Identifier
    otherDeclaredIdentifiers name = filter (/=name) . declaredIdentifiers

    referencedFrom :: Identifier -> Identifier -> Bool
    referencedFrom main identifier = transitive main (uses (named identifier)) expression

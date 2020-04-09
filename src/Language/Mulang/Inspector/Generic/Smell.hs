{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Inspector.Generic.Smell (
  discardsExceptions,
  doesConsolePrint,
  doesNilTest,
  doesTypeTest,
  hasAssignmentReturn,
  hasEmptyIfBranches,
  hasEmptyRepeat,
  hasLongParameterList,
  hasRedundantBooleanComparison,
  hasRedundantGuards,
  hasRedundantIf,
  hasRedundantLambda,
  hasRedundantLocalVariableReturn,
  hasRedundantParameter,
  hasRedundantRepeat,
  hasTooManyMethods,
  detectDeclarationTypos,
  detectUsageTypos,
  hasUnreachableCode,
  isLongCode,
  overridesEqualOrHashButNotBoth,
  returnsNil,
  shouldInvertIfCondition,
  shouldUseOtherwise) where

import           Language.Mulang.Ast
import qualified Language.Mulang.Ast.Operator as O
import           Language.Mulang.Generator (identifierReferences, declaredIdentifiers, referencedIdentifiers)
import           Language.Mulang.Inspector.Primitive

import           Data.Text.Metrics (damerauLevenshtein)
import           Data.Text (pack)

-- | Inspection that tells whether an expression has expressions like 'x == True'
hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = compares isBooleanLiteral

hasRedundantRepeat :: Inspection
hasRedundantRepeat = containsExpression f
  where f (Repeat (MuNumber 1) _) = True
        f _                       = False

doesNilTest :: Inspection
doesNilTest = compares f
  where f MuNil = True
        f _     = False

doesTypeTest :: Inspection
doesTypeTest = compares f
  where f (MuString _) = True
        f _            = False

isLongCode :: Inspection
isLongCode = containsExpression f
  where f (Sequence xs)  = (length xs) >= 16
        f  _             = False

compares :: (Expression -> Bool) -> Inspection
compares f = containsExpression (any f.comparisonOperands)

comparisonOperands (Call Equal                [a1, a2])   = [a1, a2] -- deprecated
comparisonOperands (Call NotEqual             [a1, a2])   = [a1, a2] -- deprecated
comparisonOperands (Call (Primitive O.Equal)    [a1, a2])   = [a1, a2]
comparisonOperands (Call (Primitive O.NotEqual) [a1, a2])   = [a1, a2]
comparisonOperands _                                      = []

returnsNil :: Inspection
returnsNil = containsExpression f
  where f (Return MuNil) = True
        f _              = False

-- | Inspection that tells whether an expression has an if expression where both branches return
-- boolean literals
hasRedundantIf :: Inspection
hasRedundantIf = containsExpression f
  where f (If _ (Assignment v1 x) (Assignment v2 y)) = all isBooleanLiteral [x, y] && v1 == v2
        f (If _ (Variable v1 x) (Variable v2 y))     = all isBooleanLiteral [x, y] && v1 == v2
        f (If _ (Return x) (Return y))               = all isBooleanLiteral [x, y]
        f (If _ (Yield x) (Yield y))                 = all isBooleanLiteral [x, y]
        f (If _ x y)                                 = all isBooleanLiteral [x, y]
        f (Sequence expressions)                     = containsUnstructuredBooleanReturn expressions
        f _                                          = False

        containsUnstructuredBooleanReturn (If _ (Return x) None:Return y:_) | all isBooleanLiteral [x, y] = True
        containsUnstructuredBooleanReturn (_:xs)                            = containsUnstructuredBooleanReturn xs
        containsUnstructuredBooleanReturn []                                = False

-- | Inspection that tells whether an expression has guards where both branches return
-- boolean literals
hasRedundantGuards :: Inspection
hasRedundantGuards = containsBody f -- TODO not true when condition is a pattern
  where f (GuardedBody [
            (_, Return x),
            (Primitive O.Otherwise, Return y)]) = all isBooleanLiteral [x, y]
        f _ = False

-- | Inspection that tells whether an expression has guards with a hardcoded false instead of an otherwise
shouldUseOtherwise :: Inspection
shouldUseOtherwise = containsBody f
  where f (GuardedBody (last -> (MuTrue, _))) = True
        f _                                   = False

-- | Inspection that tells whether an expression has lambda expressions like '\x -> g x'
hasRedundantLambda :: Inspection
hasRedundantLambda = containsExpression f
  where f (Lambda [VariablePattern (x)] (Return (Call _ [Reference (y)]))) = x == y
        f _ = False

-- | Inspection that tells whether an expression has parameters that
-- can be avoided using point-free
hasRedundantParameter :: Inspection
hasRedundantParameter = containsExpression f
  where f function@(SimpleFunction _ params (Return (Application _ args))) | Just (VariablePattern param) <- safeLast params,
                                                                             Just (Reference arg) <- safeLast args = param == arg && showsUpOnlyOnce param (identifierReferences function)
        f _ = False
        showsUpOnlyOnce p = (==1).countElem p
        countElem p = length.filter (==p)
        safeLast [] = Nothing
        safeLast l = Just $ last l

isBooleanLiteral (MuBool _) = True
isBooleanLiteral _          = False

hasRedundantLocalVariableReturn :: Inspection
hasRedundantLocalVariableReturn = containsExpression f
  where f (Sequence [ Variable declaredVariable _,
                      Return (Reference returnedVariable)]) = returnedVariable == declaredVariable
        f _                                                 = False

hasAssignmentReturn :: Inspection
hasAssignmentReturn = containsExpression f
  where f (Return (Assignment _ _)) = True
        f (Return (Variable _ _))   = True
        f _                         = False

discardsExceptions :: Inspection
discardsExceptions = containsExpression f
  where f (Try _ [(_, None)] _)  = True
        f (Try _ [(_, Print _)] _) = True
        f _                        = False

doesConsolePrint :: Inspection
doesConsolePrint = containsExpression f
  where f (Print _) = True
        f _         = False

hasLongParameterList :: Inspection
hasLongParameterList = containsExpression f
  where f (Params p) = (>4).length $ p
        f _ = False

hasTooManyMethods :: Inspection
hasTooManyMethods = containsExpression f
  where f (Sequence expressions) = (>15).length.filter isMethod $ expressions
        f _ = False

        isMethod (Method _ _) = True
        isMethod _ = False

overridesEqualOrHashButNotBoth :: Inspection
overridesEqualOrHashButNotBoth = containsExpression f
  where f (Sequence expressions) = (any isEqual expressions) /= (any isHash expressions)
        f (Class _ _ (PrimitiveMethod O.Equal _)) = True
        f (Class _ _ (EqualMethod _))             = True
        f (Class _ _ (PrimitiveMethod O.Hash _))  = True
        f (Class _ _ (HashMethod _))              = True
        f _ = False

        isEqual (PrimitiveMethod O.Equal _) = True
        isEqual (EqualMethod _)             = True -- deprecated
        isEqual _ = False

        isHash (PrimitiveMethod O.Hash _) = True
        isHash (HashMethod _)             = True -- deprecated
        isHash _ = False

shouldInvertIfCondition :: Inspection
shouldInvertIfCondition = containsExpression f
  where f (If _ None elseBranch) = elseBranch /= None
        f _                        = False

hasEmptyIfBranches :: Inspection
hasEmptyIfBranches = containsExpression f
  where f (If _ None None) = True
        f _                = False

hasEmptyRepeat :: Inspection
hasEmptyRepeat = containsExpression f
  where f (Repeat _ None) = True
        f _               = False

hasUnreachableCode :: Inspection
hasUnreachableCode = containsExpression f
  where f (Subroutine _ equations) = any equationMatchesAnyValue . init $ equations
        f (Sequence expressions)   = hasCodeAfterReturn expressions
        f _                        = False

        equationMatchesAnyValue (Equation patterns body) = all patternMatchesAnyValue patterns && bodyMatchesAnyValue body

        patternMatchesAnyValue WildcardPattern     = True
        patternMatchesAnyValue (VariablePattern _) = True
        patternMatchesAnyValue _                   = False

        bodyMatchesAnyValue (UnguardedBody _)    = True
        bodyMatchesAnyValue (GuardedBody guards) = any (isTruthy . fst) guards

        isTruthy (MuBool True)           = True
        isTruthy (Primitive O.Otherwise) = True
        isTruthy _                       = False

        hasCodeAfterReturn []           = False
        hasCodeAfterReturn [_]          = False
        hasCodeAfterReturn (Return _:_) = True
        hasCodeAfterReturn (_:xs)       = hasCodeAfterReturn xs


detectDeclarationTypos :: Identifier -> Expression -> [Identifier]
detectDeclarationTypos name expression | elem name identifiers = []
                                       | otherwise = filter (similar name) identifiers
  where
      identifiers  = declaredIdentifiers expression
      similar one other = damerauLevenshtein (pack one) (pack other) == 1


detectUsageTypos :: Identifier -> Expression -> [Identifier]
detectUsageTypos name expression | elem name identifiers = []
                                 | otherwise = filter (similar name) identifiers
  where
      identifiers  = referencedIdentifiers expression
      similar one other = damerauLevenshtein (pack one) (pack other) == 1

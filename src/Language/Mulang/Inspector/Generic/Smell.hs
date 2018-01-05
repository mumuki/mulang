module Language.Mulang.Inspector.Generic.Smell (
  hasRedundantBooleanComparison,
  hasRedundantIf,
  hasRedundantGuards,
  hasRedundantLambda,
  hasRedundantParameter,
  hasRedundantLocalVariableReturn,
  hasAssignmentReturn,
  hasEmptyIfBranches,
  doesNullTest,
  doesTypeTest,
  isLongCode,
  returnsNull,
  discardsExceptions,
  doesConsolePrint,
  hasLongParameterList,
  hasTooManyMethods,
  overridesEqualOrHashButNotBoth) where

import Language.Mulang.Ast
import Language.Mulang.Inspector
import Language.Mulang.Generator(identifierReferences)

-- | Inspection that tells whether an identifier has expressions like 'x == True'
hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = compares isBooleanLiteral

doesNullTest :: Inspection
doesNullTest = compares f
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

comparisonOperands (Call Equal    [a1, a2])   = [a1, a2]
comparisonOperands (Call NotEqual [a1, a2])   = [a1, a2]
comparisonOperands _                          = []

returnsNull :: Inspection
returnsNull = containsExpression f
  where f (Return MuNil) = True
        f _              = False

-- | Inspection that tells whether an identifier has an if expression where both branches return
-- boolean literals
hasRedundantIf :: Inspection
hasRedundantIf = containsExpression f
  where f (If _ (Assignment v1 x) (Assignment v2 y)) = all isBooleanLiteral [x, y] && v1 == v2
        f (If _ (Variable v1 x) (Variable v2 y))     = all isBooleanLiteral [x, y] && v1 == v2
        f (If _ (Return x) (Return y))               = all isBooleanLiteral [x, y]
        f (If _ (Yield x) (Yield y))                 = all isBooleanLiteral [x, y]
        f (If _ x y)                                 = all isBooleanLiteral [x, y]
        f _                                          = False

-- | Inspection that tells whether an identifier has guards where both branches return
-- boolean literals
hasRedundantGuards :: Inspection
hasRedundantGuards = containsBody f -- TODO not true when condition is a pattern
  where f (GuardedBody [
            (_, Return x),
            (Reference "otherwise", Return y)]) = all isBooleanLiteral [x, y]
        f _ = False


-- | Inspection that tells whether an identifier has lambda expressions like '\x -> g x'
hasRedundantLambda :: Inspection
hasRedundantLambda = containsExpression f
  where f (Lambda [VariablePattern (x)] (Return (Call _ [Reference (y)]))) = x == y
        f _ = False


-- | Inspection that tells whether an identifier has parameters that
-- can be avoided using point-free
hasRedundantParameter :: Inspection
hasRedundantParameter = containsExpression f
  where f function@(SimpleFunction _ params (Return (Application _ args))) | (VariablePattern param) <- last params,
                                                                             (Reference arg) <- last args = param == arg && showsUpOnlyOnce param (identifierReferences function)
        f _ = False
        showsUpOnlyOnce p = (==1).countElem p
        countElem p = length.filter (==p)

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
  where f (Try _ [(_, MuNull)] _)  = True
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
        f (Class _ _ (EqualMethod _)) = True
        f (Class _ _ (HashMethod _)) = True
        f _ = False

        isEqual (EqualMethod _) = True
        isEqual _ = False
        
        isHash (HashMethod _) = True
        isHash _ = False

hasEmptyIfBranches :: Inspection
hasEmptyIfBranches = containsExpression f
  where f (If _ MuNull elseBranch) = elseBranch /= MuNull
        f _                        = False

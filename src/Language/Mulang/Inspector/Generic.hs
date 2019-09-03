module Language.Mulang.Inspector.Generic (
  countIfs,
  countFors,
  countFunctions,
  countVariables,
  assigns,
  assignsMatching,
  calls,
  callsMatching,
  declares,
  declaresComputation,
  declaresComputationWithArity,
  declaresComputationWithArity',
  declaresEntryPoint,
  declaresEntryPointMatching,
  declaresFunction,
  declaresFunctionMatching,
  declaresRecursively,
  declaresVariable,
  declaresVariableMatching,
  delegates,
  delegates',
  parses,
  raises,
  rescues,
  returnsMatching,
  uses,
  usesAnonymousVariable,
  usesLogic,
  usesMath,
  usesExceptionHandling,
  usesExceptions,
  usesFor,
  usesIf,
  usesIfMatching,
  usesThenMatching,
  usesElseMatching,
  usesPrimitive,
  usesPrint,
  usesPrintMatching,
  usesYield,
  usesYieldMatching) where

import Language.Mulang.Ast hiding (Equal, NotEqual)
import Language.Mulang.Ast.Operator (Operator (..))
import Language.Mulang.Generator (declaredIdentifiers, expressions, declarations, referencedIdentifiers, equationsExpandedExpressions)
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Bound (uncounting, containsBoundDeclaration, countBoundDeclarations, BoundInspection, BoundCounter)
import Language.Mulang.Inspector.Contextualized (decontextualize, ContextualizedBoundInspection)
import Language.Mulang.Inspector.Primitive
import Language.Mulang.Inspector.Matcher (unmatching, matches, Matcher)
import Language.Mulang.Inspector.Query (inspect, select)
import Language.Mulang.Inspector.Literal (isMath, isLogic)

import Data.Maybe (listToMaybe)
import Data.List.Extra (has)

-- | Inspection that tells whether an expression is equal to a given piece of code after being parsed
parses :: (Code -> Expression) -> Code -> Inspection
parses parser code = (== (parser code))

assigns :: BoundInspection
assigns = unmatching assignsMatching

assignsMatching :: Matcher -> BoundInspection
assignsMatching matcher predicate = containsExpression f
  where f (Unification name value) = predicate name && matcher [value]
        f _                        = False

-- | Inspection that tells whether an expression uses the the given target identifier
-- in its definition
uses :: BoundInspection
uses p = containsExpression f
  where f = any p . referencedIdentifiers

usesPrimitive :: Operator -> Inspection
usesPrimitive operator = containsExpression f
  where f (Primitive o) = operator == o
        f _             = False

calls :: BoundInspection
calls = unmatching callsMatching

callsMatching :: Matcher -> BoundInspection
callsMatching matcher p = containsExpression f
  where f (Call (Reference id) arguments) = p id && matcher arguments
        f _                               = False

delegates :: BoundInspection
delegates = decontextualize . delegates'

delegates' :: ContextualizedBoundInspection
delegates' p context expression = inspect $ do
  (Subroutine name1 _)       <- declarations context
  (Call (Reference name2) _) <- expressions expression
  select (name1 == name2)
  select (p name1)

-- | Inspection that tells whether an expression uses ifs
-- in its definition
usesIf :: Inspection
usesIf = unmatching usesIfMatching

usesIfMatching :: Matcher -> Inspection
usesIfMatching matcher = positive (countIfs matcher)

countIfs :: Matcher -> Counter
countIfs matcher = countExpressions f
  where f (If c _ _) = matcher [c]
        f _          = False

usesThenMatching :: Matcher -> Inspection
usesThenMatching matcher = containsExpression f
  where f (If _ t _) = matcher [t]
        f _          = False

usesElseMatching :: Matcher -> Inspection
usesElseMatching matcher = containsExpression f
  where f (If _ _ e) = matcher [e]
        f _          = False

usesYield :: Inspection
usesYield = unmatching usesYieldMatching

usesYieldMatching ::  Matcher -> Inspection
usesYieldMatching matcher = containsExpression f
  where f (Yield e) = matcher [e]
        f _         = False

usesPrint :: Inspection
usesPrint = unmatching usesPrintMatching

usesPrintMatching :: Matcher -> Inspection
usesPrintMatching matcher = containsExpression f
  where f (Print e) = matcher [e]
        f _         = False

usesFor :: Inspection
usesFor = positive countFors

countFors :: Counter
countFors = countExpressions f
  where f (For _ _) = True
        f _         = False


returnsMatching :: Matcher -> Inspection
returnsMatching matcher = containsExpression f
  where f (Return body) = matcher [body]
        f _             = False

-- | Inspection that tells whether a top level declaration exists
declares :: BoundInspection
declares = containsBoundDeclaration f
  where f (TypeSignature _ _) = False
        f _                   = True

-- | Inspection that tells whether an expression is direct recursive
declaresRecursively :: BoundInspection
declaresRecursively = containsBoundDeclaration f
  where f e | (Just name) <- (nameOf e) = uses (named name) e
            | otherwise = False

        nameOf :: Expression -> Maybe Identifier
        nameOf = listToMaybe . declaredIdentifiers

declaresFunction :: BoundInspection
declaresFunction = unmatching declaresFunctionMatching

declaresFunctionMatching :: Matcher -> BoundInspection
declaresFunctionMatching = uncounting countFunctions

countFunctions :: Matcher -> BoundCounter
countFunctions matcher = countBoundDeclarations f
  where f (Function _ equations) = matches matcher equationsExpandedExpressions $ equations
        f _                      = False

declaresVariable :: BoundInspection
declaresVariable = unmatching declaresVariableMatching

declaresVariableMatching :: Matcher -> BoundInspection
declaresVariableMatching = uncounting countVariables

countVariables :: Matcher -> BoundCounter
countVariables matcher = countBoundDeclarations f
  where f (Variable _ body) = matches matcher id [body]
        f _                 = False

declaresEntryPoint :: BoundInspection
declaresEntryPoint = unmatching declaresEntryPointMatching

declaresEntryPointMatching :: Matcher -> BoundInspection
declaresEntryPointMatching matcher = containsBoundDeclaration f
  where f (EntryPoint _ e)  = matcher [e]
        f _                 = False

-- | Inspection that tells whether a top level computation declaration exists
declaresComputation :: BoundInspection
declaresComputation = declaresComputationWithArity' (const True)

declaresComputationWithArity :: Int -> BoundInspection
declaresComputationWithArity arity = declaresComputationWithArity' (== arity)

declaresComputationWithArity' :: (Int -> Bool) -> BoundInspection
declaresComputationWithArity' arityPredicate = containsBoundDeclaration f
  where f (Subroutine _ es)       = any equationArityIs es
        f (Clause _ args _)       = argsHaveArity args
        f _  = False

        equationArityIs (Equation args _) = argsHaveArity args

        argsHaveArity = arityPredicate.length

usesLogic :: Inspection
usesLogic = containsExpression isLogic

usesMath :: Inspection
usesMath = containsExpression isMath

raises :: BoundInspection
raises predicate = containsExpression f
  where f (Raise (New (Reference n) _))         = predicate n
        f (Raise (Application (Reference n) _)) = predicate n
        f (Raise (Reference n))                 = predicate n
        f _                                     = False

usesExceptions :: Inspection
usesExceptions = containsExpression f
  where f (Raise _)     = True
        f _             = False

rescues :: BoundInspection
rescues predicate = containsExpression f
  where f (Try _ rescues _) = any (matchesType predicate) . map fst  $ rescues
        f _                 = False

usesExceptionHandling :: Inspection
usesExceptionHandling  = containsExpression f
  where f (Try _ _ _) = True
        f _           = False

usesAnonymousVariable :: Inspection
usesAnonymousVariable = containsExpression f
  where f (Subroutine _ body)    = equationContainsWildcard body
--TODO        f (Lambda args _)  = equationContainsWildcard equations
        f (Clause _ params _)    = any isOrContainsWildcard params
        f _                      = False

        equationContainsWildcard =  has isOrContainsWildcard subroutineBodyPatterns

        isOrContainsWildcard (InfixApplicationPattern p1 _ p2) = any isOrContainsWildcard [p1, p2]
        isOrContainsWildcard (ApplicationPattern _ ps)         = any isOrContainsWildcard ps
        isOrContainsWildcard (TuplePattern ps)                 = any isOrContainsWildcard ps
        isOrContainsWildcard (ListPattern ps)                  = any isOrContainsWildcard ps
        isOrContainsWildcard (FunctorPattern _ ps)             = any isOrContainsWildcard ps
        isOrContainsWildcard (AsPattern _ p)                   = isOrContainsWildcard p
        isOrContainsWildcard WildcardPattern                   = True
        isOrContainsWildcard _                                 = False

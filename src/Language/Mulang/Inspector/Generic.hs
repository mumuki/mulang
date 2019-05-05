module Language.Mulang.Inspector.Generic (
  assigns,
  assignsMatching,
  calls,
  callsMatching,
  declares,
  declaresComputation,
  declaresComputationWithArity,
  declaresComputationWithArity',
  declaresEntryPoint,
  declaresFunction,
  declaresRecursively,
  declaresVariable,
  delegates,
  delegates',
  parses,
  raises,
  rescues,
  returnsMatching,
  uses,
  usesAnonymousVariable,
  usesBooleanLogic,
  usesArithmetic,
  usesExceptionHandling,
  usesExceptions,
  usesFor,
  usesIf,
  usesPrimitive,
  usesPrint,
  usesYield) where

import Language.Mulang.Ast hiding (Equal, NotEqual)
import Language.Mulang.Ast.Operator (Operator (..))
import Language.Mulang.Generator (declaredIdentifiers, expressions, declarations, referencedIdentifiers)
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Bound (containsBoundDeclaration, BoundInspection)
import Language.Mulang.Inspector.Contextualized (decontextualize, ContextualizedBoundInspection)
import Language.Mulang.Inspector.Primitive
import Language.Mulang.Inspector.Matcher (unmatching, Matcher)
import Language.Mulang.Inspector.Query (inspect, select)

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
usesIf = containsExpression f
  where f (If _ _ _) = True
        f _          = False

usesYield :: Inspection
usesYield = containsExpression f
  where f (Yield _) = True
        f _         = False

usesPrint :: Inspection
usesPrint = containsExpression f
  where f (Print _) = True
        f _         = False

usesFor :: Inspection
usesFor = containsExpression f
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
declaresFunction = containsBoundDeclaration f
  where f (Function _ _) = True
        f _              = False

declaresVariable :: BoundInspection
declaresVariable = containsBoundDeclaration f
  where f (Variable _ _)  = True
        f _               = False

declaresEntryPoint :: BoundInspection
declaresEntryPoint = containsBoundDeclaration f
  where f (EntryPoint _ _)  = True
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

usesBooleanLogic :: Inspection
usesBooleanLogic = containsExpression f
  where f (Primitive Negation) = True
        f (Primitive And)      = True
        f (Primitive Or)       = True
        f _                    = False

usesArithmetic :: Inspection
usesArithmetic = containsExpression f
  where f (Primitive Plus)     = True
        f (Primitive Minus)    = True
        f (Primitive Multiply) = True
        f (Primitive Divide)   = True
        f _                    = False

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

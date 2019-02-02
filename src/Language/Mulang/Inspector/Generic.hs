module Language.Mulang.Inspector.Generic (
  assigns,
  assignsWith,
  calls,
  callsWith,
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
  uses,
  usesAnonymousVariable,
  usesExceptionHandling,
  usesExceptions,
  usesFor,
  usesIf,
  usesLiteral,
  isLiteral,
  areLiterals,
  usesYield) where

import Language.Mulang.Ast
import Language.Mulang.Generator (declaredIdentifiers, expressions, declarations, referencedIdentifiers)
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Bound (containsBoundDeclaration, BoundInspection)
import Language.Mulang.Inspector.Contextualized (decontextualize, ContextualizedBoundInspection)
import Language.Mulang.Inspector.Primitive
import Language.Mulang.Inspector.Query (inspect, select)

import Data.Maybe (listToMaybe)
import Data.List.Extra (has)

import Text.Read (readMaybe)

-- | Inspection that tells whether an expression is equal to a given piece of code after being parsed
parses :: (Code -> Expression) -> Code -> Inspection
parses parser code = (== (parser code))

type MultiInspection = [Expression] -> Bool

anyExpressions :: MultiInspection
anyExpressions = const True

usesLiteral :: Code -> Inspection
usesLiteral value = containsExpression (isLiteral value)

isLiteral :: Code -> Inspection
isLiteral value = f
  where f MuNil              = value == "Nil"
        f (MuNumber number)  = (readMaybe value) == Just number
        f (MuBool bool)      = (readMaybe value) == Just bool
        f (MuString string)  = (readMaybe value) == Just string
        f (MuChar char)      = (readMaybe value) == Just char
        f (MuSymbol string)  = (readMaybe value) == Just ("#" ++ string)

areLiterals :: [Code] -> MultiInspection
areLiterals codes expressions = and (zipWith isLiteral codes expressions)

assigns :: BoundInspection
assigns = assignsWith anyExpression

assignsWith :: Inspection -> BoundInspection
assignsWith valueMatcher predicate = containsExpression f
  where f (Assignment name value)  = predicate name && valueMatcher value
        f (Variable name value)    = predicate name && valueMatcher value
        f (Attribute name value)   = predicate name && valueMatcher value
        f _                        = False

-- | Inspection that tells whether an expression uses the the given target identifier
-- in its definition
uses :: BoundInspection
uses p = containsExpression f
  where f = any p . referencedIdentifiers

calls :: BoundInspection
calls = callsWith anyExpressions

callsWith :: MultiInspection -> BoundInspection
callsWith argumentsMatcher p = containsExpression f
  where f (Call (Reference id) arguments) = p id && argumentsMatcher arguments
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

usesFor :: Inspection
usesFor = containsExpression f
  where f (For _ _) = True
        f _         = False


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

raises :: BoundInspection
raises predicate = containsExpression f
  where f (Raise (New (Reference n) _)) = predicate n
        f (Raise (Reference n))         = predicate n
        f _                             = False

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

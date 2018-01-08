module Language.Mulang.Inspector.Generic (
  parses,
  assigns,
  calls,
  uses,
  usesIf,
  usesYield,
  usesFor,
  declares,
  declaresVariable,
  declaresRecursively,
  declaresEntryPoint,
  declaresFunction,
  declaresComputation,
  declaresComputationWithArity,
  declaresComputationWithArity',
  declaresTypeAlias,
  declaresTypeSignature,
  usesAnonymousVariable,
  raises,
  rescues,
  usesExceptions,
  usesExceptionHandling,
  containsExpression,
  containsDeclaration,
  containsBoundDeclaration,
  containsBody,
  matchesType,
  typesReturnAs,
  typesParameterAs,
  typesAs,
  usesType,
  Inspection,
  IdentifierInspection) where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Generator (expressions, boundDeclarations, equationBodies, declarations, referencedIdentifiers)

import Data.Maybe (listToMaybe)

type Inspection = Expression  -> Bool
type IdentifierInspection = IdentifierPredicate -> Inspection

typesReturnAs :: IdentifierInspection
typesReturnAs predicate = containsDeclaration f
  where f (SubroutineTypeSignature _ _ name)  = predicate name
        f _                                   = False

typesParameterAs :: IdentifierInspection
typesParameterAs predicate = containsDeclaration f
  where f (SubroutineTypeSignature _ names _)  = any predicate names
        f _                                    = False

typesAs :: IdentifierInspection
typesAs predicate = containsDeclaration f
  where f (TypeSignature _ Nothing name)   = predicate name
        f _                                = False

usesType :: IdentifierInspection
usesType predicate = containsDeclaration f
  where f (TypeSignature _ _ name)            | predicate name = True
        f (SubroutineTypeSignature _ names _) = any predicate names
        f _                                   = False

-- | Inspection that tells whether an expression is equal to a given piece of code after being parsed
parses :: (String -> Expression) -> String -> Inspection
parses parser code = (== (parser code))

assigns :: IdentifierInspection
assigns predicate = containsExpression f
  where f (Assignment name _)  = predicate name
        f (Variable name _)    = predicate name
        f (Attribute name _)   = predicate name
        f _                    = False

-- | Inspection that tells whether an expression uses the the given target identifier
-- in its definition
uses :: IdentifierInspection
uses p = containsExpression f
  where f = any p . referencedIdentifiers

calls :: IdentifierInspection
calls p = containsExpression f
  where f (Call (Reference id) _ ) = p id
        f _                        = False


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
declares :: IdentifierInspection
declares = containsBoundDeclaration f
  where f (TypeSignature _ _ _) = False
        f _                     = True

-- | Inspection that tells whether an expression is direct recursive
declaresRecursively :: IdentifierInspection
declaresRecursively = containsBoundDeclaration f
  where f e | (Just name) <- (nameOf e) = uses (named name) e
            | otherwise = False

        nameOf :: Expression -> Maybe Identifier
        nameOf = fmap fst . listToMaybe . declarations


declaresFunction :: IdentifierInspection
declaresFunction = containsBoundDeclaration f
  where f (Function _ _) = True
        f _              = False

declaresVariable :: IdentifierInspection
declaresVariable = containsBoundDeclaration f
  where f (Variable _ _)  = True
        f _               = False

declaresEntryPoint :: IdentifierInspection
declaresEntryPoint = containsBoundDeclaration f
  where f (EntryPoint _ _)  = True
        f _                 = False

-- | Inspection that tells whether a top level computation declaration exists
declaresComputation :: IdentifierInspection
declaresComputation = declaresComputationWithArity' (const True)

declaresComputationWithArity :: Int -> IdentifierInspection
declaresComputationWithArity arity = declaresComputationWithArity' (== arity)

declaresComputationWithArity' :: (Int -> Bool) -> IdentifierInspection
declaresComputationWithArity' arityPredicate = containsBoundDeclaration f
  where f (Subroutine _ es)       = any equationArityIs es
        f (Clause _ args _)       = argsHaveArity args
        f _  = False

        equationArityIs (Equation args _) = argsHaveArity args

        argsHaveArity = arityPredicate.length

declaresTypeAlias :: IdentifierInspection
declaresTypeAlias = containsBoundDeclaration f
  where f (TypeAlias _) = True
        f _             = False

declaresTypeSignature :: IdentifierInspection
declaresTypeSignature = containsBoundDeclaration f
  where f (TypeSignature _ _ _) = True
        f _                     = False

raises :: IdentifierInspection
raises predicate = containsExpression f
  where f (Raise (New n _))     = predicate n
        f (Raise (Reference n)) = predicate n
        f _                     = False

usesExceptions :: Inspection
usesExceptions = containsExpression f
  where f (Raise _)     = True
        f _             = False

rescues :: IdentifierInspection
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


containsExpression :: (Expression -> Bool) -> Inspection
containsExpression f = has f expressions

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodies

containsBoundDeclaration :: (Expression -> Bool) -> IdentifierInspection
containsBoundDeclaration f b  = has f (boundDeclarations b)

containsDeclaration :: (Expression -> Bool) -> Inspection
containsDeclaration f = has f (map snd . declarations)

matchesType :: IdentifierPredicate -> Pattern -> Bool
matchesType predicate (TypePattern n)               = predicate n
matchesType predicate (AsPattern _ (TypePattern n)) = predicate n
matchesType predicate (UnionPattern patterns)       = any (matchesType predicate) patterns
matchesType _         _                             = False

-- private

has f g = any f . g




module Language.Mulang.Inspector.Generic (
  parses,
  assigns,
  uses,
  usesIf,
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
  rescuesType,
  usesExceptions,
  usesExceptionHandling,
  containsExpression,
  containsDeclaration,
  containsBody,
  Inspection,
  BindedInspection) where

import Language.Mulang.Ast
import Language.Mulang.Binding
import Language.Mulang.Unfold (allExpressions)
import Language.Mulang.Explorer

type Inspection = Expression  -> Bool
type BindedInspection = BindingPredicate -> Inspection

-- | Inspection that tells whether an expression is equal to a given piece of code after being parsed
parses :: (String -> Expression) -> String -> Inspection
parses parser code = (== (parser code))

assigns :: BindedInspection
assigns predicate = containsExpression f
  where f (Assignment name _)  = predicate name
        f (Variable name _)    = predicate name
        f (Attribute name _)   = predicate name
        f _                    = False

-- | Inspection that tells whether an expression uses the the given target binding
-- in its definition
uses :: BindedInspection
uses p = containsExpression f
  where f = any p . map fst .  referencesOf

-- | Inspection that tells whether an expression uses ifs
-- in its definition
usesIf :: Inspection
usesIf = containsExpression f
  where f (If _ _ _) = True
        f _          = False

-- | Inspection that tells whether a top level binding exists
declares :: BindedInspection
declares = containsDeclaration f
  where f (TypeSignature _ _ _) = False
        f _                     = True

-- | Inspection that tells whether an expression is direct recursive
declaresRecursively :: BindedInspection
declaresRecursively = containsDeclaration f
  where f e | (Just name) <- (nameOf e) = uses (named name) e
            | otherwise = False


declaresFunction :: BindedInspection
declaresFunction = containsDeclaration f
  where f (Function _ _) = True
        f _              = False

declaresVariable :: BindedInspection
declaresVariable = containsDeclaration f
  where f (Variable _ _)  = True
        f _               = False

declaresEntryPoint :: BindedInspection
declaresEntryPoint = containsDeclaration f
  where f (EntryPoint _ _)  = True
        f _                 = False

-- | Inspection that tells whether a top level computation binding exists
declaresComputation :: BindedInspection
declaresComputation = declaresComputationWithArity' (const True)

declaresComputationWithArity :: Int -> BindedInspection
declaresComputationWithArity arity = declaresComputationWithArity' (== arity)

declaresComputationWithArity' :: (Int -> Bool) -> BindedInspection
declaresComputationWithArity' arityPredicate = containsDeclaration f
  where f (Subroutine _ es)       = any equationArityIs es
        f (Clause _ args _)       = argsHaveArity args
        f _  = False

        equationArityIs = \(Equation args _) -> argsHaveArity args

        argsHaveArity = arityPredicate.length

declaresTypeAlias :: BindedInspection
declaresTypeAlias = containsDeclaration f
  where f (TypeAlias _) = True
        f _             = False

declaresTypeSignature :: BindedInspection
declaresTypeSignature = containsDeclaration f
  where f (TypeSignature _ _ _) = True
        f _                     = False

raises :: BindedInspection
raises predicate = containsExpression f
  where f (Raise (New n _))     = predicate n
        f (Raise (Reference n)) = predicate n
        f _                     = False

usesExceptions :: Inspection
usesExceptions = containsExpression f
  where f (Raise _)     = True
        f _             = False

rescuesType :: BindedInspection
rescuesType predicate = containsExpression f
  where f (Rescue (TypePattern n) _)               = predicate n
        f (Rescue (AsPattern _ (TypePattern n)) _) = predicate n
        f _                                        = False

usesExceptionHandling :: Inspection
usesExceptionHandling  = containsExpression f
  where f (Rescue _ _) = True
        f _            = False

usesAnonymousVariable :: Inspection
usesAnonymousVariable = containsExpression f
  where f (Subroutine _ equations)    = equationContainsWildcard equations
--TODO        f (Lambda args _)                      = equationContainsWildcard equations
        f (Clause _ params _)         = paramsContainsWildcard params
        f _                           = False

        equationContainsWildcard = any (paramsContainsWildcard . equationParams)
        paramsContainsWildcard = any isOrContainsWildcard

        isOrContainsWildcard (InfixApplicationPattern p1 _ p2) = any isOrContainsWildcard [p1, p2]
        isOrContainsWildcard (ApplicationPattern _ ps)         = any isOrContainsWildcard ps
        isOrContainsWildcard (TuplePattern ps)                 = any isOrContainsWildcard ps
        isOrContainsWildcard (ListPattern ps)                  = any isOrContainsWildcard ps
        isOrContainsWildcard (FunctorPattern _ ps)             = any isOrContainsWildcard ps
        isOrContainsWildcard (AsPattern _ p)                   = isOrContainsWildcard p
        isOrContainsWildcard WildcardPattern                   = True
        isOrContainsWildcard _                                 = False


containsExpression :: (Expression -> Bool) -> Inspection
containsExpression f = has f allExpressions

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodiesOf

containsDeclaration :: (Expression -> Bool) -> BindedInspection
containsDeclaration f b  = has f (bindedDeclarationsOf' b)

-- private

has f g = any f . g




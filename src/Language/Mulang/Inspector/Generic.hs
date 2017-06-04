module Language.Mulang.Inspector.Generic (
  parses,
  uses,
  usesIf,
  declares,
  declaresVariable,
  declaresRecursively,
  declaresEntryPoint,
  declaresFunction,
  declaresComputation,
  declaresComputationWithArity,
  declaresComputationWithExactArity,
  declaresTypeAlias,
  declaresTypeSignature,
  usesAnonymousVariable,
  containsExpression,
  containsDeclaration,
  containsBody,
  Inspection) where

import Language.Mulang.Ast
import Language.Mulang.Binding
import Language.Mulang.Explorer

type Inspection = Expression  -> Bool

-- | Inspection that tells whether an expression is equal to a given piece of code after being parsed
parses :: (String -> Expression) -> String -> Inspection
parses parser code = (== (parser code))

-- | Inspection that tells whether an expression uses the the given target binding
-- in its definition
uses :: BindingPredicate -> Inspection
uses p = containsExpression f
  where f = any p . map fst .  referencesOf

-- | Inspection that tells whether an expression uses ifs
-- in its definition
usesIf :: Inspection
usesIf = containsExpression f
  where f (If _ _ _) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
declares :: BindingPredicate -> Inspection
declares = containsDeclaration f
  where f (TypeSignature _ _) = False
        f _                   = True

-- | Inspection that tells whether an expression is direct recursive
declaresRecursively :: BindingPredicate -> Inspection
declaresRecursively = containsDeclaration f
  where f e | (Just name) <- (nameOf e) = uses (named name) e
            | otherwise = False


declaresFunction :: BindingPredicate -> Inspection
declaresFunction = containsDeclaration f
  where f (Function _ _) = True
        f _                         = False

declaresVariable :: BindingPredicate -> Inspection
declaresVariable = containsDeclaration f
  where f (Variable _ _)  = True
        f _                          = False

declaresEntryPoint :: BindingPredicate -> Inspection
declaresEntryPoint = containsDeclaration f
  where f (EntryPoint _)  = True
        f _               = False

-- | Inspection that tells whether a top level computation binding exists
declaresComputation :: BindingPredicate -> Inspection
declaresComputation = declaresComputationWithArity (const True)

declaresComputationWithExactArity :: Int -> BindingPredicate -> Inspection
declaresComputationWithExactArity arity = declaresComputationWithArity (== arity)

declaresComputationWithArity :: (Int -> Bool) -> BindingPredicate -> Inspection
declaresComputationWithArity arityPredicate = containsDeclaration f
  where f (Function _ equations)  = any equationArityIs equations
        f (Procedure _ equations) = any equationArityIs equations
        f (Method _ equations)    = any equationArityIs equations
        f (Rule _ args _)         = argsHaveArity args
        f (Fact _ args)           = argsHaveArity args
        f _  = False

        equationArityIs = \(Equation args _) -> argsHaveArity args

        argsHaveArity = arityPredicate.length

declaresTypeAlias :: BindingPredicate -> Inspection
declaresTypeAlias = containsDeclaration f
  where f (TypeAlias _) = True
        f _             = False

declaresTypeSignature :: BindingPredicate -> Inspection
declaresTypeSignature = containsDeclaration f
  where f (TypeSignature _ _)  = True
        f _                    = False


usesAnonymousVariable :: Inspection
usesAnonymousVariable = containsExpression f
  where f (Function _ equations)    = equationContainsWildcard equations
        f (Procedure _ equations)   = equationContainsWildcard equations
        f (Method _ equations)      = equationContainsWildcard equations
--TODO        f (Lambda args _)                      = equationContainsWildcard equations
        f (Fact _ params)             = paramsContainsWildcard params
        f (Rule _ params _)           = paramsContainsWildcard params
        f _                                    = False

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
containsExpression f = has f expressionsOf

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodiesOf

containsDeclaration :: (Expression -> Bool) -> BindingPredicate -> Inspection
containsDeclaration f b  = has f (bindedDeclarationsOf' b)

-- private

has f g = any f . g




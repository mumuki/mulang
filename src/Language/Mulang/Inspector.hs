module Language.Mulang.Inspector (
  hasObject,
  hasAttribute,
  hasMethod,
  hasComposition,
  hasGuards,
  hasIf,
  hasWhile,
  hasLambda,
  hasDirectRecursion,
  hasUsage,
  hasComprehension,
  hasBinding,
  hasFunctionDeclaration,
  hasArity,
  hasTypeDeclaration,
  hasTypeSignature,
  hasAnonymousVariable,
  isOrContainsExpression,
  isOrContainsDeclaration,
  containsBody,
  Inspection,
  ScopedInspection
  ) where

import  Language.Mulang
import  Language.Mulang.Explorer

type ScopedInspection = Binding -> Inspection
type Inspection = Expression  -> Bool

hasObject :: Inspection
hasObject =  isOrContainsExpression f
  where f (VariableDeclaration _ (MuObject _)) = True
        f _  = False

hasAttribute :: ScopedInspection
hasAttribute name =  isOrContainsExpression f
  where f (VariableDeclaration _ (Lambda _ _)) = False
        f (VariableDeclaration n _) = n == name
        f _  = False

hasMethod :: ScopedInspection
hasMethod name =  isOrContainsExpression f
  where f (FunctionDeclaration n _) = n == name
        f (VariableDeclaration n (Lambda _ _)) = n == name
        f _  = False

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
hasComposition :: Inspection
hasComposition = isOrContainsExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = containsBody f
  where f (GuardedBody _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
hasIf :: Inspection
hasIf = isOrContainsExpression f
  where f (If _ _ _) = True
        f _ = False

-- | Inspection that tells whether a binding uses while
-- in its definition
hasWhile :: Inspection
hasWhile = isOrContainsExpression f
  where f (While _ _) = True
        f _ = False


-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = isOrContainsExpression f
  where f (Lambda _ _) = True
        f _ = False


-- | Inspection that tells whether a binding is direct recursive
hasDirectRecursion :: Inspection
hasDirectRecursion = isOrContainsExpression f
  where f e | (Just name) <- (nameOf e) = hasUsage name e
            | otherwise = False

-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
hasUsage :: String -> Inspection
hasUsage target = isOrContainsExpression f
  where f = elem target . map fst .  referencesOf

-- | Inspection that tells whether a binding uses
-- comprehensions - list comprehension, for comprehension, do-syntax, etc -
-- in its definitions
hasComprehension :: Inspection
hasComprehension = isOrContainsExpression f
  where f (Comprehension _ _) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: ScopedInspection
hasBinding = isOrContainsDeclaration (const True)

hasFunctionDeclaration :: Inspection
hasFunctionDeclaration = isOrContainsExpression f
  where f (FunctionDeclaration _ _) = True
        f (VariableDeclaration _ (Lambda _ _)) = True
        f (VariableDeclaration _ (Variable _)) = True -- not actually always true
        f _  = False

hasArity :: Int -> Inspection
hasArity arity = isOrContainsExpression f
  where f (FunctionDeclaration _ equations) = any equationHasArity equations
        f (VariableDeclaration _ (Lambda args _)) = argsHaveArity args
        f _  = False

        equationHasArity = \(Equation args _) -> argsHaveArity args

        argsHaveArity args = length args == arity

hasTypeDeclaration :: Inspection
hasTypeDeclaration = isOrContainsExpression f
  where f (TypeAliasDeclaration _) = True
        f _             = False

hasTypeSignature :: Inspection
hasTypeSignature = isOrContainsExpression f
  where f (TypeSignature _)  = True
        f _                  = False

hasAnonymousVariable :: Inspection
hasAnonymousVariable = isOrContainsExpression f
  where f (FunctionDeclaration _ equations)    = any (any (== WildcardPattern) . p) equations
        f _                                    = False
        p (Equation params _) = params

isOrContainsExpression :: (Expression -> Bool) -> Inspection
isOrContainsExpression f = has f expressionsOf

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodiesOf

isOrContainsDeclaration :: (Expression -> Bool) -> ScopedInspection
isOrContainsDeclaration f name  = has f (bindedDeclarationsOf name)

-- private

has f g = any f . g




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
  hasExpression,
  hasBody,
  Inspection,
  GlobalInspection
  ) where

import  Language.Mulang
import  Language.Mulang.Explorer

type Inspection = Binding -> Expression  -> Bool
type GlobalInspection = Expression  -> Bool

hasObject :: Inspection
hasObject =  hasTopLevelDeclaration f
  where f (VariableDeclaration _ (MuObject _)) = True
        f _  = False

hasAttribute :: Binding -> Inspection
hasAttribute name =  hasExpression f
  where f (VariableDeclaration _ (Lambda _ _)) = False
        f (VariableDeclaration n _) = n == name
        f _  = False

hasMethod :: Binding -> Inspection
hasMethod name =  hasExpression f
  where f (FunctionDeclaration n _) = n == name
        f (VariableDeclaration n (Lambda _ _)) = n == name
        f _  = False

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
hasComposition :: Inspection
hasComposition = hasExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = hasBody f
  where f (GuardedBodies _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
hasIf :: Inspection
hasIf = hasExpression f
  where f (If _ _ _) = True
        f _ = False

-- | Inspection that tells whether a binding uses while
-- in its definition
hasWhile :: Inspection
hasWhile = hasExpression f
  where f (While _ _) = True
        f _ = False


-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = hasExpression f
  where f (Lambda _ _) = True
        f _ = False


-- | Inspection that tells whether a binding is direct recursive
hasDirectRecursion :: Inspection
hasDirectRecursion binding = hasUsage binding binding

-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
hasUsage :: String -> Inspection
hasUsage target = hasExpression f
  where f expr | (Just n) <- expressionToBinding expr = n == target
               | otherwise = False

-- | Inspection that tells whether a binding uses
-- comprehensions - list comprehension, for comprehension, do-syntax, etc -
-- in its definitions
hasComprehension :: Inspection
hasComprehension = hasExpression f
  where f (Comprehension _ _) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: Inspection
hasBinding binding = not.null.declarationsBindedTo binding

hasFunctionDeclaration :: Inspection
hasFunctionDeclaration = hasTopLevelDeclaration f
  where f (FunctionDeclaration _ _) = True
        f (VariableDeclaration _ (Lambda _ _)) = True
        f (VariableDeclaration _ (Variable _)) = True -- not actually always true
        f _  = False

hasArity :: Int -> Inspection
hasArity arity = hasTopLevelDeclaration f
  where f (FunctionDeclaration _ equations) = any equationHasArity equations
        f (VariableDeclaration _ (Lambda args _)) = argsHaveArity args
        f _  = False

        equationHasArity = \(Equation args _) -> argsHaveArity args

        argsHaveArity args = length args == arity

hasTypeDeclaration :: Inspection
hasTypeDeclaration = hasTopLevelDeclaration f
  where f (TypeAliasDeclaration _) = True
        f _             = False

hasTypeSignature :: Inspection
hasTypeSignature = hasTopLevelDeclaration f
  where f (TypeSignature _)  = True
        f _                  = False

hasAnonymousVariable :: Inspection
hasAnonymousVariable = hasTopLevelDeclaration f
  where f (FunctionDeclaration _ equations)    = any (any (== WildcardPattern) . p) equations
        f _                        = False
        p (Equation params _) = params

hasExpression :: (Expression -> Bool) -> Inspection
hasExpression f binding = has f (expressionsOf binding)

hasBody :: (EquationBody -> Bool)-> Inspection
hasBody f binding = has f (rhssOf binding)

hasTopLevelDeclaration :: (Expression -> Bool) -> Inspection
hasTopLevelDeclaration f  = has f . declarationsBindedTo

-- private

has f g = any f . g




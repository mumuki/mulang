module Language.Mulang.Inspector (
  hasObject,
  hasComposition,
  hasGuards,
  hasIf,
  hasConditional,
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
hasObject =  hasDeclaration f
  where f (VariableDeclaration _ (MuObject _)) = True
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

-- | Inspection that tells whether a binding uses ifs or guards
-- in its definition
hasConditional :: Inspection
hasConditional target code = hasIf target code || hasGuards target code

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
hasFunctionDeclaration = hasDeclaration f
  where f (FunctionDeclaration _ _) = True
        f (VariableDeclaration _ (Lambda _ _)) = True
        f (VariableDeclaration _ (Variable _)) = True -- not actually always true
        f _  = False

hasArity :: Int -> Inspection
hasArity arity = hasDeclaration f
  where f (FunctionDeclaration _ equations) = any equationHasArity equations
        f (VariableDeclaration _ (Lambda args _)) = argsHaveArity args
        f _  = False

        equationHasArity = \(Equation args _) -> argsHaveArity args

        argsHaveArity args = length args == arity

hasTypeDeclaration :: Inspection
hasTypeDeclaration = hasDeclaration f
  where f (TypeAliasDeclaration _) = True
        f _             = False

hasTypeSignature :: Inspection
hasTypeSignature = hasDeclaration f
  where f (TypeSignature _)  = True
        f _                  = False

hasAnonymousVariable :: Inspection
hasAnonymousVariable = hasDeclaration f
  where f (FunctionDeclaration _ equations)    = any (any (== WildcardPattern) . p) equations
        f _                        = False
        p (Equation params _) = params

hasExpression :: (Expression -> Bool) -> Inspection
hasExpression f binding = has f (expressionsOf binding)

hasBody :: (EquationBody -> Bool)-> Inspection
hasBody f binding = has f (rhssOf binding)

hasDeclaration :: (Expression -> Bool) -> Inspection
hasDeclaration f  = has f . declarationsBindedTo

-- private

has f g = any f . g




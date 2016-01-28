module Language.Mulang.Inspector (
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
  hasDecl,
  hasRhs,
  Inspection,
  GlobalInspection
  ) where

import  Language.Mulang
import  Language.Mulang.Explorer

type Inspection = Binding -> Expression  -> Bool
type GlobalInspection = Expression  -> Bool

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
hasComposition :: Inspection
hasComposition = hasExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = hasRhs f
  where f (GuardedRhss _) = True
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
hasFunctionDeclaration funBinding = has f (declarationsBindedTo funBinding)
  where f (FunctionDeclaration _ _) = True
        f (ConstantDeclaration _ (Lambda _ _)) = True
        f (ConstantDeclaration _ (Variable _)) = True -- not actually always true
        f _  = False

hasArity :: Int -> Inspection
hasArity arity funBinding = has f (declarationsBindedTo funBinding)
  where f (FunctionDeclaration _ equations) = any equationHasArity equations
        f (ConstantDeclaration _ (Lambda args _)) = argsHaveArity args
        f _  = False

        equationHasArity = \(Equation args _) -> argsHaveArity args

        argsHaveArity args = length args == arity

hasTypeDeclaration :: Inspection
hasTypeDeclaration binding = hasDecl f
  where f (TypeAlias name) = binding == name
        f _                   = False

hasTypeSignature :: Inspection
hasTypeSignature binding = hasDecl f
  where f (TypeSignature name)  = binding == name
        f _                       = False

hasAnonymousVariable :: Inspection
hasAnonymousVariable binding = has f (declarationsBindedTo binding)
  where f (FunctionDeclaration _ equations)    = any (any (== WildcardPattern) . p) equations
        f _                        = False
        p (Equation params _) = params

hasExpression :: (Expression -> Bool) -> Inspection
hasExpression f binding = has f (expressionsOf binding)

hasRhs :: (Rhs -> Bool)-> Inspection
hasRhs f binding = has f (rhssOf binding)

hasDecl :: (Declaration -> Bool) -> GlobalInspection
hasDecl f = has f topLevelDeclarations

-- private

has f g = any f . g




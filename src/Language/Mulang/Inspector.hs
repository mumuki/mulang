module Language.Mulang.Inspector (
  declaresObject,
  declaresAttribute,
  declaresMethod,
  usesComposition,
  usesGuards,
  usesIf,
  usesWhile,
  usesLambda,
  usesNot,
  usesForall,
  declaresRecursively,
  uses,
  usesComprehension,
  declares,
  declaresRule,
  declaresFact,
  declaresFunction,
  declaresWithArity,
  declaresTypeAlias,
  declaresTypeSignature,
  usesAnnonymousVariable,
  containsExpression,
  containsDeclaration,
  containsBody,
  named,
  like,
  anyone,
  Inspection) where

import  Language.Mulang
import  Language.Mulang.Explorer
import  Data.List (isInfixOf)
type Inspection = Expression  -> Bool

named :: String -> BindingPredicate
named = (==)

like :: String -> BindingPredicate
like = isInfixOf

anyone :: BindingPredicate
anyone = const True

declaresFact :: BindingPredicate -> Inspection
declaresFact = containsDeclaration f
  where f (FactDeclaration _ _) = True
        f _                     = False

declaresRule :: BindingPredicate -> Inspection
declaresRule = containsDeclaration f
  where f (RuleDeclaration _ _ _) = True
        f _                       = False

declaresObject :: BindingPredicate -> Inspection
declaresObject =  containsDeclaration f
  where f (ObjectDeclaration _ _) = True
        f _                       = False

declaresAttribute :: BindingPredicate -> Inspection
declaresAttribute =  containsDeclaration f
  where f (AttributeDeclaration _ _) = True
        f _                          = False

declaresMethod :: BindingPredicate -> Inspection
declaresMethod =  containsDeclaration f
  where f (MethodDeclaration _ _) = True
        f _                       = False

-- | Inspection that tells whether a binding is direct recursive
declaresRecursively :: BindingPredicate -> Inspection
declaresRecursively = containsDeclaration f
  where f e | (Just name) <- (nameOf e) = uses (named name) e
            | otherwise = False

-- | Inspection that tells whether a top level binding exists
declares :: BindingPredicate -> Inspection
declares = containsDeclaration (const True)

declaresFunction :: BindingPredicate -> Inspection
declaresFunction = containsDeclaration f
  where f (FunctionDeclaration _ _) = True
        f _  = False

declaresWithArity :: Int -> BindingPredicate -> Inspection
declaresWithArity arity = containsDeclaration f
  where f (FunctionDeclaration _ equations)  = any equationArityIs equations
        f (ProcedureDeclaration _ equations) = any equationArityIs equations
        f (ProcedureDeclaration _ equations) = any equationArityIs equations
        f (MethodDeclaration _ equations)    = any equationArityIs equations
        f (RuleDeclaration _ args _)         = argsHaveArity args
        f (FactDeclaration _ args)           = argsHaveArity args
        f _  = False

        equationArityIs = \(Equation args _) -> argsHaveArity args

        argsHaveArity args = length args == arity

declaresTypeAlias :: BindingPredicate -> Inspection
declaresTypeAlias = containsDeclaration f
  where f (TypeAliasDeclaration _) = True
        f _             = False

declaresTypeSignature :: BindingPredicate -> Inspection
declaresTypeSignature = containsDeclaration f
  where f (TypeSignature _)  = True
        f _                  = False

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
usesComposition :: Inspection
usesComposition = containsExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
usesGuards :: Inspection
usesGuards = containsBody f
  where f (GuardedBody _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
usesIf :: Inspection
usesIf = containsExpression f
  where f (If _ _ _) = True
        f _ = False

-- | Inspection that tells whether a binding uses while
-- in its definition
usesWhile :: Inspection
usesWhile = containsExpression f
  where f (While _ _) = True
        f _ = False


-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
usesLambda :: Inspection
usesLambda = containsExpression f
  where f (Lambda _ _) = True
        f _ = False


usesNot :: Inspection
usesNot = containsExpression f
  where f (Not  _) = True
        f _ = False

usesForall :: Inspection
usesForall = containsExpression f
  where f (Forall  _ _) = True
        f _ = False


-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
uses :: BindingPredicate -> Inspection
uses p = containsExpression f
  where f = any p . map fst .  referencesOf

-- | Inspection that tells whether a binding uses
-- comprehensions - list comprehension, for comprehension, do-syntax, etc -
-- in its definitions
usesComprehension :: Inspection
usesComprehension = containsExpression f
  where f (Comprehension _ _) = True
        f _ = False

usesAnnonymousVariable :: Inspection
usesAnnonymousVariable = containsExpression f
  where f (FunctionDeclaration _ equations)    = equationContainsWildcard equations
        f (ProcedureDeclaration _ equations)   = equationContainsWildcard equations
        f (MethodDeclaration _ equations)      = equationContainsWildcard equations
--TODO        f (Lambda args _)                      = equationContainsWildcard equations
        f (FactDeclaration _ args)             = containsWildcard args
        f (RuleDeclaration _ args _)           = containsWildcard args
        f _                                    = False
        equationContainsWildcard = any (containsWildcard . p)
        containsWildcard = any (== WildcardPattern)
        p (Equation params _) = params

containsExpression :: (Expression -> Bool) -> Inspection
containsExpression f = has f expressionsOf

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodiesOf

containsDeclaration :: (Expression -> Bool) -> BindingPredicate -> Inspection
containsDeclaration f b  = has f (bindedDeclarationsOf' b)

-- private

has f g = any f . g




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
  usesFindall,
  usesForall,
  usesRepeat,
  usesPatternMatching,
  usesUnifyOperator,
  declaresRecursively,
  parses,
  uses,
  usesComprehension,
  declares,
  declaresRule,
  declaresFact,
  declaresFunction,
  declaresComputationWithArity,
  declaresComputationWithExactArity,
  declaresComputation,
  declaresTypeAlias,
  declaresTypeSignature,
  usesAnonymousVariable,
  containsExpression,
  containsDeclaration,
  containsBody,
  named,
  like,
  anyone,
  Inspection) where

import  Language.Mulang
import  Language.Mulang.Binding
import  Language.Mulang.Explorer

type Inspection = Expression  -> Bool

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

-- | Inspection that tells whether an expression is direct recursive
declaresRecursively :: BindingPredicate -> Inspection
declaresRecursively = containsDeclaration f
  where f e | (Just name) <- (nameOf e) = uses (named name) e
            | otherwise = False

-- | Inspection that tells whether a top level binding exists
declares :: BindingPredicate -> Inspection
declares = containsDeclaration f
  where f (TypeSignature _) = False
        f _                 = True

declaresFunction :: BindingPredicate -> Inspection
declaresFunction = containsDeclaration f
  where f (FunctionDeclaration _ _) = True
        f _  = False
-- | Inspection that tells whether a top level computation binding exists
declaresComputation :: BindingPredicate -> Inspection
declaresComputation = declaresComputationWithArity (const True)

declaresComputationWithExactArity :: Int -> BindingPredicate -> Inspection
declaresComputationWithExactArity arity = declaresComputationWithArity (== arity)

declaresComputationWithArity :: (Int -> Bool) -> BindingPredicate -> Inspection
declaresComputationWithArity arityPredicate = containsDeclaration f
  where f (FunctionDeclaration _ equations)  = any equationArityIs equations
        f (ProcedureDeclaration _ equations) = any equationArityIs equations
        f (MethodDeclaration _ equations)    = any equationArityIs equations
        f (RuleDeclaration _ args _)         = argsHaveArity args
        f (FactDeclaration _ args)           = argsHaveArity args
        f _  = False

        equationArityIs = \(Equation args _) -> argsHaveArity args

        argsHaveArity = arityPredicate.length

declaresTypeAlias :: BindingPredicate -> Inspection
declaresTypeAlias = containsDeclaration f
  where f (TypeAliasDeclaration _) = True
        f _             = False

declaresTypeSignature :: BindingPredicate -> Inspection
declaresTypeSignature = containsDeclaration f
  where f (TypeSignature _)  = True
        f _                  = False

-- | Inspection that tells whether an expression uses the composition operator '.'
-- in its definition
usesComposition :: Inspection
usesComposition = containsExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether an expression uses guards
-- in its definition
usesGuards :: Inspection
usesGuards = containsBody f
  where f (GuardedBody _) = True
        f _ = False

-- | Inspection that tells whether an expression uses ifs
-- in its definition
usesIf :: Inspection
usesIf = containsExpression f
  where f (If _ _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses while
-- in its definition
usesWhile :: Inspection
usesWhile = containsExpression f
  where f (While _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses pattern matching
-- in its definition
usesPatternMatching :: Inspection
usesPatternMatching = containsExpression f
  where f (FunctionDeclaration _ equations) = any nonVariablePattern (patterns equations)
        f _ = False

        patterns = concatMap (\(Equation ps _) -> ps)

        nonVariablePattern :: Pattern -> Bool
        nonVariablePattern (VariablePattern _) = False
        nonVariablePattern _                   = True



-- | Inspection that tells whether an expression uses reoeat
-- in its definition
usesRepeat :: Inspection
usesRepeat = containsExpression f
  where f (Repeat _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses a lambda expression
-- in its definition
usesLambda :: Inspection
usesLambda = containsExpression f
  where f (Lambda _ _) = True
        f _ = False


usesNot :: Inspection
usesNot = containsExpression f
  where f (Not  _) = True
        f _ = False

usesFindall :: Inspection
usesFindall = containsExpression f
  where f (Findall  _ _ _) = True
        f _ = False

usesForall :: Inspection
usesForall = containsExpression f
  where f (Forall  _ _) = True
        f _ = False

usesUnifyOperator :: Inspection
usesUnifyOperator = containsExpression f
  where f (Exist "=" _) = True
        f _ = False


-- | Inspection that tells whether an expression is equal to a given piece of code after being parsed
parses :: (String -> Expression) -> String -> Inspection
parses parser code = (== (parser code))

-- | Inspection that tells whether an expression uses the the given target binding
-- in its definition
uses :: BindingPredicate -> Inspection
uses p = containsExpression f
  where f = any p . map fst .  referencesOf

-- | Inspection that tells whether an expression uses
-- comprehensions - list comprehension, for comprehension, do-syntax, etc -
-- in its definitions
usesComprehension :: Inspection
usesComprehension = containsExpression f
  where f (Comprehension _ _) = True
        f _ = False

usesAnonymousVariable :: Inspection
usesAnonymousVariable = containsExpression f
  where f (FunctionDeclaration _ equations)    = equationContainsWildcard equations
        f (ProcedureDeclaration _ equations)   = equationContainsWildcard equations
        f (MethodDeclaration _ equations)      = equationContainsWildcard equations
--TODO        f (Lambda args _)                      = equationContainsWildcard equations
        f (FactDeclaration _ params)             = paramsContainsWildcard params
        f (RuleDeclaration _ params _)           = paramsContainsWildcard params
        f _                                    = False

        equationContainsWildcard = any (paramsContainsWildcard . equationParams)
        paramsContainsWildcard = any isOrContainsWildcard

        equationParams (Equation p _) = p

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




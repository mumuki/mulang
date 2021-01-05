module Language.Mulang.Generator (
  boundDeclarations,
  boundDeclarators,
  declarators,
  declarations,
  declarationsOf,
  declaredIdentifiers,
  mainDeclaredIdentifiers,
  equationBodies,
  expressions,
  referencedIdentifiers,
  identifierReferences,
  transitiveReferencedIdentifiers,
  equationsExpressions,
  statementsExpressions,
  equationsExpandedExpressions,
  Generator,
  Declarator,
  Expression(..)) where

import Language.Mulang.Ast
import Language.Mulang.Identifier

import Data.Maybe (mapMaybe)
import Data.List (nub)

type Generator a = Expression -> [a]
type Declarator = (Identifier, Expression)

-- | Returns all the declarations their identifiers -
-- | classes, methods, functions, records, local and global variables, and so on
-- |
-- | For example, in 'f x = g x where x = y', it returns '(f, f x = ...)' and '(x, x = y)'
declarators :: Generator Declarator
declarators (Sequence es)          = concatMap declarators es
declarators e@(Attribute n _)      = [(n, e)]
declarators e@(Class n _ b)        = (n, e) : declarators b
declarators e@(Clause n _ es)      = (n, e) : concatMap declarators es
declarators e@(Enumeration n _)    = [(n, e)]
declarators e@(Interface n _ b)    = (n, e) : declarators b
declarators e@(EntryPoint n b)     = (n, e) : declarators b
declarators e@(Subroutine n b)     = (n, e) : concatMap declarators (equationsExpressions b)
declarators e@(Object n b)         = (n, e) : declarators b
declarators e@(Clause n _ _)       = [(n, e)]
declarators e@(Record n)           = [(n, e)]
declarators e@(TypeAlias n _)      = [(n, e)]
declarators e@(TypeSignature n _)  = [(n, e)]
declarators e@(LValue n _)         = [(n, e)]
declarators (Other _ (Just e))     = declarators e
declarators _                      = []

declarations :: Generator Expression
declarations = map snd . declarators

-- | Returns all declarations bound to the given identifier predicate
-- |
boundDeclarations :: IdentifierPredicate -> Generator Expression
boundDeclarations f = map snd . boundDeclarators f

-- | Returns all declarators bound to the given identifier predicate
-- |
boundDeclarators :: IdentifierPredicate -> Generator Declarator
boundDeclarators f = filter (f.fst) . declarators

-- | Returns the given expression and all its subexpressions
-- For example: in 'f x = x + 1', it returns 'f x = x + 1', 'x + 1', 'x' and '1'
expressions :: Generator Expression
expressions expr = expr : concatMap expressions (subExpressions expr)
  where
    subExpressions :: Generator Expression
    subExpressions (Arrow e1 e2)           = [e1, e2]
    subExpressions (Assignment _ e)        = [e]
    subExpressions (Attribute _ v)         = [v]
    subExpressions (Call op args)          = op:args
    subExpressions (Class _ _ v)           = [v]
    subExpressions (Clause _ _ es)         = es
    subExpressions (EntryPoint _ e)        = [e]
    subExpressions (FieldAssignment r _ e) = [r, e]
    subExpressions (FieldReference r _)    = [r]
    subExpressions (For stmts a)           = statementsExpressions stmts ++ [a]
    subExpressions (Forall e1 e2)          = [e1, e2]
    subExpressions (ForLoop i c p s)       = [i, c, p, s]
    subExpressions (If a b c)              = [a, b, c]
    subExpressions (Implement e)           = [e]
    subExpressions (Include e)             = [e]
    subExpressions (Interface _ _ v)       = [v]
    subExpressions (Lambda _ a)            = [a]
    subExpressions (Match e1 equations)    = e1:equationsExpressions equations
    subExpressions (MuDict es)             = [es]
    subExpressions (MuList as)             = as
    subExpressions (MuObject es)           = [es]
    subExpressions (MuTuple as)            = as
    subExpressions (New e es)              = e:es
    subExpressions (Not e)                 = [e]
    subExpressions (Object _ v)            = [v]
    subExpressions (Other _ (Just e))      = [e]
    subExpressions (Print v)               = [v]
    subExpressions (Repeat e1 e2)          = [e1, e2]
    subExpressions (Return v)              = [v]
    subExpressions (Sequence es)           = es
    subExpressions (Subroutine _ es)       = equationsExpressions es
    subExpressions (Switch e1 list _)      = e1 : concatMap (\(x,y) -> [x,y]) list
    subExpressions (Try t cs f)            = t : map snd cs ++ [f]
    subExpressions (TypeCast e _)          = [e]
    subExpressions (Variable _ v)          = [v]
    subExpressions (Constant _ v)          = [v]
    subExpressions (While e1 e2)           = [e1, e2]
    subExpressions (Yield v)               = [v]
    subExpressions (Break e)               = [e]
    subExpressions (Continue e)            = [e]
    subExpressions _                       = []


-- | Returns all the referenced identifiers
-- For example, in 'f (x + 1)', it returns 'f' and 'x'
referencedIdentifiers :: Generator Identifier
referencedIdentifiers = nub . identifierReferences

identifierReferences :: Generator Identifier
identifierReferences = mapMaybe extractReference . expressions


-- | Returns all the identifiers transitively referenced by the given one
-- |
transitiveReferencedIdentifiers :: Identifier -> Generator Identifier
transitiveReferencedIdentifiers identifier code =  expand (concatMap referencedIdentifiers . (`declarationsOf` code)) identifier
  where
    expand :: Eq a => (a-> [a]) -> a -> [a]
    expand f x = expand' [] f [x]

    expand' _ _ [] = []
    expand' ps f (x:xs) | elem x ps = expand' ps f xs
                        | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


-- | Returns all the declared identifiers
-- For example, in 'f x = g x where x = y', it returns 'f' and 'x'
declaredIdentifiers :: Generator Identifier
declaredIdentifiers = map fst . declarators

mainDeclaredIdentifiers :: Generator Identifier
mainDeclaredIdentifiers (Sequence _) = []
mainDeclaredIdentifiers expression   = take 1 . declaredIdentifiers $ expression

-- | Returns all the body equations of functions, procedures and methods
equationBodies :: Generator EquationBody
equationBodies = concatMap bodiesOf . declarations
  where
    bodiesOf :: Generator EquationBody
    bodiesOf (Subroutine  _ equations) = equationBodies equations
    bodiesOf _ = []

    equationBodies = map (\(Equation _ body) -> body)

equationsExpandedExpressions :: [Equation] -> [Expression]
equationsExpandedExpressions = concatMap expand . equationsExpressions
  where
    expand (Sequence xs) = xs
    expand other         = [other]

declarationsOf :: Identifier -> Generator Expression
declarationsOf b = boundDeclarations (named b)

extractReference :: Expression -> Maybe Identifier
extractReference (Reference n)        = Just n
extractReference (FieldReference _ n) = Just n
extractReference (Exist n _)          = Just n
extractReference _                    = Nothing

equationsExpressions :: [Equation] -> [Expression]
equationsExpressions = concatMap (\(Equation _ body) -> bodyExpressions body)
  where
    bodyExpressions (UnguardedBody e)      = [e]
    bodyExpressions (GuardedBody b)        = b >>= \(es1, es2) -> [es1, es2]

statementsExpressions :: [Statement] -> [Expression]
statementsExpressions = map expression
  where
    expression (Generator _ e) = e
    expression (Guard e)       = e

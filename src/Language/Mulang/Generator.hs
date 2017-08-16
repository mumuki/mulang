module Language.Mulang.Generator (
  boundDeclarations,
  declarations,
  declarationsOf,
  declaredIdentifiers,
  mainDeclaredIdentifiers,
  equationBodies,
  expressions,
  referencedIdentifiers,
  identifierReferences,
  transitiveReferencedIdentifiers,
  Generator,
  Expression(..)) where

import Language.Mulang.Ast
import Language.Mulang.Identifier

import Data.Maybe (mapMaybe)
import Data.List (nub)

type Generator a = Expression -> [a]

-- | Returns all the declarations their identifiers -
-- | classes, methods, functions, records, local and global variables, and so on
-- |
-- | For example, in 'f x = g x where x = y', it returns '(f, f x = ...)' and '(x, x = y)'
declarations :: Generator (Identifier, Expression)
declarations (Sequence es)          = concatMap declarations es
declarations e@(Attribute n _)      = [(n, e)]
declarations e@(Class n _ b)        = (n, e) : declarations b
declarations e@(Clause n _ es)      = (n, e) : concatMap declarations es
declarations e@(Enumeration n _)    = [(n, e)]
declarations e@(Interface n _ b)    = (n, e) : declarations b
declarations e@(EntryPoint n b)     = (n, e) : declarations b
declarations e@(Subroutine n b)     = (n, e) : concatMap declarations (equationExpressions b)
declarations e@(Object n b)         = (n, e) : declarations b
declarations e@(Clause n _ _)       = [(n, e)]
declarations e@(Record n)           = [(n, e)]
declarations e@(TypeAlias n _)      = [(n, e)]
declarations e@(TypeSignature n _)  = [(n, e)]
declarations e@(Variable n _)       = [(n, e)]
declarations _                      = []

-- | Returns all declarations bound to the given identifier predicate
-- |
boundDeclarations :: IdentifierPredicate -> Generator Expression
boundDeclarations f = map snd . filter (f.fst) . declarations

-- | Returns the given expression and all its subexpressions
-- For example: in 'f x = x + 1', it returns 'f x = x + 1', 'x + 1', 'x' and '1'
expressions :: Generator Expression
expressions expr = expr : concatMap expressions (subExpressions expr)
  where
    subExpressions :: Generator Expression
    subExpressions (Assignment _ e)        = [e]
    subExpressions (Attribute _ v)         = [v]
    subExpressions (Call op args)          = op:args
    subExpressions (Class _ _ v)           = [v]
    subExpressions (Clause _ _ es)         = es
    subExpressions (EntryPoint _ e)        = [e]
    subExpressions (For stmts a)           = statementExpressions stmts ++ [a]
    subExpressions (Forall e1 e2)          = [e1, e2]
    subExpressions (ForLoop i c p s)       = [i, c, p, s]
    subExpressions (If a b c)              = [a, b, c]
    subExpressions (Implement e)           = [e]
    subExpressions (Include e)             = [e]
    subExpressions (Interface _ _ v)       = [v]
    subExpressions (Lambda _ a)            = [a]
    subExpressions (Match e1 equations)    = e1:equationExpressions equations
    subExpressions (MuList as)             = as
    subExpressions (MuObject es)           = [es]
    subExpressions (MuTuple as)            = as
    subExpressions (New e es)              = e:es
    subExpressions (Not e)                 = [e]
    subExpressions (Object _ v)            = [v]
    subExpressions (Other _ (Just e))      = [e]
    subExpressions (Repeat e1 e2)          = [e1, e2]
    subExpressions (Return v)              = [v]
    subExpressions (Sequence es)           = es
    subExpressions (Subroutine _ es)       = equationExpressions es
    subExpressions (Switch e1 list _)      = e1 : concatMap (\(x,y) -> [x,y]) list
    subExpressions (Try t cs f)            = t : map snd cs ++ [f]
    subExpressions (TypeCast e _)          = [e]
    subExpressions (Variable _ v)          = [v]
    subExpressions (While e1 e2)           = [e1, e2]
    subExpressions (Yield v)               = [v]
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
declaredIdentifiers = map fst . declarations

mainDeclaredIdentifiers :: Generator Identifier
mainDeclaredIdentifiers (Sequence _) = []
mainDeclaredIdentifiers expression   = take 1 . declaredIdentifiers $ expression

-- | Returns all the body equations of functions, procedures and methods
equationBodies :: Generator EquationBody
equationBodies = concatMap (bodiesOf . snd) . declarations
  where
    bodiesOf :: Generator EquationBody
    bodiesOf (Subroutine  _ equations) = equationBodies equations
    bodiesOf _ = []

    equationBodies = map (\(Equation _ body) -> body)

declarationsOf :: Identifier -> Generator Expression
declarationsOf b = boundDeclarations (named b)

extractReference :: Expression -> Maybe Identifier
extractReference (Reference n)        = Just n
extractReference (Exist n _)          = Just n
extractReference _                    = Nothing

equationExpressions = concatMap (\(Equation _ body) -> bodyExpressions body)
  where
    bodyExpressions (UnguardedBody e)      = [e]
    bodyExpressions (GuardedBody b)        = b >>= \(es1, es2) -> [es1, es2]

statementExpressions = map expression
  where
    expression (Generator _ e) = e
    expression (Guard e)       = e

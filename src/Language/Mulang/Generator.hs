module Language.Mulang.Generator (
  equationBodiesOf,
  declarationsOf,
  referencedIdentifiersOf,
  declaredIdentifiersOf,
  boundDeclarationsOf,
  boundDeclarationsOf',
  transitiveReferencedIdentifiersOf,
  nameOf,
  allExpressions,
  mainExpressions,
  Unfold,
  Generator,
  Expression(..)) where

import Language.Mulang.Ast
import Language.Mulang.Identifier

import Data.Maybe (mapMaybe)
import Data.List (nub)

type Generator a = Expression -> [a]
type Unfold = Generator Expression

{-
* declarariones (clases, métodos, funciones, estructuras, variables globales y locales)
  * Generator (Identifier, Expression)
  * declarations
-}

-- | Returns all the declared identifiers and the expressions that binds them
-- For example, in 'f x = g x where x = y', it returns '(f, f x = ...)' and '(x, x = y)'
declarationsOf :: Generator (Identifier, Expression)
declarationsOf = mapMaybe extractDeclaration .  mainExpressions

{-
* declarariones enlazadas (idem anterior)
  * IdentifierPredicate -> Generator Expression
  * boundDeclarations
-}

boundDeclarationsOf' :: IdentifierPredicate -> Unfold
boundDeclarationsOf' f = map snd . filter (f.fst) . declarationsOf

{-
* todas las expresiones (idem anterior + otras expresiones)
  * Generator Expression
  * expressions

-}

-- | Returns the given expressions and all its subexpressions
-- For example: in 'f x = x + 1', it returns 'f x = x + 1', 'x + 1', 'x' and '1'
allExpressions :: Unfold
allExpressions expr = expr : concatMap allExpressions (subExpressions expr)
  where
    subExpressions :: Unfold
    subExpressions (Variable _ v)          = [v]
    subExpressions (Subroutine _ es)       = equationExpressions es
    subExpressions (Clause _ _ es)         = es
    subExpressions (Attribute _ v)         = [v]
    subExpressions (Object _ v)            = [v]
    subExpressions (Class _ _ v)           = [v]
    subExpressions (Interface _ _ v)       = [v]
    subExpressions (EntryPoint _ e)        = [e]
    subExpressions (Call op args)          = op:args
    subExpressions (Lambda _ a)            = [a]
    subExpressions (If a b c)              = [a, b, c]
    subExpressions (While e1 e2)           = [e1, e2]
    subExpressions (Repeat e1 e2)          = [e1, e2]
    subExpressions (Switch e1 list)        = e1 : concatMap (\(x,y) -> [x,y]) list
    subExpressions (Match e1 equations)    = e1:equationExpressions equations
    subExpressions (Comprehension a _)     = [a] --TODO
    subExpressions (Not e)                 = [e]
    subExpressions (Forall e1 e2)          = [e1, e2]
    subExpressions (Return v)              = [v]
    subExpressions (Sequence es)           = es
    subExpressions (MuObject es)           = [es]
    subExpressions (MuTuple as)            = as
    subExpressions (MuList as)             = as
    subExpressions _                       = []


{-

* todos los identificadores referenciados
  * Generator Identifier
  * referencedIdentifiers

-}

-- | Returns all the referenced identifiers
-- For example, in 'f (x + 1)', it returns 'f' and 'x'
referencedIdentifiersOf :: Generator Identifier
referencedIdentifiersOf = nub . mapMaybe extractReference . allExpressions

{-

* todos los identificadores referenciados transitivamente
  * Identifier -> Generator Identifier
  * transitiveReferencedIdentifiers
-}

transitiveReferencedIdentifiersOf :: Identifier -> Generator Identifier
transitiveReferencedIdentifiersOf identifier code =  expand (concatMap referencedIdentifiersOf . (`boundDeclarationsOf` code)) identifier
  where
    expand :: Eq a => (a-> [a]) -> a -> [a]
    expand f x = expand' [] f [x]

    expand' _ _ [] = []
    expand' ps f (x:xs) | elem x ps = expand' ps f xs
                        | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


{-

* todos los identificadores declarados
  * Generator Identifier
  * declaredIdentifiers
-}

-- | Returns all the declared identifiers
-- For example, in 'f x = g x where x = y', it returns 'f' and 'x'
declaredIdentifiersOf :: Generator Identifier
declaredIdentifiersOf = map fst . declarationsOf

{-
* cuerpos de ecuaciones
  * Generator EquationBody
  * equationBodies
-}


-- | Returns all the body equations of functions, procedures and methods
equationBodiesOf :: Generator EquationBody
equationBodiesOf = concatMap bodiesOf . mainExpressions
  where
    bodiesOf :: Generator EquationBody
    bodiesOf (Subroutine  _ equations) = equationBodies equations
    bodiesOf _ = []

    equationBodies = map (\(Equation _ body) -> body)


-------------------------------------


boundDeclarationsOf :: Identifier -> Unfold
boundDeclarationsOf b = boundDeclarationsOf' (==b)


nameOf :: Expression -> Maybe Identifier
nameOf = fmap fst . extractDeclaration

extractReference :: Expression -> Maybe Identifier
extractReference (Reference n)        = Just n
extractReference (Exist n _)          = Just n
extractReference _                    = Nothing


extractDeclaration :: Expression -> Maybe (Identifier, Expression)
extractDeclaration e@(TypeSignature n _ _)= Just (n, e)
extractDeclaration e@(TypeAlias n )       = Just (n, e)
extractDeclaration e@(Variable n _)       = Just (n, e)
extractDeclaration e@(Subroutine n _)     = Just (n, e)
extractDeclaration e@(Record n)           = Just (n, e)
extractDeclaration e@(Clause n _ _)       = Just (n, e)
extractDeclaration e@(Object n _)         = Just (n, e)
extractDeclaration e@(Class n _ _)        = Just (n, e)
extractDeclaration e@(Interface n _ _)    = Just (n, e)
extractDeclaration e@(Enumeration n _)    = Just (n, e)
extractDeclaration e@(Attribute n _)      = Just (n, e)
extractDeclaration e@(EntryPoint n _)     = Just (n, e)
extractDeclaration _                      = Nothing


mainExpressions :: Unfold
mainExpressions (Sequence es)          = concatMap mainExpressions es
mainExpressions a@(Attribute _ _)      = [a]
mainExpressions c@(Class _ _ b)        = c : mainExpressions b
mainExpressions c@(Clause _ _ es)      = c : concatMap mainExpressions es
mainExpressions c@(Enumeration _ _)    = [c]
mainExpressions c@(Interface _ _ b)    = c : mainExpressions b
mainExpressions e@(EntryPoint _ b)     = e : mainExpressions b
mainExpressions e@(Subroutine _ b)     = e : concatMap mainExpressions (equationExpressions b)
mainExpressions o@(Object _ b)         = o : mainExpressions b
mainExpressions r@(Clause _ _ _)       = [r]
mainExpressions r@(Record _)           = [r]
mainExpressions t@(TypeAlias _ )       = [t]
mainExpressions t@(TypeSignature _ _ _)= [t]
mainExpressions v@(Variable _ _)       = [v]
mainExpressions _                      = []


equationExpressions = concatMap (\(Equation _ body) -> bodyExpressions body)
  where
    bodyExpressions (UnguardedBody e)      = [e]
    bodyExpressions (GuardedBody b)        = b >>= \(es1, es2) -> [es1, es2]

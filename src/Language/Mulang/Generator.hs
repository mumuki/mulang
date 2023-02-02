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
import Language.Mulang.Ast.Visitor
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
declarators (Decorator _ e)        = declarators e
declarators (EigenClass _ b)       = declarators b
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
    --
    subExpressions (Assert _ _)                           = [] -- FIXME
    subExpressions (For stmts a)                          = statementsExpressions stmts ++ [a]
    subExpressions (ForLoop i c p s)                      = [i, c, p, s]
    subExpressions (Lambda _ e)                           = [e]
    subExpressions (Match e eqs)                          = e : equationsExpressions eqs
    subExpressions (Rule _ _ es)                          = es
    subExpressions (Send e1 e2 es)                        = e1 : e2 : es
    subExpressions (Switch e1 list e2)                    = e1 : concatMap (\(x,y) -> [x,y]) list ++ [e2]
    subExpressions (Try t cs f)                           = t : map snd cs ++ [f]
    --
    subExpressions (ExpressionAndExpressionsList e es _)  = e : es
    subExpressions (SingleEquationsList eqs _)            = equationsExpressions eqs
    subExpressions (SingleExpression e _)                 = [e]
    subExpressions (SingleExpressionsList es _)           = es
    subExpressions (SinglePatternsList _ _)               = []
    subExpressions (Terminal)                             = []
    subExpressions (ThreeExpressions e1 e2 e3 _)          = [e1, e2, e3]
    subExpressions (TwoExpressions e1 e2 _)               = [e1, e2]


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
equationsExpressions = concatMap (\(Equation params body) -> paramsExpression params ++ bodyExpressions body)
  where
    bodyExpressions (UnguardedBody e)      = [e]
    bodyExpressions (GuardedBody b)        = b >>= \(es1, es2) -> [es1, es2]

    paramsExpression ((DefaultPattern _ e):ps) = e:paramsExpression ps
    paramsExpression (_:ps)                    = paramsExpression ps
    paramsExpression _                         = []

statementsExpressions :: [Statement] -> [Expression]
statementsExpressions = map expression
  where
    expression (Generator _ e) = e
    expression (Guard e)       = e

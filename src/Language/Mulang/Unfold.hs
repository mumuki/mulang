module Language.Mulang.Unfold (
  allExpressions,
  mainExpressions,
  Unfold) where

import Language.Mulang.Ast

type Unfold = Expression -> [Expression]

-- | Returns the given expressions and all its subexpressions
-- For example: in 'f x = x + 1', it returns 'f x = x + 1', 'x + 1', 'x' and '1'
allExpressions :: Unfold
allExpressions expr = expr : concatMap allExpressions (subExpressions expr)
  where
    subExpressions :: Unfold
    subExpressions (Variable _ v)          = [v]
    subExpressions (Subroutine _ es)       = expressionsOfEquations es
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
    subExpressions (Match e1 equations)    = e1:expressionsOfEquations equations
    subExpressions (Comprehension a _)     = [a] --TODO
    subExpressions (Not e)                 = [e]
    subExpressions (Forall e1 e2)          = [e1, e2]
    subExpressions (Return v)              = [v]
    subExpressions (Sequence es)           = es
    subExpressions (MuObject es)           = [es]
    subExpressions (MuTuple as)            = as
    subExpressions (MuList as)             = as
    subExpressions _                       = []

    expressionsOfEquations eqs = eqs >>= \(Equation _ body) -> topExpressionOfBody body
    topExpressionOfBody (UnguardedBody e)      = [e]
    topExpressionOfBody (GuardedBody b)        = b >>= \(es1, es2) -> [es1, es2]

mainExpressions :: Unfold
mainExpressions o@(Object _ b)         = o : mainExpressions b
mainExpressions c@(Class _ _ b)        = c : mainExpressions b
mainExpressions c@(Interface _ _ b)    = c : mainExpressions b
mainExpressions e@(EntryPoint _ b)     = e : mainExpressions b
mainExpressions t@(TypeSignature _ _ _)= [t]
mainExpressions t@(TypeAlias _ )       = [t]
mainExpressions r@(Record _)           = [r]
mainExpressions v@(Variable _ _)       = [v]
mainExpressions e@(Subroutine _ _)     = [e]
mainExpressions r@(Clause _ _ _)       = [r]
mainExpressions a@(Attribute _ _)      = [a]
mainExpressions (Sequence es)          = concatMap mainExpressions es
mainExpressions _                      = []



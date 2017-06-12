module Language.Mulang.Explorer (
  (//),
  expressionsOf,
  equationBodiesOf,
  referencesOf,
  declarationsOf,
  referencedBindingsOf,
  declaredBindingsOf,
  bindedDeclarationsOf,
  bindedDeclarationsOf',
  transitiveReferencedBindingsOf,
  nameOf,
  extractDeclaration,
  extractReference,
  Expression(..)) where

import Language.Mulang.Ast
import Language.Mulang.Binding

import Data.Maybe (maybeToList)
import Data.List (nub)

(//)  :: Expression -> Binding -> [Expression]
(//) = flip bindedDeclarationsOf

expressionsOf :: Expression -> [Expression]
expressionsOf expr = expr : concatMap expressionsOf (subExpressions expr)
  where
    subExpressions :: Expression -> [Expression]
    subExpressions (Variable _ v)          = [v]
    subExpressions (Function _ equations)  = expressionsOfEquations equations
    subExpressions (Procedure _ equations) = expressionsOfEquations equations
    subExpressions (Rule _ _ es)           = es
    subExpressions (Method _ equations)    = expressionsOfEquations equations
    subExpressions (Attribute _ v)         = [v]
    subExpressions (Object _ v)            = [v]
    subExpressions (Class _ v)             = [v]
    subExpressions (EntryPoint e)          = [e]
    subExpressions (Application a bs)      = a:bs
    subExpressions (Send e1 e2 e3)         = [e1, e2] ++ e3
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


equationBodiesOf :: Expression -> [EquationBody]
equationBodiesOf = concatMap bodiesOf . expressionsOf
  where
    bodiesOf :: Expression -> [EquationBody]
    bodiesOf (Function _ equations) = map (\(Equation _ body) -> body) equations
    bodiesOf _ = []

referencesOf :: Expression -> [(Binding, Expression)]
referencesOf = nub . concatMap (maybeToList . extractReference) . expressionsOf

declarationsOf :: Expression -> [(Binding, Expression)]
declarationsOf = concatMap (maybeToList . extractDeclaration) . expressionsOf

referencedBindingsOf :: Expression -> [Binding]
referencedBindingsOf = map fst . referencesOf

declaredBindingsOf :: Expression -> [Binding]
declaredBindingsOf = map fst . declarationsOf

bindedDeclarationsOf' :: BindingPredicate -> Expression -> [Expression]
bindedDeclarationsOf' f = map snd . filter (f.fst) . declarationsOf

bindedDeclarationsOf :: Binding -> Expression -> [Expression]
bindedDeclarationsOf b = bindedDeclarationsOf' (==b)

transitiveReferencedBindingsOf :: Binding -> Expression -> [Binding]
transitiveReferencedBindingsOf binding code =  expand (concatMap referencedBindingsOf . (`bindedDeclarationsOf` code)) binding
  where
    expand :: Eq a => (a-> [a]) -> a -> [a]
    expand f x = expand' [] f [x]

    expand' _ _ [] = []
    expand' ps f (x:xs) | elem x ps = expand' ps f xs
                        | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


nameOf :: Expression -> Maybe Binding
nameOf = fmap fst . extractDeclaration

extractReference :: Expression -> Maybe (Binding, Expression)
extractReference e@(Reference n) = Just (n, e)
extractReference e@(Exist n _)   = Just (n, e)
extractReference _               = Nothing


extractDeclaration :: Expression -> Maybe (Binding, Expression)
extractDeclaration e@(TypeSignature n _)  = Just (n, e)
extractDeclaration e@(TypeAlias n )       = Just (n, e)
extractDeclaration e@(Variable n _)       = Just (n, e)
extractDeclaration e@(Function n _)       = Just (n, e)
extractDeclaration e@(Record n)           = Just (n, e)
extractDeclaration e@(Fact n _)           = Just (n, e)
extractDeclaration e@(Rule n _ _)         = Just (n, e)
extractDeclaration e@(Procedure n _)      = Just (n, e)
extractDeclaration e@(Object n _)         = Just (n, e)
extractDeclaration e@(Class n _)          = Just (n, e)
extractDeclaration e@(Method n _)         = Just (n, e)
extractDeclaration e@(Attribute n _)      = Just (n, e)
extractDeclaration e@(EntryPoint _)       = Just ("anonymous", e)
extractDeclaration _                      = Nothing


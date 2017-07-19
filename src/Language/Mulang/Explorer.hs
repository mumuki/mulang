module Language.Mulang.Explorer (
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
import Language.Mulang.Unfold (Unfold, allExpressions, mainExpressions)

import Data.Maybe (maybeToList)
import Data.List (nub)


-- | Returns all the body equations of functions, procedures and methods
equationBodiesOf :: Expression -> [EquationBody]
equationBodiesOf = concatMap bodiesOf . mainExpressions
  where
    bodiesOf :: Expression -> [EquationBody]
    bodiesOf (Subroutine  _ equations) = equationBodies equations
    bodiesOf _ = []

    equationBodies = map (\(Equation _ body) -> body)

-- | Returns all the referenced bindings and the expressions that references them
-- For example, in 'f (x + 1)', it returns '(f, f (x + 1))' and '(x, x)'
referencesOf :: Expression -> [(Binding, Expression)]
referencesOf = nub . concatMap (maybeToList . extractReference) . allExpressions


-- | Returns all the declared bindings and the expressions that binds them
-- For example, in 'f x = g x where x = y', it returns '(f, f x = ...)' and '(x, x = y)'
declarationsOf :: Unfold -> Expression -> [(Binding, Expression)]
declarationsOf unfold = concatMap (maybeToList . extractDeclaration) .  unfold

-- | Returns all the referenced bindings
-- For example, in 'f (x + 1)', it returns 'f' and 'x'
referencedBindingsOf :: Expression -> [Binding]
referencedBindingsOf = map fst . referencesOf

-- | Returns all the declared bindings
-- For example, in 'f x = g x where x = y', it returns 'f' and 'x'
declaredBindingsOf :: Unfold -> Expression -> [Binding]
declaredBindingsOf unfold = map fst . declarationsOf unfold

bindedDeclarationsOf' :: BindingPredicate -> Unfold
bindedDeclarationsOf' f = map snd . filter (f.fst) . declarationsOf allExpressions

bindedDeclarationsOf :: Binding -> Unfold
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


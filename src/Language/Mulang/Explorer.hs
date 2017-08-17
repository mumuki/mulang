module Language.Mulang.Explorer (
  equationBodiesOf,
  declarationsOf,
  referencedIdentifiersOf,
  declaredIdentifiersOf,
  boundDeclarationsOf,
  boundDeclarationsOf',
  transitiveReferencedIdentifiersOf,
  nameOf,
  extractDeclaration,
  Expression(..)) where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Unfold (Unfold, allExpressions, mainExpressions)

import Data.Maybe (mapMaybe)
import Data.List (nub)

type Generator a = Expression -> [a]

-- | Returns all the body equations of functions, procedures and methods
equationBodiesOf :: Generator EquationBody
equationBodiesOf = concatMap bodiesOf . mainExpressions
  where
    bodiesOf :: Generator EquationBody
    bodiesOf (Subroutine  _ equations) = equationBodies equations
    bodiesOf _ = []

    equationBodies = map (\(Equation _ body) -> body)

-- | Returns all the declared identifiers and the expressions that binds them
-- For example, in 'f x = g x where x = y', it returns '(f, f x = ...)' and '(x, x = y)'
declarationsOf :: Unfold -> Generator (Identifier, Expression)
declarationsOf unfold = mapMaybe extractDeclaration .  unfold

-- | Returns all the referenced identifiers
-- For example, in 'f (x + 1)', it returns 'f' and 'x'
referencedIdentifiersOf :: Generator Identifier
referencedIdentifiersOf = nub . mapMaybe extractReference . allExpressions

-- | Returns all the declared identifiers
-- For example, in 'f x = g x where x = y', it returns 'f' and 'x'
declaredIdentifiersOf :: Unfold -> Generator Identifier
declaredIdentifiersOf unfold = map fst . declarationsOf unfold

boundDeclarationsOf' :: IdentifierPredicate -> Unfold
boundDeclarationsOf' f = map snd . filter (f.fst) . declarationsOf allExpressions

boundDeclarationsOf :: Identifier -> Unfold
boundDeclarationsOf b = boundDeclarationsOf' (==b)

transitiveReferencedIdentifiersOf :: Identifier -> Generator Identifier
transitiveReferencedIdentifiersOf identifier code =  expand (concatMap referencedIdentifiersOf . (`boundDeclarationsOf` code)) identifier
  where
    expand :: Eq a => (a-> [a]) -> a -> [a]
    expand f x = expand' [] f [x]

    expand' _ _ [] = []
    expand' ps f (x:xs) | elem x ps = expand' ps f xs
                        | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


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


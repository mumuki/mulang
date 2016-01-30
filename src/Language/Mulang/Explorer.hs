module Language.Mulang.Explorer (
  topLevelExpressions,
  topLevelDeclarations,
  topLevelBindings,
  declarationsBindedTo,
  rhssOf,
  nameOf,
  bindingsOf,
  transitiveBindingsOf,
  expressionsOf,
  expressionToBinding,
  Expression(..),
  Binding) where

import Language.Mulang
import Data.Maybe (maybeToList)
import Data.List (nub)

type Binding = String


rhssOf :: Binding -> Expression -> [EquationBody]
rhssOf binding = concatMap rhsForBinding . declarationsBindedTo binding
  where
    rhsForBinding :: Expression -> [EquationBody]
    rhsForBinding (VariableDeclaration _ exp) = [UnguardedBody exp]
    rhsForBinding (FunctionDeclaration _ cases) = cases >>= \(Equation _ rhs) -> [rhs]
    rhsForBinding _ = []


expressionsOf :: Binding -> Expression -> [Expression]
expressionsOf binding code = do
  rhs <- rhssOf binding code
  top <- topExpressions rhs
  unfoldExpression top

bindingsOf :: Binding -> Expression -> [Binding]
bindingsOf binding code = nub $ do
          expr <- expressionsOf binding code
          maybeToList . expressionToBinding $ expr

transitiveBindingsOf :: Binding -> Expression -> [Binding]
transitiveBindingsOf binding code =  expand (`bindingsOf` code) binding


declarationsBindedTo :: Binding -> Expression -> [Expression]
declarationsBindedTo binding = map snd . filter ((==binding).fst) . topLevelDeclarations

topLevelBindings :: Expression -> [Binding]
topLevelBindings = map fst . topLevelDeclarations

topLevelDeclarations :: Expression -> [(Binding, Expression)]
topLevelDeclarations = concatMap (maybeToList.declarationBinding) . topLevelExpressions

nameOf :: Expression -> Maybe Binding
nameOf = fmap fst . declarationBinding

declarationBinding :: Expression -> Maybe (Binding, Expression)
declarationBinding e@(TypeSignature n)         = Just (n, e)
declarationBinding e@(TypeAliasDeclaration n ) = Just (n, e)
declarationBinding e@(VariableDeclaration n _) = Just (n, e)
declarationBinding e@(FunctionDeclaration n _) = Just (n, e)
declarationBinding e@(RecordDeclaration n)     = Just (n, e)
declarationBinding e@(ProcedureDeclaration n)  = Just (n, e)
declarationBinding _                           = Nothing

topLevelExpressions :: Expression -> [Expression]
topLevelExpressions (Sequence es) = es
topLevelExpressions e             = [e]

expressionToBinding :: Expression -> Maybe Binding
expressionToBinding (Variable    q) = Just q
expressionToBinding _               = Nothing

-- private

topExpressions :: EquationBody -> [Expression]
topExpressions (UnguardedBody e) = [e]
topExpressions (GuardedBodies rhss) = rhss >>= \(GuardedBody es1 es2) -> [es1, es2]

unfoldExpression :: Expression -> [Expression]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)
  where
    subExpressions :: Expression -> [Expression]
    subExpressions (Application a bs)        = a:bs
    subExpressions (Lambda _ a)             = [a]
    subExpressions (Comprehension a _)      = [a] --TODO
    subExpressions (MuTuple as)             = as
    subExpressions (MuList as)              = as
    subExpressions (MuObject es)            = [es]
    subExpressions (If a b c)               = [a, b, c]
    subExpressions (Sequence es)            = es
    subExpressions (VariableDeclaration _ v)= [v]
    subExpressions (Return v)               = [v]
    subExpressions (While e1 e2)            = [e1, e2]
    subExpressions (Send e1 e2 e3)          = [e1, e2] ++ e3
    subExpressions _                        = []

expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]
  where
    expand' _ _ [] = []
    expand' ps f (x:xs) | elem x ps = expand' ps f xs
                        | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


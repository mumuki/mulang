module Language.Mulang.Explorer (
  topLevelExpressions,
  topLevelDeclarations,
  topLevelBindings,
  declarationsBindedTo,
  rhssOf,
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

declaratioBinding :: Declaration -> Binding
declaratioBinding (TypeSignature b ) = b
declaratioBinding (TypeAlias b ) = b
declaratioBinding (ConstantDeclaration n _) = n
declaratioBinding (FunctionDeclaration n _)  = n
declaratioBinding (RecordDeclaration n)  = n
declaratioBinding (ProcedureDeclaration n)  = n


declarationsBindedTo :: Binding -> Program -> [Declaration]
declarationsBindedTo binding = filter (isBinding binding) . topLevelDeclarations

rhssOf :: Binding -> Program -> [Rhs]
rhssOf binding = concatMap rhsForBinding . declarationsBindedTo binding

expressionsOf :: Binding -> Program -> [Expression]
expressionsOf binding code = do
  rhs <- rhssOf binding code
  top <- topExpressions rhs
  unfoldExpression top

bindingsOf :: Binding -> Program -> [Binding]
bindingsOf binding code = nub $ do
          expr <- expressionsOf binding code
          maybeToList . expressionToBinding $ expr

transitiveBindingsOf :: Binding -> Program -> [Binding]
transitiveBindingsOf binding code =  expand (`bindingsOf` code) binding

topLevelExpressions :: Program -> [Expression]
topLevelExpressions (Program decls) = decls

topLevelBindings :: Program -> [Binding]
topLevelBindings = map declaratioBinding . topLevelDeclarations

topLevelDeclarations :: Program -> [Declaration]
topLevelDeclarations = concatMap getDeclaration . topLevelExpressions
        where
          getDeclaration (DeclarationExpression d) = [d]
          getDeclaration  _                        = []

expressionToBinding :: Expression -> Maybe Binding
expressionToBinding (Variable    q) = Just q
expressionToBinding _               = Nothing

-- private

topExpressions :: Rhs -> [Expression]
topExpressions (UnguardedRhs e) = [e]
topExpressions (GuardedRhss rhss) = rhss >>= \(GuardedRhs es1 es2) -> [es1, es2]

unfoldExpression :: Expression -> [Expression]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)

subExpressions :: Expression -> [Expression]
subExpressions (InfixApplication a b c) = [a, (Variable b), c]
subExpressions (Application a b)        = [a, b]
subExpressions (Lambda _ a)   = [a]
subExpressions (MuList as)      = as
subExpressions (Comprehension a _)   = [a] --TODO
subExpressions (MuTuple as)      = as
subExpressions (If a b c)       = [a, b, c]
subExpressions _ = []

isBinding :: Binding -> Declaration -> Bool
isBinding binding = (==binding).declaratioBinding

rhsForBinding :: Declaration -> [Rhs]
rhsForBinding (ConstantDeclaration _ rhs) = [rhs]
rhsForBinding (FunctionDeclaration _ cases) = cases >>= \(Equation _ rhs) -> [rhs]
rhsForBinding _ = []

expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


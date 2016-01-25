module Language.Mulang.Explorer (
  parseDecls,
  parseBindings,
  declsOf,
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

declName :: Declaration -> String
declName (TypeSignature b ) = b
declName (TypeAlias b ) = b
declName (ConstantDeclaration n _ _) = n
declName (FunctionDeclaration n _)  = n
declName _                  = []

declsOf :: Binding -> Program -> [Declaration]
declsOf binding = filter (isBinding binding) . parseDecls

rhssOf :: Binding -> Program -> [Rhs]
rhssOf binding = concatMap rhsForBinding . declsOf binding

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

parseDecls :: Program -> [Declaration]
parseDecls (Program decls) = decls

parseBindings :: Program -> [Binding]
parseBindings = map declName . parseDecls

expressionToBinding :: Expression -> Maybe Binding
expressionToBinding (Variable    q) = Just q
expressionToBinding _                = Nothing

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
subExpressions (ListComprehension a _)   = [a] --TODO
subExpressions (MuTuple as)      = as
subExpressions (If a b c)       = [a, b, c]
subExpressions _ = []

isBinding :: Binding -> Declaration -> Bool
isBinding binding = (==binding).declName

rhsForBinding :: Declaration -> [Rhs]
rhsForBinding (ConstantDeclaration _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (FunctionDeclaration _ cases) = cases >>= \(Equation _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l


expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)


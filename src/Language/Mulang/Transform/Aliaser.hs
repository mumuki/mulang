module Language.Mulang.Transform.Aliaser (
    alias) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Language.Mulang.Ast
import           Language.Mulang.Ast.Operator (Operator)
import           Language.Mulang.Ast.Visitor

alias :: Map Identifier Operator -> Expression -> Expression
alias m (Send r (Reference s) es)             | Just o <- Map.lookup s m = Application (Primitive o) (alias m r:mapAliase m es)
alias m (Application (Reference f) es)        | Just o <- Map.lookup f m = Application (Primitive o) (mapAliase m es)
--
alias _ a@(Assert _ _)                        = a
alias m (For stms e1)                         = For stms (alias m e1)
alias m (ForLoop e c inc b)                   = ForLoop (alias m e) (alias m c) (alias m inc) (alias m b)
alias m (Lambda ps e2)                        = Lambda ps (alias m e2)
alias m (Match e1 equations)                  = Match (alias m e1) (mapAliaseEquation m equations)
alias m (Rule n args es)                      = Rule n args (mapAliase m es)
alias m (Send r e es)                         = Send (alias m r) (alias m e) (mapAliase m es)
alias m (Switch v cs d)                       = Switch (alias m v) (aliasSwitchCases m cs) (alias m d)
alias m (Try t cs f)                          = Try (alias m t) (aliasTryCases m cs) (alias m f)
--
alias _ (SinglePatternsList ps c)             = c ps
alias _ c@(Terminal)                          = c
alias m (ExpressionAndExpressionsList e es c) = c (alias m e) (mapAliase m es)
alias m (SingleEquationsList eqs c)           = c (mapAliaseEquation m eqs)
alias m (SingleExpression e c)                = c (alias m e)
alias m (SingleExpressionsList es c)          = c (mapAliase m es)
alias m (ThreeExpressions e1 e2 e3 c)         = c (alias m e1) (alias m e2) (alias m e3)
alias m (TwoExpressions e1 e2 c)              = c (alias m e1) (alias m e2)

mapAliase m = map (alias m)
mapAliaseEquation m = map (aliasEquation m)

aliasEquation :: Map Identifier Operator -> Equation -> Equation
aliasEquation m = mapEquation f f
  where f = alias m

aliasTryCases    m = map (\(p, e) -> (p, alias m e))
aliasSwitchCases m = map (\(e1, e2) -> (alias m e1, alias m e2))

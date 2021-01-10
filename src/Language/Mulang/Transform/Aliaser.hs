module Language.Mulang.Transform.Aliaser (
    aliase) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Language.Mulang.Ast
import           Language.Mulang.Ast.Operator (Operator)
import           Language.Mulang.Ast.Visitor

aliase :: Map Identifier Operator -> Expression -> Expression
aliase m (Send r (Reference s) es)             | Just o <- Map.lookup s m = Application (Primitive o) (aliase m r:mapAliase m es)
aliase m (Application (Reference f) es)        | Just o <- Map.lookup f m = Application (Primitive o) (mapAliase m es)
--
aliase _ a@(Assert _ _)                        = a
aliase m (For stms e1)                         = For stms (aliase m e1)
aliase m (ForLoop e c inc b)                   = ForLoop (aliase m e) (aliase m c) (aliase m inc) (aliase m b)
aliase m (Lambda ps e2)                        = Lambda ps (aliase m e2)
aliase m (Match e1 equations)                  = Match (aliase m e1) (mapAliaseEquation m equations)
aliase m (Rule n args es)                      = Rule n args (mapAliase m es)
aliase m (Send r e es)                         = Send (aliase m r) (aliase m e) (mapAliase m es)
aliase m (Switch v cs d)                       = Switch (aliase m v) (aliaseSwitchCases m cs) (aliase m d)
aliase m (Try t cs f)                          = Try (aliase m t) (aliaseTryCases m cs) (aliase m f)
--
aliase _ (SinglePatternsList ps c)             = c ps
aliase _ c@(Terminal)                          = c
aliase m (ExpressionAndExpressionsList e es c) = c (aliase m e) (mapAliase m es)
aliase m (SingleEquationsList eqs c)           = c (mapAliaseEquation m eqs)
aliase m (SingleExpression e c)                = c (aliase m e)
aliase m (SingleExpressionsList es c)          = c (mapAliase m es)
aliase m (ThreeExpressions e1 e2 e3 c)         = c (aliase m e1) (aliase m e2) (aliase m e3)
aliase m (TwoExpressions e1 e2 c)              = c (aliase m e1) (aliase m e2)

mapAliase m = map (aliase m)
mapAliaseEquation m = map (aliaseEquation m)

aliaseEquation :: Map Identifier Operator -> Equation -> Equation
aliaseEquation m = mapEquation f f
  where f = aliase m

aliaseTryCases    m = map (\(p, e) -> (p, aliase m e))
aliaseSwitchCases m = map (\(e1, e2) -> (aliase m e1, aliase m e2))

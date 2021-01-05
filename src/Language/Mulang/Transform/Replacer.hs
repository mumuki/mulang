module Language.Mulang.Transform.Replacer (
    replace) where

import           GHC.Generics


import Language.Mulang.Ast
import Language.Mulang.Ast.Visitor
import Language.Mulang.Inspector (Inspection)

replace :: Inspection -> Expression -> Expression -> Expression
replace i o e                                     | i e = o
--
replace _ _  a@(Assert _ _)                       = a
replace i o (For stms e1)                         = For stms (replace i o e1)
replace i o (ForLoop e c inc b)                   = ForLoop (replace i o e) (replace i o c) (replace i o inc) (replace i o b)
replace i o (Lambda ps e2)                        = Lambda ps (replace i o e2)
replace i o (Match e1 equations)                  = Match (replace i o e1) (mapReplaceEquation i o equations)
replace i o (Rule n args es)                      = Rule n args (mapReplace i o es)
replace i o (Send r e es)                         = Send (replace i o r) (replace i o e) (mapReplace i o es)
replace i o (Switch v cs d)                       = Switch (replace i o v) (replaceSwitchCases i o cs) (replace i o d)
replace i o (Try t cs f)                          = Try (replace i o t) (replaceTryCases i o cs) (replace i o f)
--
replace _ o (SinglePatternsList ps c)             = c ps
replace _ o c@(Terminal)                          = c
replace i o (ExpressionAndExpressionsList e es c) = c (replace i o e) (mapReplace i o es)
replace i o (SingleEquationsList eqs c)           = c (mapReplaceEquation i o eqs)
replace i o (SingleExpression e c)                = c (replace i o e)
replace i o (SingleExpressionsList es c)          = c (mapReplace i o es)
replace i o (ThreeExpressions e1 e2 e3 c)         = c (replace i o e1) (replace i o e2) (replace i o e3)
replace i o (TwoExpressions e1 e2 c)              = c (replace i o e1) (replace i o e2)

mapReplace i o = map (replace i o)
mapReplaceEquation i o = map (replaceEquation i o)

replaceEquation :: Inspection -> Expression -> Equation -> Equation
replaceEquation i o (Equation ps (UnguardedBody e))   = Equation ps (UnguardedBody (replace i o e))
replaceEquation i o (Equation ps (GuardedBody b))     = Equation ps (GuardedBody (map (\(c, e) -> (replace i o c, replace i o e)) b))

replaceTryCases    i o = map (\(p, e) -> (p, replace i o e))
replaceSwitchCases i o = map (\(e1, e2) -> (replace i o e1, replace i o e2))

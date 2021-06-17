module Language.Mulang.Transform.Replacer (
    replace,
    localReplace,
    globalReplace,
    Replacer) where

import Language.Mulang.Ast
import Language.Mulang.Ast.Visitor
import Language.Mulang.Inspector (Inspection)

type Replacer = Inspection -> Expression -> Expression -> Expression

replace :: Replacer
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
replace i o (RecordUpdate r ups)                  = RecordUpdate (replace i o r) (replaceRecordUpdates i o ups)
--
replace _ _ (SinglePatternsList ps c)             = c ps
replace _ _ c@(Terminal)                          = c
replace i o (ExpressionAndExpressionsList e es c) = c (replace i o e) (mapReplace i o es)
replace i o (SingleEquationsList eqs c)           = c (mapReplaceEquation i o eqs)
replace i o (SingleExpression e c)                = c (replace i o e)
replace i o (SingleExpressionsList es c)          = c (mapReplace i o es)
replace i o (ThreeExpressions e1 e2 e3 c)         = c (replace i o e1) (replace i o e2) (replace i o e3)
replace i o (TwoExpressions e1 e2 c)              = c (replace i o e1) (replace i o e2)

mapReplace i o = map (replace i o)
mapReplaceEquation i o = map (replaceEquation i o)

replaceEquation :: Inspection -> Expression -> Equation -> Equation
replaceEquation i o = mapEquation f f
  where f = replace i o

replaceRecordUpdates i o = map (\(id, e)  -> (id, replace i o e))
replaceTryCases      i o = map (\(p, e)   -> (p, replace i o e))
replaceSwitchCases   i o = map (\(e1, e2) -> (replace i o e1, replace i o e2))

localReplace :: Replacer
localReplace i o (Sequence es)                         = Sequence (map (localReplace i o) es)
localReplace i o e@(Class _ _ _)                       = replace i o e
localReplace i o e@(Interface _ _ _)                   = replace i o e
localReplace i o e@(Rule _ _ _)                        = replace i o e
localReplace i o e@(SingleEquationsList _ _)           = replace i o e
--
localReplace _ _  e                                    = e


globalReplace :: Replacer
globalReplace i o e                                     | i e = o
globalReplace i o (Sequence es)                         = Sequence (map (globalReplace i o) es)
globalReplace _ _ e@(Class _ _ _)                       = e
globalReplace _ _ e@(Interface _ _ _)                   = e
globalReplace _ _ e@(Rule _ _ _)                        = e
globalReplace _ _ e@(SingleEquationsList _ _)           = e
--
globalReplace i o  e                                    = replace i o e

{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Language.Mulang.Ast.Visitor (
    mapEquation,
    pattern Terminal,
    pattern SingleExpression,
    pattern TwoExpressions,
    pattern ThreeExpressions,
    pattern SingleExpressionsList,
    pattern SingleEquationsList,
    pattern SinglePatternsList,
    pattern ExpressionAndExpressionsList
  ) where

import Language.Mulang.Ast

-- By using this module, AST can be visited with less effort using the following strategy:
--
-- --
-- (ProblemSpecificPatterns)
-- --
-- (Assert b a)
-- (For stmts e)
-- (ForLoop e1 e2 e3 e4)
-- (Lambda ps e)
-- (Match e eqs)
-- (Rule i ps es)
-- (Send e1 e2 es)
-- (Switch e1 ps e2)
-- (Try t cs f)
-- --
-- (ExpressionAndExpressionsList e1 es _)
-- (SingleEquationsList es _)
-- (SingleExpression e1 _)
-- (SingleExpressionsList es _)
-- (SinglePatternsList es _)
-- (ThreeExpressions e1 e2 e3 _)
-- (TwoExpressions e1 e2 _)
-- Terminal

pattern Terminal <- (extractTerminal -> True)

extractTerminal :: Expression -> Bool
extractTerminal (Enumeration _ _)   = True
extractTerminal (MuBool _)          = True
extractTerminal (MuChar _)          = True
extractTerminal (MuNumber _)        = True
extractTerminal (MuString _)        = True
extractTerminal (MuSymbol _)        = True
extractTerminal (Other _ Nothing)   = True
extractTerminal (Primitive _)       = True
extractTerminal (Record _)          = True
extractTerminal (Reference _)       = True
extractTerminal (TypeAlias _ _)     = True
extractTerminal (TypeSignature _ _) = True
extractTerminal Equal               = True
extractTerminal MuNil               = True
extractTerminal None                = True
extractTerminal NotEqual            = True
extractTerminal Self                = True
extractTerminal _                   = False

pattern SingleExpression e1 c <- (extractSingleExpression -> Just (e1, c))

extractSingleExpression :: Expression -> Maybe (Expression, Expression -> Expression)
extractSingleExpression (Assignment v1 e)        = Just (e, \e -> (Assignment v1 e))
extractSingleExpression (Attribute v1 e)         = Just (e, \e -> (Attribute v1 e))
extractSingleExpression (Break e)                = Just (e, \e -> (Break e))
extractSingleExpression (Class v1 v2 e)          = Just (e, \e -> (Class v1 v2 e))
extractSingleExpression (Constant v1 e)          = Just (e, \e -> (Constant v1 e))
extractSingleExpression (Continue e)             = Just (e, \e -> (Continue e))
extractSingleExpression (EntryPoint v1 e)        = Just (e, \e -> (EntryPoint v1 e))
extractSingleExpression (FieldReference e v1)    = Just (e, \e -> (FieldReference e v1))
extractSingleExpression (Implement e)            = Just (e, \e -> (Implement e))
extractSingleExpression (Include e)              = Just (e, \e -> (Include e))
extractSingleExpression (Interface v1 v2 e)      = Just (e, \e -> (Interface v1 v2 e))
extractSingleExpression (MuDict e)               = Just (e, \e -> (MuDict e))
extractSingleExpression (MuObject e)             = Just (e, \e -> (MuObject e))
extractSingleExpression (Not e)                  = Just (e, \e -> (Not e))
extractSingleExpression (Object v1 e)            = Just (e, \e -> (Object v1 e))
extractSingleExpression (Other v1 (Just e))      = Just (e, \e -> (Other v1 (Just e)))
extractSingleExpression (Print e)                = Just (e, \e -> (Print e))
extractSingleExpression (Raise e)                = Just (e, \e -> (Raise e))
extractSingleExpression (Return e)               = Just (e, \e -> (Return e))
extractSingleExpression (TypeCast e v1)          = Just (e, \e -> (TypeCast e v1))
extractSingleExpression (Variable v1 e)          = Just (e, \e -> (Variable v1 e))
extractSingleExpression (Yield e)                = Just (e, \e -> (Yield e))
extractSingleExpression _                        = Nothing

pattern TwoExpressions e1 e2 c <- (extractTwoExpressions -> Just (e1, e2, c))

extractTwoExpressions :: Expression -> Maybe (Expression, Expression,
                                              Expression -> Expression -> Expression)
extractTwoExpressions (Arrow e1 e2)               = Just (e1, e2, \e1 e2 -> (Arrow e1 e2))
extractTwoExpressions (FieldAssignment e1 v1 e2)  = Just (e1, e2, \e1 e2 -> (FieldAssignment e1 v1 e2))
extractTwoExpressions (Forall e1 e2)              = Just (e1, e2, \e1 e2 -> (Forall e1 e2))
extractTwoExpressions (Repeat e1 e2)              = Just (e1, e2, \e1 e2 -> (Repeat e1 e2))
extractTwoExpressions (Test e1 e2)                = Just (e1, e2, \e1 e2 -> (Test e1 e2))
extractTwoExpressions (TestGroup e1 e2)           = Just (e1, e2, \e1 e2 -> (TestGroup e1 e2))
extractTwoExpressions (While e1 e2)               = Just (e1, e2, \e1 e2 -> (While e1 e2))
extractTwoExpressions _                           = Nothing

pattern ThreeExpressions e1 e2 e3 c <- (extractThreeExpressions -> Just (e1, e2, e3, c))

extractThreeExpressions :: Expression -> Maybe (Expression, Expression, Expression,
                                                Expression -> Expression -> Expression -> Expression)
extractThreeExpressions (Findall e1 e2 e3) = Just (e1, e2, e3, Findall)
extractThreeExpressions (If e1 e2 e3)      = Just (e1, e2, e3, If)
extractThreeExpressions _                  = Nothing

pattern SingleExpressionsList es c <- (extractSingleExpressionsList -> Just (es, c))

extractSingleExpressionsList :: Expression -> Maybe ([Expression],
                                                     [Expression] -> Expression)
extractSingleExpressionsList (MuList es)   = Just (es, MuList)
extractSingleExpressionsList (MuTuple es)  = Just (es, MuTuple)
extractSingleExpressionsList (Sequence es) = Just (es, Sequence)
extractSingleExpressionsList _               = Nothing

pattern SingleEquationsList es e <- (extractSingleEquationsList -> Just (es, e))

extractSingleEquationsList :: Expression -> Maybe ([Equation],
                                                   [Equation] -> Expression)
extractSingleEquationsList (EqualMethod eqs)       = Just (eqs, (EqualMethod))
extractSingleEquationsList (Function v eqs)        = Just (eqs, (Function v))
extractSingleEquationsList (HashMethod eqs)        = Just (eqs, (HashMethod))
extractSingleEquationsList (Method v eqs)          = Just (eqs, (Method v))
extractSingleEquationsList (PrimitiveMethod v eqs) = Just (eqs, (PrimitiveMethod v))
extractSingleEquationsList (Procedure v eqs)       = Just (eqs, (Procedure v))
extractSingleEquationsList _                       = Nothing

pattern SinglePatternsList es c <- (extractSinglePatternsList -> Just (es, c))

extractSinglePatternsList :: Expression -> Maybe ([Pattern],
                                                  [Pattern] -> Expression)
extractSinglePatternsList (Exist v ps) = Just (ps, (Exist v))
extractSinglePatternsList (Fact v ps)  = Just (ps, (Fact v))
extractSinglePatternsList _              = Nothing

pattern ExpressionAndExpressionsList e1 es c <- (extractExpressionAndExpressionsList -> Just (e1, es, c))

extractExpressionAndExpressionsList :: Expression -> Maybe (Expression, [Expression],
                                                            Expression -> [Expression] -> Expression)
extractExpressionAndExpressionsList (Application e1 es) = Just (e1, es, Application)
extractExpressionAndExpressionsList (New e1 es)         = Just (e1, es, New)
extractExpressionAndExpressionsList _                   = Nothing

-- Maps the conditions and bodies of the given equations' expressions
mapEquation :: (Expression -> Expression) -> (Expression -> Expression) -> Equation -> Equation
mapEquation _  bf (Equation ps (UnguardedBody e)) = Equation ps (UnguardedBody (bf e))
mapEquation cf bf (Equation ps (GuardedBody gs))  = Equation ps (GuardedBody (map (\(c, b) -> (cf c, bf b)) gs))
module Language.Mulang.Parsers.Python (
  py,
  py2,
  py3,
  parsePython,
  parsePython2,
  parsePython3) where

import qualified Language.Mulang.Ast as M
import           Language.Mulang.Builder
import           Language.Mulang.Parsers

import qualified Language.Python.Version3.Parser as Python3
import qualified Language.Python.Version2.Parser as Python2
import           Language.Python.Common.Token (Token)
import           Language.Python.Common.AST

import           Data.List (isPrefixOf)
import           Data.List.Extra (dropLast)
import           Data.Maybe (fromMaybe, listToMaybe)

import           Control.Fallible


py, py2, py3 :: Parser
py = py3
py2 = parsePythonOrFail Python2.parseModule
py3 = parsePythonOrFail Python3.parseModule

parsePython, parsePython2, parsePython3 :: EitherParser
parsePython = parsePython3
parsePython2 = parsePythonOrLeft Python2.parseModule
parsePython3 = parsePythonOrLeft Python3.parseModule

parsePythonOrFail p = orFail . parsePython' p
parsePythonOrLeft p = orLeft . parsePython' p
parsePython' parseModule = fmap (normalize . muPyAST) . (`parseModule` "")

muPyAST :: (ModuleSpan, [Token]) -> M.Expression
muPyAST (modul, _) = muModule modul

muModule :: ModuleSpan -> M.Expression
muModule (Module statements) = compactMap muStatement statements


muStatement :: StatementSpan -> M.Expression
muStatement (While cond body _ _)             = M.While (muExpr cond) (muSuite body)
muStatement (For targets generator body _ _)  = M.For [M.Generator (M.TuplePattern (map (M.VariablePattern . muVariable) targets)) (muExpr generator)] (muSuite body)
muStatement (Fun name args _ body _)          = muComputation (muIdent name) (map muParameter args) (muSuite body)
muStatement (Class name parents body _)       = muClass (listToMaybe . map muParent $ parents) (muIdent name) (muSuite body)
muStatement (Conditional guards els _ )       = foldr muIf (muSuite els) guards
muStatement (Assign [to] from _)              = M.Assignment (muVariable to) (muExpr from)
muStatement (AugmentedAssign to op from _)    = M.Assignment (muVariable to) (M.Application (M.Reference . muAssignOp $ op) [M.Reference . muVariable $ to, muExpr from])
--muStatement (Decorated
--     { decorated_decorators :: [Decorator annot] -- ^ Decorators.
--     , decorated_def :: Statement annot -- ^ Function or class definition to be decorated.
--     , stmt_annot :: annot
--     }
muStatement (Return expr _)                   = M.Return $ fmapOrNull muExpr expr
muStatement (Try body handlers _ finally _)   = M.Try (muSuite body) (map muHandler handlers) (muSuite finally)
muStatement (Raise expr _)                    = M.Raise $ muRaiseExpr expr
--muStatement (With
--     { with_context :: [(Expr annot, Maybe (Expr annot))] -- ^ Context expression(s) (yields a context manager).
--     , with_body :: Suite annot -- ^ Suite to be managed.
--     , stmt_annot :: annot
--     }
muStatement (Pass _)                          = M.None
--muStatement (Break { stmt_annot :: annot }
--muStatement (Continue { stmt_annot :: annot }
--muStatement (Delete
--     { del_exprs :: [Expr annot] -- ^ Items to delete.
--     , stmt_annot :: annot
--     }
muStatement (StmtExpr expr _)                 = muExpr expr
--muStatement (Global
--     { global_vars :: [Ident annot] -- ^ Variables declared global in the current block.
--     , stmt_annot :: annot
--     }
--muStatement (NonLocal
--     { nonLocal_vars :: [Ident annot] -- ^ Variables declared nonlocal in the current block (their binding comes from bound the nearest enclosing scope).
--     , stmt_annot :: annot
--     }
--muStatement (Assert
--     { assert_exprs :: [Expr annot] -- ^ Expressions being asserted.
--     , stmt_annot :: annot
--     }
muStatement (Print _ exprs _ _)               = M.Print $ compactMap muExpr exprs
muStatement (Exec expr _ _)                   = muExpr expr
muStatement e                                 = M.debug e

muClass (Just "unittest.TestCase") name body = M.TestGroup (M.MuString name) $ normalizeTests body
muClass parent name                     body = M.Class name parent body

normalizeTests (M.Sequence exprs) = M.Sequence $ map normalizeTest exprs
normalizeTests expr               = normalizeTest expr

normalizeTest func@(M.SimpleProcedure name _ body) =  if isPrefixOf "test_" name
                                                      then M.Test (M.MuString name) body
                                                      else func
normalizeTest e                                    = e

muIf (condition, body) otherwise = M.If (muExpr condition) (muSuite body) otherwise

muParent (ArgExpr (Var ident _) _)            = muIdent ident
muParent (ArgExpr (Dot (Var var _) attr _) _) = muIdent var ++ "." ++ muIdent attr
muParent _                                    = undefined

muComputation name params body | containsReturn body = M.SimpleFunction name params body
                               | otherwise           = M.SimpleProcedure name params body


containsReturn :: M.Expression -> Bool
containsReturn (M.Return _)    = True
containsReturn (M.Sequence xs) = any containsReturn xs
containsReturn _               = False

muParameter :: ParameterSpan -> M.Pattern
muParameter (Param name _ _ _) = M.VariablePattern (muIdent name)

muIdent :: IdentSpan -> String
muIdent (Ident id _) = id

muSuite :: SuiteSpan -> M.Expression
muSuite = compactMap muStatement


muExpr :: ExprSpan -> M.Expression
muExpr (Var ident _)              = M.Reference (muIdent ident)
muExpr (Int value _ _)            = muNumberFromInt value
muExpr (LongInt value _ _)        = muNumberFromInt value
muExpr (Float value _ _)          = M.MuNumber value
--muExpr (Imaginary { imaginary_value :: Double, expr_literal :: String, expr_annot :: annot }
muExpr (Bool value _)             = M.MuBool value
muExpr (None _)                   = M.MuNil
--muExpr (Ellipsis { expr_annot :: annot }
--muExpr (ByteStrings { byte_string_strings :: [String], expr_annot :: annot }
muExpr (Strings strings _)        = muString strings
muExpr (UnicodeStrings strings _) = muString strings
muExpr (Call fun args _)          = muCallType fun (map muArgument args)
--muExpr (Subscript { subscriptee :: Expr annot, subscript_expr :: Expr annot, expr_annot :: annot }
--muExpr (SlicedExpr { slicee :: Expr annot, slices :: [Slice annot], expr_annot :: annot }
--muExpr (CondExpr
--     { ce_true_branch :: Expr annot -- ^ Expression to evaluate if condition is True.
--     , ce_condition :: Expr annot -- ^ Boolean condition.
--     , ce_false_branch :: Expr annot -- ^ Expression to evaluate if condition is False.
--     , expr_annot :: annot
--     }
muExpr (BinaryOp op left right _) = muApplication op [left, right]
muExpr (UnaryOp op arg _)         = muApplication op [arg]
--muExpr (Dot { dot_expr :: Expr annot, dot_attribute :: Ident annot, expr_annot :: annot }
muExpr (Lambda args body _)       = M.Lambda (map muParameter args) (muExpr body)
muExpr (Tuple exprs _)            = M.MuTuple $ map muExpr exprs
muExpr (Yield arg _)              = M.Yield $ fmapOrNull muYieldArg arg
--muExpr (Generator { gen_comprehension :: Comprehension annot, expr_annot :: annot }
--muExpr (ListComp { list_comprehension :: Comprehension annot, expr_annot :: annot }
muExpr (List exprs _)             = muList exprs
--muExpr (Dictionary { dict_mappings :: [DictMappingPair annot], expr_annot :: annot }
--muExpr (DictComp { dict_comprehension :: Comprehension annot, expr_annot :: annot }
muExpr (Set exprs _)              = muList exprs
--muExpr (SetComp { set_comprehension :: Comprehension annot, expr_annot :: annot }
--muExpr (Starred { starred_expr :: Expr annot, expr_annot :: annot }
muExpr (Paren expr _)             = muExpr expr
--muExpr (StringConversion { backquoted_expr :: Expr annot, expr_anot :: annot }
muExpr e                          = M.debug e


muList = M.MuList . map muExpr

muCallType (Dot _ (Ident "assertEqual" _) _) [a, b] = M.Assert False $ M.Equality a b
muCallType (Dot _ (Ident "assertTrue" _) _)  [a]    = M.Assert False $ M.Truth a
muCallType (Dot _ (Ident "assertFalse" _) _) [a]    = M.Assert True $ M.Truth a
muCallType (Dot receiver ident _)            x      = muCall (M.Send $ muExpr receiver) ident x
muCallType (Var ident _)                     x      = muCall M.Application ident x

muCall callType ident = callType (M.Reference $ muIdent ident)


muApplication op args = M.Application (muOp op) (map muExpr args)

muString = M.MuString . concat . map removeQuotes
  where removeQuotes ('"':'"':'"':rest)    = dropLast 3 rest
        removeQuotes ('"':rest)            = dropLast 1 rest
        removeQuotes ('\'':'\'':'\'':rest) = dropLast 3 rest
        removeQuotes ('\'':rest)           = dropLast 1 rest
        removeQuotes other                 = other

muNumberFromInt = M.MuNumber . fromInteger

muVariable :: ExprSpan -> M.Identifier
muVariable (Var ident _) = muIdent ident

muArgument (ArgExpr expr _)             = muExpr expr
muArgument (ArgVarArgsPos expr _ )      = muExpr expr
muArgument (ArgVarArgsKeyword expr _ )  = muExpr expr
--muArgument ArgKeyword
--     { arg_keyword :: Ident annot -- ^ Keyword name.
--     , arg_expr :: Expr annot -- ^ Argument expression.
--     , arg_annot :: annot
--     }
muArgument e                            = M.debug e

--muYieldArg (YieldFrom expr _)(Expr annot) annot -- ^ Yield from a generator (Version 3 only)
muYieldArg (YieldExpr expr) = muExpr expr

muOp (Equality _)           = M.Equal
muOp (NotEquals _)          = M.NotEqual
muOp op                     = M.Reference $ muOpReference op

muOpReference (And _)                = "and"
muOpReference (Or _)                 = "or"
muOpReference (Not _)                = "not"
muOpReference (Exponent _)           = "**"
muOpReference (LessThan _)           = "<"
muOpReference (GreaterThan _)        = ">"
muOpReference (GreaterThanEquals _)  = ">="
muOpReference (LessThanEquals _)     = "<="
muOpReference (NotEqualsV2 _)        = "<>" -- Version 2 only.
muOpReference (In _)                 = "in"
muOpReference (Is _)                 = "is"
muOpReference (IsNot _)              = "is not"
muOpReference (NotIn _)              = "not in"
muOpReference (BinaryOr _)           = "|"
muOpReference (Xor _)                = "^"
muOpReference (BinaryAnd _)          = "&"
muOpReference (ShiftLeft _)          = "<<"
muOpReference (ShiftRight _)         = ">>"
muOpReference (Multiply _)           = "*"
muOpReference (Plus _)               = "+"
muOpReference (Minus _)              = "-"
muOpReference (Divide _)             = "/"
muOpReference (FloorDivide _)        = "//"
muOpReference (Invert _)             = "~"
muOpReference (Modulo _)             = "%"

muAssignOp (PlusAssign _)       = "+"
muAssignOp (MinusAssign _)      = "-"
muAssignOp (MultAssign _)       = "*"
muAssignOp (DivAssign _)        = "/"
muAssignOp (ModAssign _)        = "%"
muAssignOp (PowAssign _)        = "**"
muAssignOp (BinAndAssign _)     = "&"
muAssignOp (BinOrAssign _)      = "|"
muAssignOp (BinXorAssign _)     = "^"
muAssignOp (LeftShiftAssign _)  = "<"
muAssignOp (RightShiftAssign _) = ">"
muAssignOp (FloorDivAssign _)   = "/"

muHandler (Handler (ExceptClause clause _) suite _) = (muExceptClause clause, muSuite suite)

muExceptClause Nothing                    = M.WildcardPattern
muExceptClause (Just (except, maybeVar))  = muPattern maybeVar (M.TypePattern $ muVarToId except)

muPattern Nothing = id
muPattern (Just var) = M.AsPattern (muVarToId var)

muRaiseExpr (RaiseV3 Nothing)                    = M.None
muRaiseExpr (RaiseV3 (Just (expr, _)))           = muExpr expr
muRaiseExpr (RaiseV2 (Just (ex, Nothing)))       = muExpr ex
muRaiseExpr (RaiseV2 (Just (ex, Just (arg, _)))) = (M.Application (muExpr ex) [muExpr arg])

-- Helpers

fmapOrNull f = fromMaybe M.None . fmap f

muVarToId (Var ident _) = muIdent ident

module Language.Mulang.Parsers.Python (
  npy,
  py,
  py2,
  py3,
  parsePython,
  parsePython2,
  parsePython3) where

import qualified Language.Mulang.Ast as M
import qualified Language.Mulang.Ast.Operator as O
import           Language.Mulang.Builder (compactMap, compactTuple)
import           Language.Mulang.Parsers
import           Language.Mulang.Transform.Normalizer (normalize)

import qualified Language.Python.Version3.Parser as Python3
import qualified Language.Python.Version2.Parser as Python2
import           Language.Mulang.Normalizers.Python (pythonNormalizationOptions)
import           Language.Python.Common.Token (Token)
import           Language.Python.Common.AST

import           Data.List (isPrefixOf)
import           Data.List.Extra (dropLast)
import           Data.Maybe (fromMaybe, listToMaybe)

import           Control.Fallible
import           Control.Monad (msum)

npy, py, py2, py3 :: Parser
npy = normalize pythonNormalizationOptions . py
py = py3
py2 = parsePythonOrFail Python2.parseModule
py3 = parsePythonOrFail Python3.parseModule

parsePython, parsePython2, parsePython3 :: EitherParser
parsePython = parsePython3
parsePython2 = parsePythonOrLeft Python2.parseModule
parsePython3 = parsePythonOrLeft Python3.parseModule

parsePythonOrFail p = orFail . parsePython' p
parsePythonOrLeft p = orLeft . parsePython' p
parsePython' parseModule = fmap muPyAST . (`parseModule` "")

muPyAST :: (ModuleSpan, [Token]) -> M.Expression
muPyAST (modul, _) = muModule modul

muModule :: ModuleSpan -> M.Expression
muModule (Module statements) = compactMap muStatement statements

muClassStatement :: StatementSpan -> M.Expression
muClassStatement (Fun name args _ body _)   = M.SimpleMethod (muIdent name) (map muParameter args) (muSuite body)
muClassStatement (Decorated ds statement _) = M.Decorator (map muDecorator ds) (muClassStatement statement)
muClassStatement other = muStatement other

muDecorator (Decorator [Ident "staticmethod" _] [] _) = M.Static
muDecorator (Decorator [Ident "classmethod" _] [] _)  = M.Classy
muDecorator (Decorator [Ident name _] [] _)           = M.Annotation (M.Reference name)
muDecorator other                                     = M.debugModifier other

muStatement :: StatementSpan -> M.Expression
muStatement (While cond body _ _)             = M.While (muExpr cond) (muSuite body)
muStatement (For targets generator body _ _)  = M.For [M.Generator (muPatterns targets) (muExpr generator)] (muSuite body)
muStatement (Fun name args _ body _)          = muComputation (muIdent name) (map muParameter args) (muSuite body)
muStatement (Class name parents body _)       = muClass (listToMaybe . map muParent $ parents) (muIdent name) (muClassSuite body)
muStatement (Conditional guards els _ )       = foldr muIf (muSuite els) guards
muStatement (Assign [to] from _)              = muAssignment to (muExpr from)
muStatement (AugmentedAssign to op from _)    = muAssignment to (M.Application (muAssignOp $ op) [muExpr to, muExpr from])
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
muStatement (Break _)                         = M.Break M.None
muStatement (Continue _)                      = M.Continue M.None
muStatement (Delete exprs _)                  = compactMap (M.Application (M.Reference "del") . (:[]) . muExpr) exprs
muStatement (StmtExpr expr _)                 = muExpr expr
muStatement (Global exprs _)                  = compactMap (M.Application (M.Reference "global") . (:[]) . muReference) exprs
--muStatement (NonLocal
--     { nonLocal_vars :: [Ident annot] -- ^ Variables declared nonlocal in the current block (their binding comes from bound the nearest enclosing scope).
--     , stmt_annot :: annot
--     }
--muStatement (Assert
--     { assert_exprs :: [Expr annot] -- ^ Expressions being asserted.
--     , stmt_annot :: annot
--     }
muStatement (Print _ exprs _ _)               = M.Print . compactTuple . map muExpr $ exprs
muStatement (Exec expr _ _)                   = muExpr expr
muStatement e                                 = M.debug e

muClass (Just "unittest.TestCase") name body = M.TestGroup (M.MuString name) $ normalizeTests body
muClass parent name                     body = M.Class name parent body

normalizeTests (M.Sequence exprs) = M.Sequence $ map normalizeTest exprs
normalizeTests expr               = normalizeTest expr

normalizeTest func@(M.SimpleMethod name _ body) =  if isPrefixOf "test_" name
                                                   then M.Test (M.MuString name) body
                                                   else func
normalizeTest e                                  = e

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
muParameter (Param name _ Nothing _)  = muVariablePattern name
muParameter (Param name _ (Just e) _) = M.DefaultPattern (muVariablePattern name) (muExpr e)

muIdent :: IdentSpan -> String
muIdent (Ident id _) = id

muSuite, muClassSuite :: SuiteSpan -> M.Expression
muSuite = compactMap muStatement
muClassSuite = compactMap muClassStatement

muExpr :: ExprSpan -> M.Expression
muExpr (Var (Ident "True" _) _)   = M.MuTrue
muExpr (Var (Ident "False" _) _)  = M.MuFalse
muExpr (Var ident _)              = muReference ident
muExpr (Dot expr ident _)          = M.FieldReference (muExpr expr) (muIdent ident)
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
muExpr (Subscript value sub _)    = M.Application (M.Primitive O.GetAt) [muExpr value, muExpr sub]
muExpr (SlicedExpr expr [s] _)    = M.Application (M.Primitive O.Slice) (muExpr expr:muSlices s)
muExpr (CondExpr t c f _)         = M.If (muExpr c) (muExpr t) (muExpr f)
muExpr (BinaryOp op left right _) = muApplication op [left, right]
muExpr (UnaryOp op arg _)         = muApplication op [arg]
--muExpr (Dot { dot_expr :: Expr annot, dot_attribute :: Ident annot, expr_annot :: annot }
muExpr (Lambda args body _)       = M.Lambda (map muParameter args) (muExpr body)
muExpr (Tuple exprs _)            = M.MuTuple $ map muExpr exprs
muExpr (Yield arg _)              = M.Yield $ fmapOrNull muYieldArg arg
muExpr (Generator comp _)         = muComprehension comp
muExpr (ListComp comp _)          = M.Application (M.Reference "list") [muComprehension comp]
muExpr (List exprs _)             = muList exprs
muExpr (Dictionary mappings _)    = muDict mappings
muExpr (DictComp comp _)          = M.Application (M.Reference "dict") [muComprehension comp]
muExpr (Set exprs _)              = muList exprs
muExpr (SetComp comp _)           = M.Application (M.Reference "set") [muComprehension comp]
--muExpr (Starred { starred_expr :: Expr annot, expr_annot :: annot }
muExpr (Paren expr _)             = muExpr expr
--muExpr (StringConversion { backquoted_expr :: Expr annot, expr_anot :: annot }
muExpr e                          = M.debug e


muSlices :: SliceSpan -> [M.Expression]
muSlices (SliceProper l u s _)  = map muSlice $ [l, u, msum s]
muSlices other                  = [M.debug other]

muSlice :: Maybe ExprSpan -> M.Expression
muSlice Nothing  = M.None
muSlice (Just e) = muExpr e

muComprehension (Comprehension (ComprehensionExpr e) for _) = M.For (muComprehensionFor for) (M.Yield (muExpr e))
muComprehension (Comprehension other                 for _) = M.For (muComprehensionFor for) (M.Yield (M.debug other))

muComprehensionFor :: CompForSpan -> [M.Statement]
muComprehensionFor e = unfoldStatements $ Just (IterFor e undefined)

unfoldStatements :: (Maybe CompIterSpan) -> [M.Statement]
unfoldStatements = reverse . unfoldStatements' []

unfoldStatements' acc Nothing = acc
unfoldStatements' acc (Just (IterIf (CompIf ifExpr statements _) _)) = unfoldStatements' (muGuard ifExpr : acc) statements
unfoldStatements' acc (Just (IterFor (CompFor False fors ins_ statements _) _))
                                                                     = unfoldStatements' (muGenerator fors ins_ : acc) statements

muGenerator :: [ExprSpan] -> ExprSpan ->  M.Statement
muGenerator fors e = M.Generator (muPatterns fors) (muExpr e)

muGuard :: ExprSpan -> M.Statement
muGuard = M.Guard . muExpr

muPatterns :: [ExprSpan] -> M.Pattern
muPatterns [pattern]      = muPattern pattern
muPatterns patterns@(_:_) = M.TuplePattern . map muPattern $ patterns

muPattern :: ExprSpan -> M.Pattern
muPattern (Var ident _) = muVariablePattern ident
muPattern (Tuple es _)  = M.TuplePattern . map muPattern $ es
muPattern (Paren e  _)  = muPattern e
muPattern other         = M.debugPattern other

muVariablePattern = M.VariablePattern . muIdent

muList = M.MuList . map muExpr

muDict = M.MuDict . compactMap muArrow
muArrow (DictMappingPair k v) = M.Arrow (muExpr k) (muExpr v)

muCallType (Dot _ (Ident "assertEqual" _) _) [a, b] = M.Assert False $ M.Equality a b
muCallType (Dot _ (Ident "assertTrue" _) _)  [a]    = M.Assert False $ M.Truth a
muCallType (Dot _ (Ident "assertFalse" _) _) [a]    = M.Assert True $ M.Truth a
muCallType (Dot receiver ident _)            x      = muCall (M.Send $ muExpr receiver) ident x
muCallType (Var (Ident "print" _) _)         xs     = M.Print . compactTuple $ xs
muCallType (Var (Ident "len" _) _)           xs     = M.Application (M.Primitive O.Size) xs
muCallType (Var ident _)                     xs     = muCall M.Application ident xs

muCall callType ident = callType (muReference ident)

muReference = M.Reference . muIdent


muApplication op args = M.Application (muOp op) (map muExpr args)

muString = M.MuString . concat . map removeQuotes
  where removeQuotes ('"':'"':'"':rest)    = dropLast 3 rest
        removeQuotes ('"':rest)            = dropLast 1 rest
        removeQuotes ('\'':'\'':'\'':rest) = dropLast 3 rest
        removeQuotes ('\'':rest)           = dropLast 1 rest
        removeQuotes other                 = other

muNumberFromInt = M.MuNumber . fromInteger

muAssignment :: ExprSpan -> M.Expression -> M.Expression
muAssignment (Var ident _)           = M.Assignment (muIdent ident)
muAssignment (Dot expr ident _)      = M.FieldAssignment (muExpr expr) (muIdent ident)
muAssignment (Subscript expr sub _)  = \value -> M.Application (M.Primitive O.SetAt) [muExpr expr, muExpr sub, value]
muAssignment other                   = const (M.debug other)

muArgument (ArgExpr expr _)             = muExpr expr
muArgument (ArgVarArgsPos expr _ )      = muExpr expr
muArgument (ArgVarArgsKeyword expr _ )  = muExpr expr
muArgument (ArgKeyword name expr _)     = M.As (muIdent name) (muExpr expr)

--muYieldArg (YieldFrom expr _)(Expr annot) annot -- ^ Yield from a generator (Version 3 only)
muYieldArg (YieldExpr expr) = muExpr expr

muOp (Equality _)           = M.Primitive O.Equal
muOp (NotEquals _)          = M.Primitive O.NotEqual
muOp op                     = muOpReference op

muOpReference (And _)                = M.Primitive O.And
muOpReference (Or _)                 = M.Primitive O.Or
muOpReference (Not _)                = M.Primitive O.Negation
muOpReference (Exponent _)           = M.Reference "**"
muOpReference (LessThan _)           = M.Primitive O.LessThan
muOpReference (GreaterThan _)        = M.Primitive O.GreaterThan
muOpReference (GreaterThanEquals _)  = M.Primitive O.GreaterOrEqualThan
muOpReference (LessThanEquals _)     = M.Primitive O.LessOrEqualThan
muOpReference (NotEqualsV2 _)        = M.Primitive O.NotEqual -- Version 2 only.
muOpReference (In _)                 = M.Reference "in"
muOpReference (Is _)                 = M.Reference "is"
muOpReference (IsNot _)              = M.Reference "is not"
muOpReference (NotIn _)              = M.Reference "not in"
muOpReference (BinaryOr _)           = M.Primitive O.BitwiseOr
muOpReference (Xor _)                = M.Primitive O.BitwiseXor
muOpReference (BinaryAnd _)          = M.Primitive O.BitwiseAnd
muOpReference (ShiftLeft _)          = M.Primitive O.BitwiseLeftShift
muOpReference (ShiftRight _)         = M.Primitive O.BitwiseRightShift
muOpReference (Multiply _)           = M.Primitive O.Multiply
muOpReference (Plus _)               = M.Primitive O.Plus
muOpReference (Minus _)              = M.Primitive O.Minus
muOpReference (Divide _)             = M.Primitive O.Divide
muOpReference (FloorDivide _)        = M.Reference "//"
muOpReference (Invert _)             = M.Reference "~"
muOpReference (Modulo _)             = M.Primitive O.Modulo

muAssignOp (PlusAssign _)       = M.Primitive O.Plus
muAssignOp (MinusAssign _)      = M.Primitive O.Minus
muAssignOp (MultAssign _)       = M.Primitive O.Multiply
muAssignOp (DivAssign _)        = M.Primitive O.Divide
muAssignOp (ModAssign _)        = M.Primitive O.Modulo
muAssignOp (PowAssign _)        = M.Reference "**"
muAssignOp (BinAndAssign _)     = M.Primitive O.BitwiseAnd
muAssignOp (BinOrAssign _)      = M.Primitive O.BitwiseOr
muAssignOp (BinXorAssign _)     = M.Primitive O.BitwiseXor
muAssignOp (LeftShiftAssign _)  = M.Primitive O.BitwiseLeftShift
muAssignOp (RightShiftAssign _) = M.Primitive O.BitwiseRightShift
muAssignOp (FloorDivAssign _)   = M.Reference "/"

muHandler (Handler (ExceptClause clause _) suite _) = (muExceptClause clause, muSuite suite)

muExceptClause Nothing                    = M.WildcardPattern
muExceptClause (Just (except, maybeVar))  = muExceptPattern maybeVar (M.TypePattern $ muVarToId except)

muExceptPattern Nothing = id
muExceptPattern (Just var) = M.AsPattern (muVarToId var)

muRaiseExpr (RaiseV3 Nothing)                    = M.None
muRaiseExpr (RaiseV3 (Just (expr, _)))           = muExpr expr
muRaiseExpr (RaiseV2 (Just (ex, Nothing)))       = muExpr ex
muRaiseExpr (RaiseV2 (Just (ex, Just (arg, _)))) = (M.Application (muExpr ex) [muExpr arg])

-- Helpers

fmapOrNull f = fromMaybe M.None . fmap f

muVarToId (Var ident _) = muIdent ident

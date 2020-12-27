module Language.Mulang.Parsers.JavaScript (js, parseJavaScript) where

import Language.Mulang.Ast hiding (Equal, NotEqual)
import Language.Mulang.Ast.Operator (Operator (..))
import Language.Mulang.Builder (compact, compactMap, normalizeWith, defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))
import Language.Mulang.Parsers

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST

import Data.Either ()
import Data.List (partition)
import Data.List.Extra (headOrElse)

import Control.Fallible

js :: Parser
js = orFail . parseJavaScript'

parseJavaScript :: MaybeParser
parseJavaScript = orNothing . parseJavaScript'

parseJavaScript' :: String -> Either String Expression
parseJavaScript' = fmap (normalize . muJSAST) . (`parse` "src")
  where normalize = normalizeWith (defaultNormalizationOptions { sortSequenceDeclarations = SortUniqueNonVariables })

muJSAST:: JSAST -> Expression
muJSAST (JSAstProgram statements _)     = compactMap muJSStatement statements
muJSAST (JSAstStatement statement _)    = muJSStatement statement
muJSAST (JSAstExpression expression _)  = muJSExpression expression
muJSAST (JSAstLiteral expression _)     = muJSExpression expression


muJSStatement:: JSStatement -> Expression
muJSStatement (JSStatementBlock _ statements _ _)                           = compactMap muJSStatement statements
--muJSStatement (JSConstant _ (JSCommaList JSExpression) _) -- ^const, decl, autosemi
muJSStatement (JSDoWhile _ statement _ _ expression _ _)                    = While (muJSExpression expression) (muJSStatement statement)
muJSStatement (JSFor _ _ inits _ conds _ progs _ body)                      = muFor inits conds progs body
muJSStatement (JSForIn _ _ id _ gen _ body)                                 = muForIn id gen body
muJSStatement (JSForVar _ _ _ inits _ conds _ progs _ body)                 = muFor inits conds progs body
muJSStatement (JSForVarIn _ _ _ (JSVarInitExpression id _) _ gen _ body)    = muForIn id gen body
muJSStatement (JSForLet _ _ _ inits _ conds _ progs _ body)                 = muFor inits conds progs body
muJSStatement (JSForLetIn _ _ _ (JSVarInitExpression id _) _ gen _ body)    = muForIn id gen body
muJSStatement (JSForLetOf _ _ _ (JSVarInitExpression id _) _ gen _ body)    = muForIn id gen body
muJSStatement (JSForConstOf _ _ _ (JSVarInitExpression id _) _ gen _ body)  = muForIn id gen body
muJSStatement (JSForOf _ _ id _ gen _ body)                                 = muForIn id gen body
muJSStatement (JSForVarOf _ _ _ (JSVarInitExpression id _) _ gen _ body)    = muForIn id gen body
muJSStatement (JSFunction _ ident _ params _ body _)                        = muComputation ident params body
muJSStatement (JSIf _ _ expression _ statement)                             = If (muJSExpression expression) (muJSStatement statement) None
muJSStatement (JSIfElse _ _ expression _ ifStatement _ elseStatement)       = If (muJSExpression expression) (muJSStatement ifStatement) (muJSStatement elseStatement)
muJSStatement (JSLabelled _ _ statement)                                    = muJSStatement statement
muJSStatement (JSEmptyStatement _)                                          = None
muJSStatement (JSExpressionStatement (JSIdentifier _ val) _)                = Reference val
muJSStatement (JSExpressionStatement expression _)                          = muJSExpression expression
muJSStatement (JSAssignStatement to op value _)                             = muAssignment to op (muJSExpression value)
muJSStatement (JSMethodCall (JSMemberDot receptor _ message) _ params _ _)  = normalizeReference $ Send (muJSExpression receptor) (muJSExpression message) (muJSExpressionList params)
muJSStatement (JSMethodCall ident _ params _ _)                             = normalizeReference $ Application (muJSExpression ident) (muJSExpressionList params)
muJSStatement (JSReturn _ maybeExpression _)                                = Return (maybe None muJSExpression maybeExpression)
muJSStatement (JSSwitch _ _ expression _ _ cases _ _)                       = muSwitch expression . partition isDefault $ cases
muJSStatement (JSThrow _ expression _)                                      = Raise (muJSExpression expression)
muJSStatement (JSTry _ block catches finally)                               = Try (muJSBlock block) (map muJSTryCatch catches) (muJSTryFinally finally)
muJSStatement (JSVariable _ list _)                                         = Mostly "var" $ muLValue Variable list
muJSStatement (JSLet _ list _)                                              = muLValue Variable list
muJSStatement (JSConstant _ list _)                                         = muLValue Constant list
muJSStatement (JSWhile _ _ expression _ statement)                          = While (muJSExpression expression) (muJSStatement statement)
muJSStatement e                                                             = debug e

normalizeReference (SimpleSend  (Reference "console") "log"      [value])                    = Print value
normalizeReference (SimpleSend  (Reference "assert") "equals"    [expected, actual])         = Assert False $ Equality expected actual
normalizeReference (SimpleSend  (Reference "assert") "notEquals" [expected, actual])         = Assert True $ Equality expected actual
normalizeReference (SimpleSend  (Reference "assert") "throws"    [block, error])             = Assert False $ Failure block error
normalizeReference (Application (Reference "assert")             [expression])               = Assert False $ Truth expression
normalizeReference (Application (Reference "describe")           [description, Lambda [] e]) = TestGroup description e
normalizeReference (Application (Reference "context")            [description, Lambda [] e]) = TestGroup description e
normalizeReference (Application (Reference "it")                 [description, Lambda [] e]) = Test description e
normalizeReference e                                                                         = e

muAssignment (JSIdentifier _ name) op value                       = Assignment name (muJSAssignOp op name value)
muAssignment (JSMemberDot expr1 _ (JSIdentifier _ name)) op value = FieldAssignment (muJSExpression expr1) name (muJSAssignOp op name value)
muAssignment other op value                                       = MuTuple [debug other, debug op, debug value]

muLValue :: (Identifier -> Expression -> Expression) -> JSCommaList JSExpression -> Expression
muLValue kind = compact . mapJSList f
  where f (JSVarInitExpression (JSIdentifier _ name) initial)  = kind name (muJSVarInitializer initial)

mapJSList :: (a -> b) -> JSCommaList a -> [b]
mapJSList f = map f . muJSCommaList

muJSExpressionList :: JSCommaList JSExpression -> [Expression]
muJSExpressionList = mapJSList muJSExpression

muJSPatternList :: JSCommaList JSExpression -> [Pattern]
muJSPatternList = mapJSList muExpressionPattern

muJSExpressionFromList = compact . muJSExpressionList

muFor inits conds progs body = (ForLoop
                                  (muJSExpressionFromList inits)
                                  (muJSExpressionFromList conds)
                                  (muJSExpressionFromList progs)
                                  (muJSStatement body))

muForIn kind (JSIdentifier _ id) generator body = (For
                                                [Generator (kind id) (muJSExpression generator)]
                                                (muJSStatement body))

muSwitch expression (def, cases) = Switch (muJSExpression expression) (map muCase cases) (headOrElse None . map muDefault $ def)

muCase (JSCase _ expression _ statements) = (muJSExpression expression, compactMap muJSStatement statements)

muDefault (JSDefault _ _ statements) = compactMap muJSStatement statements

muComputation :: JSIdent -> JSCommaList JSExpression -> JSBlock -> Expression
muComputation JSIdentNone params body = Lambda (muJSPatternList params) (muJSBlock body)
muComputation (JSIdentName _ name) params body = (computationFor (muJSBlock body)) name (muEquation (muJSPatternList params) (muJSBlock body))

isDefault:: JSSwitchParts -> Bool
isDefault (JSDefault _ _ _) = True
isDefault _                 = False

muIdentPattern :: JSIdent -> Pattern
muIdentPattern (JSIdentName _ name) = VariablePattern name

muExpressionPattern :: JSExpression -> Pattern
muExpressionPattern (JSIdentifier _ name) = VariablePattern name
muExpressionPattern other                 = debugPattern other

muEquation :: [Pattern] -> Expression -> SubroutineBody
muEquation params body = [SimpleEquation params body]

computationFor :: Expression -> Identifier -> [Equation] -> Expression
computationFor body | containsReturn body = Function
                    | otherwise           = Procedure

containsReturn :: Expression -> Bool
containsReturn (Return _)    = True
containsReturn (Sequence xs) = any containsReturn xs
containsReturn _             = False


muJSExpression:: JSExpression -> Expression
muJSExpression (JSIdentifier _ "undefined")                         = None
muJSExpression (JSIdentifier _ name)                                = Reference name
muJSExpression (JSDecimal _ val)                                    = MuNumber (read val)
muJSExpression (JSLiteral _ "null")                                 = MuNil
muJSExpression (JSLiteral _ "true")                                 = MuTrue
muJSExpression (JSLiteral _ "false")                                = MuFalse
muJSExpression (JSLiteral _ "this")                                 = Self
--muJSExpression (JSHexInteger _ String)
--muJSExpression (JSOctal _ String)
muJSExpression (JSStringLiteral _ val)                              = MuString (removeQuotes val)
--muJSExpression (JSRegEx _ String)
muJSExpression (JSArrayLiteral _ list _)                            = MuList (muJSArrayList list)
muJSExpression (JSAssignExpression (JSIdentifier _ name) op value)  = Assignment name (muJSAssignOp op name.muJSExpression $ value)
muJSExpression (JSMemberExpression (JSMemberDot receptor _ message) _ params _)  = Send (muJSExpression receptor) (muJSExpression message) (muJSExpressionList params)
--muJSExpression (JSCallExpression expression _ params _) = Application (muJSExpression expression) (muJSExpressionList expressionList)
--muJSExpression (JSCallExpressionDot JSExpression _ JSExpression)  -- ^expr, dot, expr
--muJSExpression (JSCallExpressionSquare JSExpression _ JSExpression _)  -- ^expr, [, expr, ]
--muJSExpression (JSCommaExpression JSExpression _ JSExpression)          -- ^expression components
muJSExpression (JSExpressionBinary firstVal op secondVal)           = Application (muJSBinOp op) [muJSExpression firstVal, muJSExpression secondVal]
muJSExpression (JSExpressionParen _ expression _)                   = muJSExpression expression
muJSExpression (JSExpressionPostfix (JSIdentifier _ name) op)       = Assignment name (muJSUnaryOp op name)
muJSExpression (JSExpressionTernary condition _ trueVal _ falseVal) = If (muJSExpression condition) (muJSExpression trueVal) (muJSExpression falseVal)
muJSExpression (JSFunctionExpression _ ident _ params _ body)       = muComputation ident params body
muJSExpression (JSArrowExpression  params _ body)                   = Lambda (muJSArrowParameterList params) (muJSStatement body)
muJSExpression (JSMemberDot receptor _ (JSIdentifier _ message))    = FieldReference (muJSExpression receptor) message
muJSExpression (JSMemberExpression id _ params _)                   = Application (muJSExpression id) (muJSExpressionList params)
muJSExpression (JSMemberNew _ (JSIdentifier _ name) _ args _)       = New (Reference name) (muJSExpressionList args)
muJSExpression (JSMemberSquare receptor _ index _)                  = Send (muJSExpression receptor) (Reference "[]") [muJSExpression index]
muJSExpression (JSNewExpression _ (JSIdentifier _ name))            = New (Reference name) []
muJSExpression (JSObjectLiteral _ propertyList _)                   = MuObject (compactMap muJSObjectProperty . muJSCommaTrailingList $ propertyList)
muJSExpression (JSUnaryExpression (JSUnaryOpNot _) e)               = Application (Primitive Negation) [muJSExpression e]
muJSExpression (JSUnaryExpression op (JSIdentifier _ name))         = Assignment name (muJSUnaryOp op name)
muJSExpression (JSVarInitExpression (JSIdentifier _ name) initial)  = Variable name (muJSVarInitializer initial)
muJSExpression e                                                    = debug e

removeQuotes = filter (flip notElem quoteMarks)
  where quoteMarks = "\"'"

muJSBinOp:: JSBinOp -> Expression
muJSBinOp (JSBinOpAnd _)        = Primitive And
muJSBinOp (JSBinOpBitAnd _)     = Primitive And
muJSBinOp (JSBinOpBitOr _)      = Primitive Or
muJSBinOp (JSBinOpBitXor _)     = Primitive BitwiseXor
muJSBinOp (JSBinOpDivide _)     = Primitive Divide
muJSBinOp (JSBinOpEq _)         = Primitive Similar
muJSBinOp (JSBinOpGe _)         = Primitive GreatherOrEqualThan
muJSBinOp (JSBinOpGt _)         = Primitive GreatherThan
muJSBinOp (JSBinOpInstanceOf _) = Reference "instanceof"
muJSBinOp (JSBinOpLe _)         = Primitive LessOrEqualThan
muJSBinOp (JSBinOpLsh _)        = Primitive BitwiseLeftShift
muJSBinOp (JSBinOpLt _)         = Primitive LessThan
muJSBinOp (JSBinOpMinus _)      = Primitive Minus
muJSBinOp (JSBinOpMod _)        = Primitive Modulo
muJSBinOp (JSBinOpNeq _)        = Primitive NotSimilar
muJSBinOp (JSBinOpOr _)         = Primitive Or
muJSBinOp (JSBinOpPlus _)       = Primitive Plus
muJSBinOp (JSBinOpRsh _)        = Primitive BitwiseRightShift
muJSBinOp (JSBinOpStrictEq _)   = Primitive Equal
muJSBinOp (JSBinOpStrictNeq _)  = Primitive NotEqual
muJSBinOp (JSBinOpTimes _)      = Primitive Multiply


muJSUnaryOp:: JSUnaryOp -> Identifier -> Expression
muJSUnaryOp (JSUnaryOpDecr _) r = (Application (Primitive Minus) [Reference r, MuNumber 1])
--muJSUnaryOp (JSUnaryOpDelete _)
muJSUnaryOp (JSUnaryOpIncr _) r = (Application (Primitive Plus) [Reference r, MuNumber 1])
--muJSUnaryOp (JSUnaryOpMinus _)
--muJSUnaryOp (JSUnaryOpPlus _)
--muJSUnaryOp (JSUnaryOpTilde _)
--muJSUnaryOp (JSUnaryOpTypeof _)
--muJSUnaryOp (JSUnaryOpVoid _)
muJSUnaryOp e _                 = debug e

muJSAssignOp:: JSAssignOp -> Identifier -> Expression -> Expression
muJSAssignOp (JSAssign _) _ v = v
muJSAssignOp op r v           = (Application (muJSAssignOp' op) [Reference r, v])

muJSAssignOp':: JSAssignOp -> Expression
muJSAssignOp' (JSTimesAssign _)   = Primitive Multiply
muJSAssignOp' (JSDivideAssign _)  = Primitive Divide
--muJSAssignOp' (JSModAssign _)
muJSAssignOp' (JSPlusAssign _)    = Primitive Plus
muJSAssignOp' (JSMinusAssign _)   = Primitive Minus
--muJSAssignOp' (JSLshAssign _)
--muJSAssignOp' (JSRshAssign _)
--muJSAssignOp' (JSUrshAssign _)
muJSAssignOp' (JSBwAndAssign _)   = Reference "&"
muJSAssignOp' (JSBwXorAssign _)   = Reference "^"
muJSAssignOp' (JSBwOrAssign _)    = Reference "|"
muJSAssignOp' e                   = debug e

muJSTryCatch:: JSTryCatch -> (Pattern, Expression)
muJSTryCatch (JSCatch _ _ (JSIdentifier _ name) _ block) = (VariablePattern name, muJSBlock block)
--muJSTryCatch JSCatchIf _ _ JSExpression _ JSExpression _ JSBlock -- ^catch,lb,ident,if,expr,rb,block
muJSTryCatch e = (WildcardPattern, debug e)

muJSTryFinally:: JSTryFinally -> Expression
muJSTryFinally (JSFinally _ block)   = muJSBlock block
muJSTryFinally JSNoFinally           = None

muJSBlock:: JSBlock -> Expression
muJSBlock (JSBlock _ statements _)   = compactMap muJSStatement statements

muJSVarInitializer:: JSVarInitializer -> Expression
muJSVarInitializer (JSVarInit _ expression) = muJSExpression expression
--muJSVarInitializer JSVarInitNone
muJSVarInitializer e                        = debug e


muJSObjectProperty:: JSObjectProperty -> Expression
--muJSObjectProperty JSPropertyAccessor JSAccessor JSPropertyName _ [JSExpression] _ JSBlock -- ^(get|set), name, lb, params, rb, block
muJSObjectProperty (JSPropertyNameandValue id _ [JSFunctionExpression _ _ _ params _ block])   = Method (muJSPropertyName id) (muEquation (muJSPatternList params) (muJSBlock block))
muJSObjectProperty (JSPropertyNameandValue id _ [expression])                                  = Variable (muJSPropertyName id) (muJSExpression expression)
muJSObjectProperty e                                                                           = debug e

muJSPropertyName:: JSPropertyName -> Identifier
muJSPropertyName = removeQuotes.muJSPropertyName'

muJSPropertyName':: JSPropertyName -> Identifier
muJSPropertyName' (JSPropertyIdent _ name)   = name
muJSPropertyName' (JSPropertyString _ name)  = name
muJSPropertyName' (JSPropertyNumber _ name)  = name

muJSArrayList:: [JSArrayElement] -> [Expression]
muJSArrayList list = [muJSExpression expression | (JSArrayElement expression) <- list]

muJSCommaList:: JSCommaList a -> [a]
muJSCommaList (JSLCons xs _ x)   = muJSCommaList xs ++ [x]
muJSCommaList (JSLOne x)         = [x]
muJSCommaList JSLNil             = []

muJSCommaTrailingList:: JSCommaTrailingList a -> [a]
muJSCommaTrailingList (JSCTLComma list _) = muJSCommaList list
muJSCommaTrailingList (JSCTLNone list)    = muJSCommaList list

muJSArrowParameterList :: JSArrowParameterList -> [Pattern]
muJSArrowParameterList (JSUnparenthesizedArrowParameter ident)        = [muIdentPattern ident]
muJSArrowParameterList (JSParenthesizedArrowParameterList _ params _) = muJSPatternList params


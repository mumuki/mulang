module Language.Mulang.Parsers.JavaScript (js, parseJavaScript) where

import Language.Mulang.Ast
import Language.Mulang.Builder
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
muJSStatement (JSFunction _ ident _ params _ body _)                        = muComputation ident params body
muJSStatement (JSIf _ _ expression _ statement)                             = If (muJSExpression expression) (muJSStatement statement) MuNull
muJSStatement (JSIfElse _ _ expression _ ifStatement _ elseStatement)       = If (muJSExpression expression) (muJSStatement ifStatement) (muJSStatement elseStatement)
muJSStatement (JSLabelled _ _ statement)                                    = muJSStatement statement
muJSStatement (JSEmptyStatement _)                                          = MuNull
muJSStatement (JSExpressionStatement (JSIdentifier _ val) _)                = Reference val
muJSStatement (JSExpressionStatement expression _)                          = muJSExpression expression
muJSStatement (JSAssignStatement (JSIdentifier _ name) op value _)          = Assignment name (muJSAssignOp op name (muJSExpression value))
muJSStatement (JSMethodCall (JSMemberDot receptor _ message) _ params _ _)  = Send (muJSExpression receptor) (muJSExpression message) (map muJSExpression (muJSCommaList params))
muJSStatement (JSMethodCall ident _ params _ _)                             = Application (muJSExpression ident) (map muJSExpression (muJSCommaList params))
muJSStatement (JSReturn _ maybeExpression _)                                = Return (maybe MuNull muJSExpression maybeExpression)
muJSStatement (JSSwitch _ _ expression _ _ cases _ _)                       = muSwitch expression . partition isDefault $ cases
muJSStatement (JSThrow _ expression _)                                      = Raise (muJSExpression expression)
muJSStatement (JSTry _ block catches finally)                               = Try (muJSBlock block) (map muJSTryCatch catches) (muJSTryFinally finally)
muJSStatement (JSVariable _ list _)                                         = compactMap muJSExpression.muJSCommaList $ list
muJSStatement (JSWhile _ _ expression _ statement)                          = While (muJSExpression expression) (muJSStatement statement)
muJSStatement e                                                             = debug e

muJSExpressionFromList = compactMap muJSExpression . muJSCommaList

muFor inits conds progs body = ForLoop (muJSExpressionFromList inits) (muJSExpressionFromList conds) (muJSExpressionFromList progs) (muJSStatement body)

muForIn (JSIdentifier _ id) generator body = For [Generator (VariablePattern id) (muJSExpression generator)] (muJSStatement body)

muSwitch expression (def, cases) = Switch (muJSExpression expression) (map muCase cases) (headOrElse MuNull . map muDefault $ def)

muCase (JSCase _ expression _ statements) = (muJSExpression expression, compactMap muJSStatement statements)

muDefault (JSDefault _ _ statements) = compactMap muJSStatement statements

muComputation JSIdentNone params body = Lambda (map muPattern (muJSCommaList params)) (muJSBlock body)
muComputation (JSIdentName _ name) params body = (computationFor (muJSBlock body)) name (muEquation (map muPattern (muJSCommaList params)) (muJSBlock body))

isDefault:: JSSwitchParts -> Bool
isDefault (JSDefault _ _ _) = True
isDefault _                 = False

muPattern:: JSIdent -> Pattern
muPattern (JSIdentName _ name) = VariablePattern name

muEquation params body = [SimpleEquation params body]

computationFor :: Expression -> Identifier -> [Equation] -> Expression
computationFor body | containsReturn body = Function
                    | otherwise           = Procedure

containsReturn :: Expression -> Bool
containsReturn (Return _)    = True
containsReturn (Sequence xs) = any containsReturn xs
containsReturn _             = False


muJSExpression:: JSExpression -> Expression
muJSExpression (JSIdentifier _ "undefined")                         = MuNull
muJSExpression (JSIdentifier _ name)                                = Reference name
muJSExpression (JSDecimal _ val)                                    = MuNumber (read val)
muJSExpression (JSLiteral _ "null")                                 = MuNil
muJSExpression (JSLiteral _ "true")                                 = MuTrue
muJSExpression (JSLiteral _ "false")                                = MuFalse
--muJSExpression (JSHexInteger _ String)
--muJSExpression (JSOctal _ String)
muJSExpression (JSStringLiteral _ val)                              = MuString (removeQuotes val)
--muJSExpression (JSRegEx _ String)
muJSExpression (JSArrayLiteral _ list _)                            = MuList (muJSArrayList list)
muJSExpression (JSAssignExpression (JSIdentifier _ name) op value)  = Assignment name (muJSAssignOp op name.muJSExpression $ value)
--muJSExpression (JSCallExpression expression _ params _) = Application (muJSExpression expression) (map muJSExpression.muJSCommaList $ expressionList)
--muJSExpression (JSCallExpressionDot JSExpression _ JSExpression)  -- ^expr, dot, expr
--muJSExpression (JSCallExpressionSquare JSExpression _ JSExpression _)  -- ^expr, [, expr, ]
--muJSExpression (JSCommaExpression JSExpression _ JSExpression)          -- ^expression components
muJSExpression (JSExpressionBinary firstVal op secondVal)           = Application (muJSBinOp op) [muJSExpression firstVal, muJSExpression secondVal]
muJSExpression (JSExpressionParen _ expression _)                   = muJSExpression expression
muJSExpression (JSExpressionPostfix (JSIdentifier _ name) op)       = Assignment name (muJSUnaryOp op name)
muJSExpression (JSExpressionTernary condition _ trueVal _ falseVal) = If (muJSExpression condition) (muJSExpression trueVal) (muJSExpression falseVal)
muJSExpression (JSFunctionExpression _ ident _ params _ body)       = muComputation ident params body
--muJSExpression (JSMemberDot JSExpression _ JSExpression) -- ^firstpart, dot, name
muJSExpression (JSMemberExpression id _ params _)                   = Application (muJSExpression id) (map muJSExpression.muJSCommaList $ params)
muJSExpression (JSMemberNew _ (JSIdentifier _ name) _ args _)       = New name (map muJSExpression.muJSCommaList $ args)
--muJSExpression (JSMemberSquare JSExpression _ JSExpression _) -- ^firstpart, lb, expr, rb
muJSExpression (JSNewExpression _ (JSIdentifier _ name))            = New name []
muJSExpression (JSObjectLiteral _ propertyList _)                   = MuObject (compactMap id.map muJSObjectProperty.muJSCommaTrailingList $ propertyList)
muJSExpression (JSUnaryExpression (JSUnaryOpNot _) e)               = Application (Reference "!") [muJSExpression e]
muJSExpression (JSUnaryExpression op (JSIdentifier _ name))         = Assignment name (muJSUnaryOp op name)
muJSExpression (JSVarInitExpression (JSIdentifier _ name) initial)  = Variable name (muJSVarInitializer initial)
muJSExpression e                                                    = debug e

removeQuotes = filter (flip notElem quoteMarks)
  where quoteMarks = "\"'"

muJSBinOp:: JSBinOp -> Expression
muJSBinOp (JSBinOpAnd _)        = Reference "&&"
muJSBinOp (JSBinOpBitAnd _)     = Reference "&"
muJSBinOp (JSBinOpBitOr _)      = Reference "|"
muJSBinOp (JSBinOpBitXor _)     = Reference "^"
muJSBinOp (JSBinOpDivide _)     = Reference "/"
muJSBinOp (JSBinOpEq _)         = Equal
muJSBinOp (JSBinOpGe _)         = Reference ">="
muJSBinOp (JSBinOpGt _)         = Reference ">"
muJSBinOp (JSBinOpInstanceOf _) = Reference "instanceof"
muJSBinOp (JSBinOpLe _)         = Reference "<="
muJSBinOp (JSBinOpLsh _)        = Reference "<<"
muJSBinOp (JSBinOpLt _)         = Reference "<"
muJSBinOp (JSBinOpMinus _)      = Reference "-"
muJSBinOp (JSBinOpMod _)        = Reference "%"
muJSBinOp (JSBinOpNeq _)        = NotEqual
muJSBinOp (JSBinOpOr _)         = Reference "||"
muJSBinOp (JSBinOpPlus _)       = Reference "+"
muJSBinOp (JSBinOpRsh _)        = Reference ">>"
muJSBinOp (JSBinOpStrictEq _)   = Equal
muJSBinOp (JSBinOpStrictNeq _)  = NotEqual
muJSBinOp (JSBinOpTimes _)      = Reference "*"


muJSUnaryOp:: JSUnaryOp -> Identifier -> Expression
muJSUnaryOp (JSUnaryOpDecr _) r = (Application (Reference "-") [Reference r, MuNumber 1])
--muJSUnaryOp (JSUnaryOpDelete _)
muJSUnaryOp (JSUnaryOpIncr _) r = (Application (Reference "+") [Reference r, MuNumber 1])
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
muJSAssignOp' (JSTimesAssign _)   = Reference "*"
muJSAssignOp' (JSDivideAssign _)  = Reference "/"
--muJSAssignOp' (JSModAssign _)
muJSAssignOp' (JSPlusAssign _)    = Reference "+"
muJSAssignOp' (JSMinusAssign _)   = Reference "-"
--muJSAssignOp' (JSLshAssign _)
--muJSAssignOp' (JSRshAssign _)
--muJSAssignOp' (JSUrshAssign _)
muJSAssignOp' (JSBwAndAssign _)   = Reference "&"
muJSAssignOp' (JSBwXorAssign _)   = Reference "^"
muJSAssignOp' (JSBwOrAssign _)    = Reference "|"
muJSAssignOp' e                   = debug e

muJSTryCatch:: JSTryCatch -> (Pattern, Expression)
--muJSTryCatch JSCatch _ _ JSExpression _ JSBlock -- ^catch,lb,ident,rb,block
--muJSTryCatch JSCatchIf _ _ JSExpression _ JSExpression _ JSBlock -- ^catch,lb,ident,if,expr,rb,block
muJSTryCatch e = (WildcardPattern, debug e)

muJSTryFinally:: JSTryFinally -> Expression
muJSTryFinally (JSFinally _ block)   = muJSBlock block
muJSTryFinally JSNoFinally           = MuNull

muJSBlock:: JSBlock -> Expression
muJSBlock (JSBlock _ statements _)   = compactMap muJSStatement statements

muJSVarInitializer:: JSVarInitializer -> Expression
muJSVarInitializer (JSVarInit _ expression) = muJSExpression expression
--muJSVarInitializer JSVarInitNone
muJSVarInitializer e                        = debug e


muJSObjectProperty:: JSObjectProperty -> Expression
--muJSObjectProperty JSPropertyAccessor JSAccessor JSPropertyName _ [JSExpression] _ JSBlock -- ^(get|set), name, lb, params, rb, block
muJSObjectProperty (JSPropertyNameandValue id _ [JSFunctionExpression _ _ _ params _ block])   = Method (muJSPropertyName id) (muEquation (map muPattern (muJSCommaList params)) (muJSBlock block))
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


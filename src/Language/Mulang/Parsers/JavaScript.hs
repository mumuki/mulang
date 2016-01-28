module Language.Mulang.Parsers.JavaScript where

import Language.Mulang
import Language.Mulang.Builder
import Language.Haskell.Syntax
import Language.Haskell.Parser

import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.AST


parseJavaScript :: String -> Maybe Expression
parseJavaScript code = Just . mu $ readJs code

mu (NN (JSSourceElementsTop staments)) =  compact (mapMuNode staments)
  where
    muNode (JSIdentifier n)                                  = [Variable n]
    muNode (JSDecimal val)                                   = [MuNumber (read val)]
    muNode (JSExpression es)                                 = [compact (mapMuNode es)]
    muNode (JSLiteral _)                                     = []
    muNode (JSHexInteger v)                                  = muNode (JSStringLiteral '\'' v)
    muNode (JSOctal v)                                       = muNode (JSStringLiteral '"' v)
    muNode (JSStringLiteral _ v)                             = [MuString v]
    muNode (JSVariables _ decls _)                           = mapMuNode decls
    muNode (JSArrayLiteral _ es _)                           = [MuList (mapMuNode es)]
    muNode (JSVarDecl (NT (JSIdentifier var) _ _) initial)   = [
                             (DeclarationExpression . VariableDeclaration var) (compact . mapMuNode $ initial)]
    muNode (JSWith _ _ _ _ _)                                = [ExpressionOther]
    muNode (JSExpressionTernary cond _ true _ false)         = [muIf (head cond) (head true) false]
    muNode (JSIf _ _ cond _ true false)                      = [muIf cond (head true) false]
    muNode (JSWhile _ _ cond _ action)                       = [muWhile cond action]

    muNode _ = []

    gc (NN n) = n
    gc (NT n _ _) = n

    mapMuNode = concatMap (muNode.gc)
    muNodeSingle = compact . muNode . gc

    muIf :: JSNode -> JSNode -> [JSNode] -> Expression
    muIf cond true [] = If (muNodeSingle cond) (muNodeSingle true) MuUnit
    muIf cond true false = If (muNodeSingle cond) (muNodeSingle true) (compact.mapMuNode $ false)

    muWhile cond action  = While (muNodeSingle cond) (muNodeSingle action)

{-JSRegEx String
JSArguments JSNode [JSNode] JSNode
lb, args, rb
JSBlock [JSNode] [JSNode] [JSNode]
optional lb,optional block statements,optional rb
JSBreak JSNode [JSNode] JSNode
break, optional identifier, autosemi
JSCallExpression String [JSNode] [JSNode] [JSNode]
type : ., (), []; opening [ or ., contents, closing
JSCase JSNode JSNode JSNode [JSNode]
case,expr,colon,stmtlist
JSCatch JSNode JSNode JSNode [JSNode] JSNode JSNode
catch,lb,ident,[if,expr],rb,block
JSContinue JSNode [JSNode] JSNode
continue,optional identifier,autosemi
JSDefault JSNode JSNode [JSNode]
default,colon,stmtlist
JSDoWhile JSNode JSNode JSNode JSNode JSNode JSNode JSNode
do,stmt,while,lb,expr,rb,autosemi
JSElision JSNode
comma
JSExpressionBinary String [JSNode] JSNode [JSNode]
what, lhs, op, rhs
JSExpressionParen JSNode JSNode JSNode
lb,expression,rb
JSExpressionPostfix String [JSNode] JSNode
type, expression, operator
JSExpressionTernary [JSNode] JSNode [JSNode] JSNode [JSNode]
cond, ?, trueval, :, falseval
JSFinally JSNode JSNode
finally,block
JSFor JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode
for,lb,expr,semi,expr,semi,expr,rb.stmt
JSForIn JSNode JSNode [JSNode] JSNode JSNode JSNode JSNode
for,lb,expr,in,expr,rb,stmt
JSForVar JSNode JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode
for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
JSForVarIn JSNode JSNode JSNode JSNode JSNode JSNode JSNode JSNode
for,lb,var,vardecl,in,expr,rb,stmt
JSFunction JSNode JSNode JSNode [JSNode] JSNode JSNode
fn,name, lb,parameter list,rb,block | JSFunctionBody [JSNode] -- ^body
JSFunctionExpression JSNode [JSNode] JSNode [JSNode] JSNode JSNode
fn,[name],lb, parameter list,rb,block`

JSLabelled JSNode JSNode JSNode
identifier,colon,stmt
JSMemberDot [JSNode] JSNode JSNode
firstpart, dot, name
JSMemberSquare [JSNode] JSNode JSNode JSNode
firstpart, lb, expr, rb
JSObjectLiteral JSNode [JSNode] JSNode
lbrace contents rbrace
JSOperator JSNode
opnode
JSPropertyAccessor JSNode JSNode JSNode [JSNode] JSNode JSNode
(get|set), name, lb, params, rb, block
JSPropertyNameandValue JSNode JSNode [JSNode]
name, colon, value
JSReturn JSNode [JSNode] JSNode
return,optional expression,autosemi | JSSourceElements [JSNode] -- ^source elements
JSSourceElementsTop [JSNode]
source elements | JSStatementBlock JSNode JSNode JSNode -- ^lb,block,rb | JSStatementList [JSNode] -- ^statements
JSSwitch JSNode JSNode JSNode JSNode JSNode
switch,lb,expr,rb,caseblock
JSThrow JSNode JSNode
throw val
JSTry JSNode JSNode [JSNode]
try,block,rest
JSUnary String JSNode
type, operator
while,lb,expr,rb,stmt
 -}

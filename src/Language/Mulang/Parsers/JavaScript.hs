module Language.Mulang.Parsers.JavaScript where

import Language.Mulang
import Language.Mulang.Builder

import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.AST

import Data.Maybe (fromJust)

parseJavaScript :: String -> Maybe Expression
parseJavaScript code = Just . mu $ readJs code

mu :: JSNode -> Expression
mu = compact . muNode . gc
  where
    muNode (JSSourceElementsTop statements)                  = [mus statements]
    muNode (JSIdentifier n)                                  = [Variable n]
    muNode (JSDecimal val)                                   = [MuNumber (read val)]
    muNode (JSExpression es)                                 = [mus es]
    muNode (JSLiteral _)                                     = []
    muNode (JSElision _)                                     = []
    muNode (JSHexInteger v)                                  = muNode (JSStringLiteral '\'' v)
    muNode (JSOctal v)                                       = muNode (JSStringLiteral '"' v)
    muNode (JSStringLiteral _ v)                             = [MuString v]
    muNode (JSVariables _ decls _)                           = map mu decls
    muNode (JSArrayLiteral _ es _)                           = [MuList (concatMap (muNode . gc) es)]
    muNode (JSVarDecl var initial)                           = [muVar var initial]
    muNode (JSWith _ _ _ _ _)                                = [ExpressionOther]
    muNode (JSExpressionTernary cond _ true _ false)         = [muIf (head cond) (head true) false]
    muNode (JSIf _ _ cond _ true false)                      = [muIf cond (head true) false]
    muNode (JSWhile _ _ cond _ action)                       = [muWhile cond action]
    muNode (JSBlock _ exps _)                                = [mus exps]
    muNode (JSFunctionExpression _ [] _ params _ body)       = [Lambda (muParams params) (mu body)]
    muNode (JSFunctionExpression _ name _ params _ body)     = [muFunction (head name) params body]
    muNode (JSReturn _ e _)                                  = [mus e]
    muNode (JSExpressionParen _ e _)                         = [mu e]
    muNode (JSObjectLiteral _ es _)                          = [MuObject (mus es)]
    muNode (JSLabelled _ _ e)                                = [mu e]
    muNode (JSPropertyNameandValue var _ initial)            = [muVar var initial]
    muNode (JSFunction _ name _ params _ body)               = [muFunction name params body]


    muNode e = error (show e)

    muParams _ = [OtherPattern]

    muIdentifier = show

    gc (NN n) = n
    gc (NT n _ _) = n

    mus :: [JSNode] -> Expression
    mus  = compact . concatMap (muNode . gc)

    muIf :: JSNode -> JSNode -> [JSNode] -> Expression
    muIf cond true [] = If (mu cond) (mu true) MuUnit
    muIf cond true false = If (mu cond) (mu true) (mus false)

    muWhile cond action  = While (mu cond) (mu action)

    muVar (NT (JSIdentifier var) _ _) initial = VariableDeclaration var (mus initial)

    muFunction :: JSNode -> [JSNode] -> JSNode -> Expression
    muFunction name params body =  FunctionDeclaration (muIdentifier name) [Equation (muParams params) (UnguardedBody (mu body))]


{-JSRegEx String
JSArguments JSNode [JSNode] JSNode
lb, args, rb
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
JSExpressionBinary String [JSNode] JSNode [JSNode]
what, lhs, op, rhs
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


JSMemberDot [JSNode] JSNode JSNode
firstpart, dot, name
JSMemberSquare [JSNode] JSNode JSNode JSNode
firstpart, lb, expr, rb
JSOperator JSNode
opnode
JSPropertyAccessor JSNode JSNode JSNode [JSNode] JSNode JSNode
(get|set), name, lb, params, rb, block
JSSwitch JSNode JSNode JSNode JSNode JSNode
switch,lb,expr,rb,caseblock
JSThrow JSNode JSNode
throw val
JSTry JSNode JSNode [JSNode]
try,block,rest
JSUnary String JSNode
type, operator
 -}

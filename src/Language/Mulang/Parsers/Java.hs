{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.Java (java, parseJava) where

import Language.Mulang.Ast hiding (While, Return, Equal, Lambda)
import qualified Language.Mulang.Ast as M (Expression(While, Return, Equal, Lambda))
import Language.Mulang.Parsers
import Language.Mulang.Builder (compactMap, compactConcatMap)

import Language.Java.Parser
import Language.Java.Syntax

import Control.Fallible

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

java :: Parser
java = orFail . parseJava'

parseJava :: MaybeParser
parseJava = orNothing . parseJava'

parseJava' = fmap m . j

m (CompilationUnit _ _ typeDecls) = compactMap muTypeDecl $ typeDecls

muTypeDecl (ClassTypeDecl decl)    = muClassTypeDecl decl
muTypeDecl (InterfaceTypeDecl decl) = muInterfaceTypeDecl decl

muClassTypeDecl (ClassDecl _ name _ superclass _interfaces (ClassBody body)) =
  Class (i name) (fmap muClassRefType superclass) (compactMap muDecl body )
muClassTypeDecl (EnumDecl _ name _ (EnumBody constants _))                   =
  Enumeration (i name) (map muEnumConstant constants)

muInterfaceTypeDecl (InterfaceDecl _ name _ interfaces (InterfaceBody body)) =
  Interface (i name) (map (Reference . muClassRefType) interfaces) (compactMap muMemberDecl body )

muDecl (MemberDecl memberDecl) = muMemberDecl memberDecl
muDecl (InitDecl _ _)          = Other

muMemberDecl (FieldDecl _ _type _varDecls)                                    = Other
muMemberDecl (MethodDecl _ _ _ name params _ (MethodBody Nothing))            = TypeSignature (i name) (map muFormalParam params) "void"
muMemberDecl (MethodDecl (elem Static -> True) _ _ (Ident "main") [_] _ body) = EntryPoint "main" (muMethodBody body)
muMemberDecl (MethodDecl _ _ _ name params _ body)                            = SimpleMethod (i name) (map (VariablePattern . muFormalParam) params) (muMethodBody body)
muMemberDecl (ConstructorDecl _ _ _ _params _ _constructorBody)               = Other
muMemberDecl (MemberClassDecl decl)                                           = muClassTypeDecl decl
muMemberDecl (MemberInterfaceDecl decl)                                       = muInterfaceTypeDecl decl

muEnumConstant (EnumConstant name _ _) = i name

muFormalParam (FormalParam _ _types _ id) = (v id)

muBlock (Block statements) = compactConcatMap muBlockStmt statements

muBlockStmt (BlockStmt stmt) = [muStmt stmt]
muBlockStmt (LocalClass decl) = [muClassTypeDecl decl]
muBlockStmt (LocalVars _ _type vars) = map muVarDecl vars

muStmt (StmtBlock block)               = muBlock block
muStmt (IfThen exp ifTrue)             = If (muExp exp) (muStmt ifTrue) MuNull
muStmt (IfThenElse exp ifTrue ifFalse) = If (muExp exp) (muStmt ifTrue) (muStmt ifFalse)
muStmt (While cond body)               = M.While (muExp cond) (muStmt body)
muStmt (Return exp)                    = M.Return $ fmapOrNull muExp exp
muStmt (ExpStmt exp)                   = muExp exp
muStmt Empty                           = MuNull
muStmt (Assert exp _)                  = SimpleSend Self "assert" [muExp exp]
muStmt (Synchronized _ block)          = muBlock block
muStmt (Labeled _ stmt)                = muStmt stmt
--muStmt (EnhancedFor _ _ name gen body) = Other
--Throw Exp
--Try Block [Catch] (Maybe Block)
--Switch Exp [SwitchBlock]
muStmt _                               = Other

muExp (Lit lit)                         = muLit lit
muExp (MethodInv invoke)                = muMethodInvocation invoke
muExp This                              = Self
muExp (BinOp arg1 op arg2)              = Send (muExp arg1) (muOp op) [muExp arg2]
muExp (Cond cond ifTrue ifFalse)        = If (muExp cond) (muExp ifTrue) (muExp ifFalse)
muExp (ExpName name)                    = muName name
muExp (Assign lhs EqualA exp)           = Assignment (muLhs lhs) (muExp exp)
muExp (InstanceCreation _ clazz args _) = SimpleNew (r clazz) (map muExp args)
muExp (PreNot exp)                      = SimpleSend (muExp exp) "!" []
muExp (Lambda params exp)               = M.Lambda (muLambdaParams params) (muLambdaExp exp)
muExp (MethodRef _ message)             = M.Lambda [VariablePattern "it"] (SimpleSend (Reference "it") (i message) [])
muExp _                                 = Other

muLambdaExp (LambdaExpression exp) = muExp exp
muLambdaExp (LambdaBlock block) = muBlock block

muLambdaParams (LambdaSingleParam name)     = [VariablePattern (i name)]
muLambdaParams (LambdaInferredParams names) = map (VariablePattern . i) names
muLambdaParams (LambdaFormalParams params)  = map (VariablePattern . muFormalParam) params

muLhs (NameLhs (Name names)) = ns names

muName (Name names) = Reference . ns $ names

muLit (String s)  = MuString s
muLit (Char c)    = MuString [c]
muLit (Int i)     = MuNumber (fromIntegral i)
muLit (Float d)   = MuNumber d
muLit (Double d)  = MuNumber d
muLit (Boolean b) = MuBool   b
muLit Null        = MuNull
muLit _           = Other

muOp Mult   = Reference "*"
muOp Div    = Reference "/"
muOp Rem    = Reference "rem"
muOp Add    = Reference "+"
muOp Sub    = Reference "-"
muOp LThan  = Reference "<"
muOp LThanE = Reference "<="
muOp GThan  = Reference ">"
muOp GThanE = Reference ">="
muOp And    = Reference "&&"
muOp Or     = Reference "||"
muOp Equal  = M.Equal
muOp NotEq  = NotEqual
muOp _      = Other

muVarDecl (VarDecl id init) = Variable (v id) (fmapOrNull muVarInit init)

muMethodBody (MethodBody (Just block)) = muBlock block

muVarInit (InitExp exp) = muExp exp
muVarInit (InitArray _ArrayInit) = Other

muMethodInvocation (MethodCall (Name [message]) args)           =  SimpleSend Self (i message) (map muExp args)
muMethodInvocation (MethodCall (Name (receptor:message)) args)  =  SimpleSend (Reference (i receptor)) (ns message) (map muExp args)
muMethodInvocation (PrimaryMethodCall receptor _ selector args) =  SimpleSend (muExp receptor) (i selector) (map muExp args)
muMethodInvocation _ = Other

muClassRefType (ClassRefType clazz) = r clazz

{-
Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
SuperMethodCall [RefType] Ident [Argument]
Invoking a method of the super class, giving arguments for any generic type parameters.
ClassMethodCall Name [RefType] Ident [Argument]
Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
TypeMethodCall Name [RefType] Ident [Argument]
Invoking a method of a named type, giving arguments for any generic type parameters.
-}

-- Combinators

fmapOrNull f = fromMaybe MuNull . fmap f

-- Helpers

v (VarId name) = i name
v (VarDeclArray id) =  (v id) ++ "[]"

i (Ident name) = name
r (ClassType [(name, _)]) = i name

j = parser compilationUnit

ns = intercalate "." . map i

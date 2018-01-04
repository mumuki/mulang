{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.Java (java, parseJava) where

import Language.Mulang.Ast hiding (While, Return, Equal, Lambda, Try, Switch)
import qualified Language.Mulang.Ast as M (Expression(While, Return, Equal, Lambda, Try, Switch))
import Language.Mulang.Parsers
import Language.Mulang.Builder (compact, compactMap, compactConcatMap)

import Language.Java.Parser
import Language.Java.Syntax

import Control.Fallible

import Data.Maybe (fromMaybe)
import Data.List (intercalate, partition)
import Data.List.Extra (headOrElse)
import Data.Char (toLower)

java :: Parser
java = orFail . parseJava'

parseJava :: EitherParser
parseJava = orLeft . parseJava'

parseJava' = fmap m . j

m (CompilationUnit _ _ typeDecls) = compactMap muTypeDecl $ typeDecls

muTypeDecl (ClassTypeDecl decl)    = muClassTypeDecl decl
muTypeDecl (InterfaceTypeDecl decl) = muInterfaceTypeDecl decl

muClassTypeDecl (ClassDecl _ name _ superclass interfaces (ClassBody body)) =
  Class (i name) (fmap muRefType superclass) (compact (map muImplements interfaces ++ concatMap muDecl body))
muClassTypeDecl (EnumDecl _ name _ (EnumBody constants _))                   =
  Enumeration (i name) (map muEnumConstant constants)

muImplements interface = Implement $ muRefType interface

muInterfaceTypeDecl (InterfaceDecl _ name _ interfaces (InterfaceBody body)) =
  Interface (i name) (map muRefType interfaces) (compactConcatMap muMemberDecl body )

muDecl :: Decl -> [Expression]
muDecl (MemberDecl memberDecl) = muMemberDecl memberDecl
muDecl (InitDecl _ _)          = return Other

muMemberDecl :: MemberDecl -> [Expression]
muMemberDecl (FieldDecl _ _type varDecls)                            = map (variableToAttribute.muVarDecl) varDecls
muMemberDecl (MethodDecl _ _ typ name params _ (MethodBody Nothing)) = return $ TypeSignature (i name) (map muFormalParamType params) (muMaybeType typ)
muMemberDecl (MethodDecl (elem Static -> True) _ Nothing (Ident "main") [_] _ body)
                                                                     = return $ EntryPoint "main" (muMethodBody body)
muMemberDecl (MethodDecl _ _ _ (Ident "equals") params _ body)       = return $ EqualMethod [SimpleEquation (map muFormalParam params) (muMethodBody body)]
muMemberDecl (MethodDecl _ _ _ (Ident "hashCode") params _ body)     = return $ HashMethod [SimpleEquation (map muFormalParam params) (muMethodBody body)]
muMemberDecl (MethodDecl _ _ _ name params _ body)                   = return $ SimpleMethod (i name) (map muFormalParam params) (muMethodBody body)
muMemberDecl (ConstructorDecl _ _ _ _params _ _constructorBody)      = return $ Other
muMemberDecl (MemberClassDecl decl)                                  = return $ muClassTypeDecl decl
muMemberDecl (MemberInterfaceDecl decl)                              = return $ muInterfaceTypeDecl decl

muEnumConstant (EnumConstant name _ _) = i name

muFormalParam (FormalParam _ _ _ id)      = VariablePattern (v id)
muFormalParamType (FormalParam _ typ _ _) = (muType typ)

muBlock (Block statements) = compactConcatMap muBlockStmt statements

muBlockStmt (BlockStmt stmt) = [muStmt stmt]
muBlockStmt (LocalClass decl) = [muClassTypeDecl decl]
muBlockStmt (LocalVars _ _type vars) = map muVarDecl vars

muMaybeType Nothing    = "void"
muMaybeType (Just typ) = muType typ

muType (PrimType t) = muPrimType t
muType (RefType t)  = muRefType t

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
muStmt (Throw exp)                     = Raise $ muExp exp
muStmt (Try block catches finally)     = M.Try (muBlock block) (map muCatch catches) (fmapOrNull muBlock finally)
muStmt (EnhancedFor _ _ name gen body) = For [Generator (VariablePattern (i name)) (muExp gen)] (muStmt body) 
muStmt (Switch exp cases)              = muSwitch exp . partition isDefault $ cases 
muStmt _                               = Other

muExp (Lit lit)                         = muLit lit
muExp (MethodInv invoke)                = muMethodInvocation invoke
muExp This                              = Self
muExp (BinOp arg1 op arg2)              = Send (muExp arg1) (muOp op) [muExp arg2]
muExp (Cond cond ifTrue ifFalse)        = If (muExp cond) (muExp ifTrue) (muExp ifFalse)
muExp (ExpName name)                    = muName name
muExp (Assign lhs EqualA exp)           = Assignment (muLhs lhs) (muExp exp)
muExp (InstanceCreation _ clazz args _) = New (r clazz) (map muExp args)
muExp (PreNot exp)                      = SimpleSend (muExp exp) "!" []
muExp (Lambda params exp)               = M.Lambda (muLambdaParams params) (muLambdaExp exp)
muExp (MethodRef _ message)             = M.Lambda [VariablePattern "it"] (SimpleSend (Reference "it") (i message) [])
muExp _                                 = Other

muLambdaExp (LambdaExpression exp) = muExp exp
muLambdaExp (LambdaBlock block) = muBlock block

muLambdaParams (LambdaSingleParam name)     = [VariablePattern (i name)]
muLambdaParams (LambdaInferredParams names) = map (VariablePattern . i) names
muLambdaParams (LambdaFormalParams params)  = map muFormalParam params

muCatch :: Catch -> (Pattern, Expression)
muCatch (Catch param block) = (TypePattern (muFormalParamType param), muBlock block)

muLhs (NameLhs (Name names)) = ns names

muName (Name names) = Reference . ns $ names

muLit (String s)  = MuString s
muLit (Char c)    = MuString [c]
muLit (Int i)     = MuNumber (fromIntegral i)
muLit (Float d)   = MuNumber d
muLit (Double d)  = MuNumber d
muLit (Boolean b) = MuBool   b
muLit Null        = MuNil
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

muMethodInvocation (MethodCall (Name [Ident "System", Ident "out", Ident "println"]) [expr])  = Print (muExp expr)
muMethodInvocation (MethodCall (Name [Ident "System", Ident "out", Ident "print"]) [expr])    = Print (muExp expr)
muMethodInvocation (MethodCall (Name [Ident "System", Ident "out", Ident "printf"]) (expr:_)) = Print (muExp expr)

muMethodInvocation (MethodCall (Name [message]) args)           =  SimpleSend Self (i message) (map muExp args)
muMethodInvocation (MethodCall (Name receptorAndMessage) args)  =  SimpleSend (Reference  (ns . init $ receptorAndMessage)) (i . last $ receptorAndMessage) (map muExp args)
muMethodInvocation (PrimaryMethodCall receptor _ selector args) =  SimpleSend (muExp receptor) (i selector) (map muExp args)
muMethodInvocation _ = Other

muRefType (ClassRefType clazz) = r clazz
muRefType (ArrayType t)        = (muType t) ++ "[]"

muPrimType = map toLower . dropLast 1 . show

muSwitch exp (def, cases) =  M.Switch (muExp exp) (map muCase cases) (headOrElse MuNull . map muDefault $ def)

muCase (SwitchBlock (SwitchCase exp) block) = (muExp exp, compactConcatMap muBlockStmt block)

muDefault (SwitchBlock Default block) = compactConcatMap muBlockStmt block

isDefault (SwitchBlock Default _) = True
isDefault _                       = False

-- Combinators

fmapOrNull f = fromMaybe MuNull . fmap f

-- Helpers

variableToAttribute (Variable id init) = Attribute id init

v (VarId name) = i name
v (VarDeclArray id) =  (v id) ++ "[]"

i (Ident name) = name
r (ClassType [(name, _)]) = i name

j = parser compilationUnit

ns = intercalate "." . map i

-- list helpers

dropLast n xs = take (length xs - n) xs

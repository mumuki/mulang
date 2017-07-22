{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.Java (java, parseJava) where

import Language.Mulang.Ast hiding (While, Return)
import qualified Language.Mulang.Ast as M (Expression(Return))
import Language.Mulang.Parsers
import Language.Mulang.Builder (compactMap, compactConcatMap)

import Language.Java.Parser
import Language.Java.Syntax

import Control.Fallible

import Data.Maybe (fromMaybe)

java :: Parser
java = orFail . parseJava'

parseJava :: MaybeParser
parseJava = orNothing . parseJava'

parseJava' = fmap m . j

m (CompilationUnit _ _ typeDecls) = compactMap muTypeDecl $ typeDecls

muTypeDecl (ClassTypeDecl decl)    = muClassTypeDecl decl
muTypeDecl (InterfaceTypeDecl decl) = muInterfaceTypeDecl decl

muClassTypeDecl (ClassDecl _ name _ superclass _interfaces (ClassBody body)) = Class (i name) (fmap r superclass) (compactMap muDecl body )
muClassTypeDecl (EnumDecl _ name _ (EnumBody constants _))                  = Enumeration (i name) (map muEnumConstant constants)

muInterfaceTypeDecl (InterfaceDecl _ name _ interfaces (InterfaceBody body)) = Interface (i name) (map r interfaces) (compactMap muMemberDecl body )

muDecl (MemberDecl memberDecl) = muMemberDecl memberDecl
muDecl (InitDecl _ _)          = Other

muMemberDecl (FieldDecl _ _type varDecls)                                     = Other
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

muStmt (StmtBlock block) = muBlock block
muStmt (IfThen _Exp _Stmt) = Other
muStmt (IfThenElse _Exp _Stmt1 _Stmt2) = Other
muStmt (While _Exp _Stmt) = Other
muStmt (BasicFor _MaybeForInit _MaybeExp _MaybeExps _Stmt) = Other
muStmt (Return exp) = M.Return $ fmapOrNull muExp exp

muExp (Lit (String s))  = MuString s
muExp (Lit (Char c))    = MuString [c]
muExp (Lit (Int i))     = MuNumber (fromIntegral i)
muExp (Lit (Float d))   = MuNumber d
muExp (Lit (Double d))  = MuNumber d
muExp (Lit (Boolean b)) = MuBool   b
muExp (Lit Null)        = MuNull
muExp _                 = Other

muVarDecl (VarDecl id init) = Variable (v id) (fmapOrNull muVarInit init)

muMethodBody (MethodBody (Just block)) = muBlock block

muVarInit (InitExp exp) = muExp exp
muVarInit (InitArray _ArrayInit) = Other

fmapOrNull f = fromMaybe MuNull . fmap f

v (VarId name) = i name
v (VarDeclArray id) =  (v id) ++ "[]"

i (Ident name) = name
r (ClassRefType (ClassType [(name, _)])) = i name

j = parser compilationUnit

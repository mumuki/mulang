module Language.Mulang.Parsers.Java (java, parseJava) where

import Language.Mulang.Ast hiding (While, Return)
import qualified Language.Mulang.Ast as M (Expression(Return))
import Language.Mulang.Parsers
import Language.Mulang.Builder (compact)

import Language.Java.Parser
import Language.Java.Syntax

import Control.Fallible

import Data.Maybe (fromMaybe)

java :: Parser
java = orFail . parseJava'

parseJava :: MaybeParser
parseJava = orNothing . parseJava'

--parseJava' :: a -> Either String Expression
parseJava' = fmap m . j

m (CompilationUnit _ _ typeDecls) = compact . map muTypeDecl $ typeDecls

muTypeDecl (ClassTypeDecl decl)    = muClassTypeDecl decl
muTypeDecl (InterfaceTypeDecl decl) = muInterfaceTypeDecl decl

muClassTypeDecl (ClassDecl _ name _ superclass interfaces (ClassBody body)) = Class (i name) (fmap r superclass) (compact.map muDecl $ body )
muClassTypeDecl (EnumDecl _ name _ (EnumBody constants _)) = Enumeration (i name) (map muEnumConstant constants)

muInterfaceTypeDecl (InterfaceDecl _ name _ interfaces (InterfaceBody body)) = Interface (i name) (map r interfaces) (compact.map muMemberDecl $ body )

muDecl (MemberDecl memberDecl) = muMemberDecl memberDecl
muDecl (InitDecl _ _)          = Other

muMemberDecl (FieldDecl _ _type varDecls) = Other
muMemberDecl (MethodDecl modifiers _ _ (Ident "main") [_args] _ (MethodBody (Just block))) | elem Static modifiers =
  EntryPoint "main" (muBlock block)
muMemberDecl (MethodDecl _ _ _ name params _ (MethodBody (Just block))) =
  SimpleMethod (i name) (map (VariablePattern . muFormalParam) params) (muBlock block)
muMemberDecl (MethodDecl _ _ _ name params _ (MethodBody Nothing))      =
  TypeSignature (i name) (map muFormalParam params) "void"
muMemberDecl (ConstructorDecl _ _ _ params _ constructorBody)           = Other
muMemberDecl (MemberClassDecl decl) = muClassTypeDecl decl
muMemberDecl (MemberInterfaceDecl decl) = muInterfaceTypeDecl decl

muEnumConstant (EnumConstant name _ _) = i name

muFormalParam (FormalParam _ types _ id) = (v id)

muBlock (Block statements) = compact  . map muBlockStmt $ statements

muBlockStmt (BlockStmt stmt) = muStmt stmt
muBlockStmt (LocalClass decl) = muClassTypeDecl decl
muBlockStmt (LocalVars _ _type _vars) = Other

muStmt (StmtBlock block) = muBlock block
muStmt (IfThen _Exp _Stmt) = Other
muStmt (IfThenElse _Exp _Stmt1 _Stmt2) = Other
muStmt (While _Exp _Stmt) = Other
muStmt (BasicFor _MaybeForInit _MaybeExp _MaybeExps _Stmt) = Other
muStmt (Return exp) = M.Return $ fromMaybe MuNull (fmap muExp exp)

muExp _ = MuString "hello"

v (VarId name) = i name
v (VarDeclArray id) =  (v id) ++ "[]"

i (Ident name) = name
r (ClassRefType (ClassType [(name, _)])) = i name

j = parser compilationUnit

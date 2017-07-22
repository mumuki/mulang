module Language.Mulang.Parsers.Java (java, parseJava) where

import Language.Mulang.Ast
import Language.Mulang.Parsers
import Language.Mulang.Builder (compact)

import Language.Java.Parser
import Language.Java.Syntax

import Control.Fallible

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
muClassTypeDecl (EnumDecl _ name _ body) = Other

muInterfaceTypeDecl (InterfaceDecl _ name _ interfaces (InterfaceBody body)) = Interface (i name) (map r interfaces) (compact.map muMemberDecl $ body )

muDecl (MemberDecl memberDecl) = muMemberDecl memberDecl
muDecl (InitDecl _ _)          = Other

muMemberDecl (FieldDecl _ _type varDecls) = Other
muMemberDecl (MethodDecl _ _ _ name params _ methodBody) = SimpleMethod (i name) [] MuNull
muMemberDecl (ConstructorDecl _ _ _ params _ constructorBody) = Other
muMemberDecl (MemberClassDecl decl) = muClassTypeDecl decl
muMemberDecl (MemberInterfaceDecl decl) = muInterfaceTypeDecl decl

i (Ident name) = name
r (ClassRefType (ClassType [(name, _)])) = i name

j = parser compilationUnit

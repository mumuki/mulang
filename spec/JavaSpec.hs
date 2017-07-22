{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module JavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang
--import           Language.Mulang.Parsers.Java

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

import Language.Mulang.Parsers
import Language.Mulang.Builder (compact)
import Control.Fallible
import Language.Java.Parser
import Language.Java.Syntax


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

run :: Text -> Expression
run = java . unpack

j = parser compilationUnit

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses Simple Class" $ do
      run "public class Foo {}" `shouldBe` Class "Foo" Nothing MuNull

    it "parsers Class With Superclass" $ do
      run "public class Foo extends Bar {}" `shouldBe` Class "Foo" (Just "Bar") MuNull

    it "parses Simple Interface" $ do
      run "public interface Foo {}" `shouldBe` Interface "Foo" [] MuNull

    it "parses Interface with superinterfaces" $ do
      run "public interface Foo extends Bar, Baz {}" `shouldBe` Interface "Foo" ["Bar", "Baz"] MuNull

    it "parses Class With Methods" $ do
      run [text|
            class Foo {
               public void hello() {}
            }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] MuNull)



{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module JavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Java

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = java . unpack


spec :: Spec
spec = do
  describe "parse" $ do
    it "parses Simple Class" $ do
      run "public class Foo {}" `shouldBe` Class "Foo" Nothing MuNull

    it "parsers Class With Superclass" $ do
      run "public class Foo extends Bar {}" `shouldBe` Class "Foo" (Just "Bar") MuNull

    it "parses Simple Interface" $ do
      run "public interface Foo {}" `shouldBe` Interface "Foo" [] MuNull

    it "parses Simple Interface With Messages" $ do
      run "public interface Foo { void foo(); }" `shouldBe` Interface "Foo" [] (TypeSignature "foo" [] "void")

    it "parses Interface with superinterfaces" $ do
      run "public interface Foo extends Bar, Baz {}" `shouldBe` Interface "Foo" ["Bar", "Baz"] MuNull

    it "parses Class With Methods" $ do
      run [text|
            class Foo {
               public void hello() {}
            }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] MuNull)

    it "parses Empty Returns" $ do
        run [text|class Foo {
               public void hello() { return; }
            }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return MuNull))

    it "parses Strings In Returns" $ do
      run [text|class Foo {
             public String hello() { return "hello"; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return (MuString "hello")))

    it "parses Int In Returns" $ do
      run [text|class Foo {
             public int hello() { return 1; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return (MuNumber 1)))

    it "parses Double In Returns" $ do
      run [text|class Foo {
             public double hello() { return 453.2; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return (MuNumber 453.2)))

    it "parses Bools In Returns" $ do
      run [text|class Foo {
             public boolean hello() { return true; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return MuTrue))

    it "parses Chars In Returns" $ do
      run [text|class Foo {
             public char hello() { return 'f'; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return (MuString "f")))

    it "parses Parameters" $ do
      run "public class Foo extends Bar { int succ(int y) {} }" `shouldBe` Class "Foo" (Just "Bar") (SimpleMethod "succ" [VariablePattern "y"] MuNull)

    it "parses Enums" $ do
      run "public enum Foo { A, B }" `shouldBe` Enumeration "Foo" ["A", "B"]

    it "parsesMain" $ do
      run [text|
          public class MyMain {
             public static void main(String[] args) { }
          }|] `shouldBe` Class "MyMain" Nothing (EntryPoint "main" MuNull)

    it "parses Variables And Ints" $ do
      run [text|
          class Foo {
             public void hello() { int x = 1; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Variable "x" (MuNumber 1)))

    it "parses Variables without initialization" $ do
      run [text|
          class Foo {
             public void hello() { int x; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Variable "x" MuNull))

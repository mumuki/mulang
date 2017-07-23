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

    it "parses Simple Interface With Messages With Params" $ do
      run "public interface Foo { void foo(String x, int y); }" `shouldBe` Interface "Foo" [] (TypeSignature "foo" ["String", "int"] "void")

    it "parses Interface with superinterfaces" $ do
      run "public interface Foo extends Bar, Baz {}" `shouldBe` Interface "Foo" [Reference "Bar", Reference "Baz"] MuNull

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

    it "parses Negation In Returns" $ do
      run [text|class Foo {
             public boolean hello() { return !true; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Return (SimpleSend MuTrue "!" [])))

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

    it "parses Variables And Ints" $ do
      run [text|
          class Foo {
             public void hello() { Foo x = this; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Variable "x" Self))

    it "parses Variables And ternaries" $ do
      run [text|
          class Foo {
             public void hello() { Foo x = true ? this : this; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Variable "x" (If MuTrue Self Self)))

    it "parses Variables without initialization" $ do
      run [text|
          class Foo {
             public void hello() { int x; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (Variable "x" MuNull))

    it "parses self-send" $ do
      run [text|
          class Foo {
             public void hello() { f(); }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (SimpleSend Self "f" []))

    it "parses self-send" $ do
      run [text|
          class Foo {
             public void hello() { f(2); }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (SimpleSend Self "f" [MuNumber 2]))

    it "parses explict self-send" $ do
      run [text|
          class Foo {
             public void hello() { this.f(); }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (SimpleSend Self "f" []))

    it "parses argument-send" $ do
      run [text|
          class Foo {
             public void hello(String g) { g.toString(); }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [VariablePattern "g"] (SimpleSend (Reference "g") "toString" []))

    it "parses expression-send" $ do
      run [text|
          class Foo {
             public void hello(String g) { g.size().toString(); }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [VariablePattern "g"] (SimpleSend (SimpleSend (Reference "g") "size" []) "toString" []))


    it "parses Ifs with empty braces" $ do
      run [text|
          class Foo {
             public void hello() { if (true) { } }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (If MuTrue MuNull MuNull))

    it "parses Ifs with return in braces" $ do
      run [text|
          class Foo {
             public void hello() { if (true) { return true; } else { return false; } }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (If MuTrue (Return MuTrue) (Return MuFalse)))

    it "parses Ifs with equal comparisons on conditions" $ do
      run [text|
          class Foo {
             public void hello(String x) { if (x == "foo") { } }
          }|] `shouldBe` Class "Foo" Nothing (
                           SimpleMethod "hello" [VariablePattern "x"] (
                             If (Send (Reference "x") Equal [MuString "foo"])
                              MuNull
                              MuNull))

    it "parses Ifs with not-equal comparisons on conditions" $ do
      run [text|
          class Foo {
             public void hello(String x) { if (x != "foo") { } }
          }|] `shouldBe` Class "Foo" Nothing (
                           SimpleMethod "hello" [VariablePattern "x"] (
                             If (Send (Reference "x") NotEqual [MuString "foo"])
                              MuNull
                              MuNull))

    it "parsesAssignmentsAndDoubles" $ do
      run [text|class Foo {
             public void hello() { double m = 1.0; m = 3.4; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (
                           Sequence [
                             Variable "m" (MuNumber 1.0),
                             Assignment "m" (MuNumber 3.4)]))

    it "parses Lambdas" $ do
      run [text|class Foo {
             public Object hello() { return (int x) -> x + 1; }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (
                           Return (Lambda [VariablePattern "x"] (SimpleSend (Reference "x") "+" [MuNumber 1]))))

    it "parses News" $ do
      run [text|class Foo {
             public Foo hello() { return new Bar(3); }
          }|] `shouldBe` Class "Foo" Nothing (SimpleMethod "hello" [] (
                           Return (SimpleNew "Bar" [MuNumber 3])))

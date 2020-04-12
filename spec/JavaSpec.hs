{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module JavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast hiding (Equal, NotEqual)
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.Java

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = java . unpack

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses Simple Class" $ do
      run "public class Foo {}" `shouldBe` Class "Foo" Nothing None

    it "parsers Class With Superclass" $ do
      run "public class Foo extends Bar {}" `shouldBe` Class "Foo" (Just "Bar") None

    it "parses Simple Interface" $ do
      run "public interface Foo {}" `shouldBe` Interface "Foo" [] None

    it "parses Simple Interface with type args" $ do
      run "public interface Foo<A> {}" `shouldBe` Sequence [
          ModuleSignature "Foo" ["A"],
          Interface "Foo" [] None]

    it "parses Simple Interface with complex type parametrization" $ do
      run "public interface Foo<A extends Comparable<? super T>> {}" `shouldBe` Sequence [
          ModuleSignature "Foo" ["A extends Comparable<? super T>"],
          Interface "Foo" [] None]

    it "parses Simple Class with type args" $ do
      run "public class Foo<A> {}" `shouldBe` Sequence [
          ModuleSignature "Foo" ["A"],
          Class "Foo" Nothing None]

    it "parses Simple Interface With Messages" $ do
      run "public interface Foo { void foo(); }" `shouldBe` Interface "Foo" [] (SubroutineSignature "foo" [] "void" [])

    it "parses Simple Interface With Non-Void Messages" $ do
      run "public interface Foo { int foo(); }" `shouldBe` Interface "Foo" [] (SubroutineSignature "foo" [] "int" [])

    it "parses Simple Interface With Messages With Params" $ do
      run "public interface Foo { void foo(String x, int y); }" `shouldBe` Interface "Foo" [] (SubroutineSignature "foo" ["String", "int"] "void" [])

    it "parses Interface with superinterfaces" $ do
      run "public interface Foo extends Bar, Baz {}" `shouldBe` Interface "Foo" ["Bar", "Baz"] None

    it "parses Class With initializers" $ do
      run [text|
            class Foo {
               static { System.out.println("hello"); }
            }|] `shouldBe` Class "Foo" Nothing (Print (MuString "hello"))

    it "parses Class With Methods" $ do
      run [text|
            class Foo {
               public void hello() {}
            }|] `shouldBe` Class "Foo" Nothing (Sequence [
                              SubroutineSignature "hello" [] "void" [],
                              (SimpleMethod "hello" [] None)])

    it "parses Methods with type arguments" $ do
      run [text|
            class Foo {
               public <A> A hello(A a) {}
            }|] `shouldBe` Class "Foo" Nothing (Sequence [
                              SubroutineSignature "hello" ["A"] "A" ["A"],
                              (SimpleMethod "hello" [VariablePattern "a"] None)])

    it "parses Methods with type arguments and type constraints" $ do
      run [text|
            class Foo {
               public <A extends Serializable> A hello(A a) {}
            }|] `shouldBe` Class "Foo" Nothing (Sequence [
                              SubroutineSignature "hello" ["A"] "A" ["A"],
                              (SimpleMethod "hello" [VariablePattern "a"] None)])

    it "parses Empty Returns" $ do
        run [text|class Foo {
               public void hello() { return; }
            }|] `shouldBe` Class "Foo" Nothing (Sequence [
                              SubroutineSignature "hello" [] "void" [],
                              (SimpleMethod "hello" [] (Return None))])

    it "parses Strings In Returns" $ do
      run [text|class Foo {
             public String hello() { return "hello"; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                                                SubroutineSignature "hello" [] "String" [],
                                                (SimpleMethod "hello" [] (Return (MuString "hello")))])

    it "parses Int In Returns" $ do
      run [text|class Foo {
             public int hello() { return 1; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "int" [],
                          (SimpleMethod "hello" [] (Return (MuNumber 1)))])


    it "parses Double In Returns" $ do
      run [text|class Foo {
             public double hello() { return 453.2; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "double" [],
                          (SimpleMethod "hello" [] (Return (MuNumber 453.2)))])


    it "parses Bools In Returns" $ do
      run [text|class Foo {
             public boolean hello() { return true; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "boolean" [],
                            (SimpleMethod "hello" [] (Return MuTrue))])

    it "parses Negation In Returns" $ do
      run [text|class Foo {
             public boolean hello() { return !true; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "boolean" [],
                          (SimpleMethod "hello" [] (Return (PrimitiveSend MuTrue Negation [])))])

    it "parses Chars In Returns" $ do
      run [text|class Foo {
             public char hello() { return 'f'; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "char" [],
                          (SimpleMethod "hello" [] (Return (MuChar 'f')))])

    it "parses equals methods invocations" $ do
      run [text|public class Foo {
            public static void main() {
              System.out.println((5).equals(6));
            }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "main" [] "void" [],
                          (SimpleMethod "main" [] (Print (PrimitiveSend (MuNumber 5) Equal [MuNumber 6])))])

    it "parses Parameters" $ do
      run "public class Foo extends Bar { int succ(int y) {} }" `shouldBe` Class "Foo" (Just "Bar") (Sequence [
                                                                              SubroutineSignature "succ" ["int"] "int" [],
                                                                              (SimpleMethod "succ" [VariablePattern "y"] None)])

    it "parses Enums" $ do
      run "public enum Foo { A, B }" `shouldBe` Enumeration "Foo" ["A", "B"]

    it "parsesMain" $ do
      run [text|
          public class MyMain {
             public static void main(String[] args) { }
          }|] `shouldBe` Class "MyMain" Nothing (EntryPoint "main" None)

    it "parses Variables And Ints" $ do
      run [text|
          class Foo {
             public void hello() { int x = 1; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "void" [],
                            (SimpleMethod "hello" [] (Sequence [
                              VariableSignature "x" "int" [],
                              Variable "x" (MuNumber 1)]))])

    it "parses Variables And Ints" $ do
      run [text|
          class Foo {
             public void hello() { Foo x = this; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                           SubroutineSignature "hello" [] "void" [],
                           (SimpleMethod "hello" [] (Sequence [
                              VariableSignature "x" "Foo" [],
                              Variable "x" Self]))])

    it "parses Variables And ternaries" $ do
      run [text|
          class Foo {
             public void hello() { Foo x = true ? this : this; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "void" [],
                            (SimpleMethod "hello" [] (Sequence [
                              VariableSignature "x" "Foo" [],
                              Variable "x" (If MuTrue Self Self)]))])

    it "parses Variables without initialization" $ do
      run [text|
          class Foo {
             public void hello() { int x; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "void" [],
                            (SimpleMethod "hello" [] (Sequence [
                              VariableSignature "x" "int" [],
                              Variable "x" None]))])

    it "parses self-send" $ do
      run [text|
          class Foo {
             public void hello() { f(); }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "void" [],
                            (SimpleMethod "hello" [] (SimpleSend Self "f" []))])

    it "parses self-send" $ do
      run [text|
          class Foo {
             public void hello() { f(2); }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "void" [],
                            (SimpleMethod "hello" [] (SimpleSend Self "f" [MuNumber 2]))])

    it "parses explict self-send" $ do
      run [text|
          class Foo {
             public void hello() { this.f(); }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "void" [],
                          (SimpleMethod "hello" [] (SimpleSend Self "f" []))])

    it "parses nested send" $ do
      run [text|
          class Foo {
            void foo() {
              System.err.println("hello");
            }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "foo" [] "void" [],
                            (SimpleMethod "foo" [] (SimpleSend  (Reference "System.err") "println" [MuString "hello"]))])

    it "parses argument-send" $ do
      run [text|
          class Foo {
             public void hello(String g) { g.toString(); }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" ["String"] "void" [],
                          (SimpleMethod "hello" [VariablePattern "g"] (SimpleSend (Reference "g") "toString" []))])


    it "parses expression-send" $ do
      run [text|
          class Foo {
             public void hello(String g) { g.size().toString(); }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" ["String"] "void" [],
                          (SimpleMethod "hello" [VariablePattern "g"] (SimpleSend (SimpleSend (Reference "g") "size" []) "toString" []))])


    it "parses Ifs with empty braces" $ do
      run [text|
          class Foo {
             public void hello() { if (true) { } }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "void" [],
                          (SimpleMethod "hello" [] (If MuTrue None None))])

    it "parses Ifs with return in braces" $ do
      run [text|
          class Foo {
             public void hello() { if (true) { return true; } else { return false; } }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "void" [],
                          (SimpleMethod "hello" [] (If MuTrue (Return MuTrue) (Return MuFalse)))])

    it "parses Ifs with equal comparisons on conditions" $ do
      run [text|
          class Foo {
             public void hello(String x) { if (x == "foo") { } }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                           SubroutineSignature "hello" ["String"] "void" [],
                           SimpleMethod "hello" [VariablePattern "x"] (
                             If (Send (Reference "x") (Primitive Equal) [MuString "foo"])
                              None
                              None)])

    it "parses Ifs with not-equal comparisons on conditions" $ do
      run [text|
          class Foo {
             public void hello(String x) { if (x != "foo") { } }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" ["String"] "void" [],
                            (SimpleMethod "hello" [VariablePattern "x"] (
                            If (Send (Reference "x") (Primitive NotEqual) [MuString "foo"])
                              None
                              None))])

    it "parsesAssignmentsAndDoubles" $ do
      run [text|class Foo {
             public void hello() { double m = 1.0; m = 3.4; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "void" [],
                            (SimpleMethod "hello" [] (
                                                     Sequence [
                                                       VariableSignature "m" "double" [],
                                                       Variable "m" (MuNumber 1.0),
                                                       Assignment "m" (MuNumber 3.4)]))])

    it "parses Lambdas" $ do
      run [text|class Foo {
             public Object hello() { return (int x) -> x + 1; }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "hello" [] "Object" [],
                            (SimpleMethod "hello" [] (
                            Return (Lambda [VariablePattern "x"] (PrimitiveSend (Reference "x") Plus [MuNumber 1]))))])

    it "parses News" $ do
      run [text|class Foo {
             public Foo hello() { return new Bar(3); }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "Foo" [],
                          SimpleMethod "hello" [] (
                           Return (New (Reference "Bar") [MuNumber 3]))])

    it "parses switch with default" $ do
      run [text|class Foo {
            public Foo hello() {
              switch(a){
                case 1: return 1;
                default: return 3;
              }
            }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "Foo" [],
                          SimpleMethod "hello" [] (
                           Switch (Reference "a") [(MuNumber 1, Return (MuNumber 1))] (Return (MuNumber 3.0)))])

    it "parses switch with no default" $ do
      run [text|class Foo {
            public Foo hello() {
              switch(a){
                case 1: return 1;
                case 2: return 2;
              }
            }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "Foo" [],
                          SimpleMethod "hello" [] (
                           Switch (Reference "a") [(MuNumber 1,Return (MuNumber 1)), (MuNumber 2, Return (MuNumber 2))] None)])

    it "parses for" $ do
      run [text|class Foo {
            public Foo hello() {
              for(int i : ints) a();
            }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "Foo" [],
                          SimpleMethod "hello" []
                            (For [Generator (VariablePattern "i") (Reference "ints")] (Send Self (Reference "a") []))])

    it "parses c-style for" $ do
      run [text|class Foo {
            public Foo hello() {
              for(int i = 0; true; a()) b();
            }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                          SubroutineSignature "hello" [] "Foo" [],
                          SimpleMethod "hello" []
                            (ForLoop
                              (Sequence [VariableSignature "i" "int" [], Variable "i" (MuNumber 0)])
                              MuTrue
                              (Send Self (Reference "a") []) (Send Self (Reference "b") []))])

    it "parses attributes" $ do
      run [text|class Foo {
             private int foo = 4;
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            VariableSignature "foo" "int" [],
                            Attribute "foo" (MuNumber 4)])

    it "parses attribute access" $ do
      run [text|class Foo {
             public int foo(M m) {
               return m.x;
             }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "foo" [] "void" ["M"],
                            (SimpleMethod "foo" [VariablePattern "x"] (FieldReference (Reference "m") "x"))])


    it "parses attribute assignment" $ do
      run [text|class Foo {
             public void foo(M m) {
                m.x = 3;
             }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "foo" ["M"] "void" [],
                            (SimpleMethod "foo" [VariablePattern "m"] (FieldAssignment (Reference "m") "x" (MuNumber 3)))])

    it "parses complex attribute assignment" $ do
      run [text|class Foo {
             public void foo(M m) {
                m.x.y = 3;
             }
          }|] `shouldBe` Class "Foo" Nothing (Sequence [
                            SubroutineSignature "foo" ["M"] "void" [],
                            (SimpleMethod "foo" [VariablePattern "m"] (FieldAssignment (FieldReference (Reference "m") "x") "y" (MuNumber 3)))])

    context "assertions" $ do
      let wrapped expression =  Class "Foo" Nothing (Sequence [
                                  TypeSignature "test" (ParameterizedType [] "void" []),
                                  SimpleMethod "test" [] expression])

      it "parses assertions" $ do
        run [text|
            class Foo {
              @Test
              void test(){
                assertTrue(true);
              }
            }|] `shouldBe` wrapped (Assert False (Truth (MuBool True)))

      it "parses assertions" $ do
        run [text|
            class Foo {
              @Test
              void test(){
                assertEquals(2, 2);
              }
            }|] `shouldBe` wrapped (Assert False (Equality (MuNumber 2) (MuNumber 2)))

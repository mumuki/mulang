module ExpectationsCompilerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import           Language.Mulang.Ast
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Java

spec :: Spec
spec = do
  let run code scope inspection = snd (compileExpectation (Expectation scope inspection)) code

  it "works with DeclaresEntryPoint" $ do
    run (hs "f x = 2") "*" "DeclaresEntryPoint" `shouldBe` False

  it "works with DeclaresProcedure" $ do
    run (hs "f x = 2") "*" "DeclaresProcedure" `shouldBe` False

  it "works with DeclaresAttribute on a class" $ do
    run (java "class Person { String name() { return \"name\"; } }") "Person" "DeclaresAttribute:name" `shouldBe` False
    run (java "class Person { String name; }") "Person" "DeclaresAttribute" `shouldBe` True
    run (java "class Person { String name; }") "Person" "DeclaresAttribute:name" `shouldBe` True

  it "works with DeclaresAttribute on an object" $ do
    run (js "var Person = {}") "Person" "DeclaresAttribute:name" `shouldBe` False
    run (js "var Person = {name: 'name'}") "Person" "DeclaresAttribute:name" `shouldBe` True

  it "works with DeclaresMethod on a class" $ do
    run (java "class Person { }") "Person" "DeclaresMethod:name" `shouldBe` False
    run (java "class Person { String name() { return \"name\"; } }") "Person" "DeclaresMethod:name" `shouldBe` True

  it "works with DeclaresMethod on empty code" $ do
    run (js "") "pepita" "DeclaresMethod:*" `shouldBe` False

  it "works with DeclaresMethod and explicitly anyone object" $ do
    run (js "var pepita = {volar:function(){}}") "pepita" "DeclaresMethod:*" `shouldBe` True
    run (js "var pepita = {}") "pepita" "DeclaresMethod:*" `shouldBe` False

  it "works with DeclaresMethod and named object" $ do
    run (js "var pepita = {volar:function(){}}") "pepita" "DeclaresMethod:volar" `shouldBe` True
    run (js "var pepita = {volar:function(){}}") "pepita" "DeclaresMethod:cantar" `shouldBe` False

  it "works with DeclaresMethod with scope" $ do
    run (js "var pepita = {volar:function(){}}") "pepita.volar" "DeclaresMethod" `shouldBe` False
    run (js "var pepita = {volar:function(){}}") "Intransitive:pepita.volar" "DeclaresMethod:^volar" `shouldBe` False
    run (js "var pepita = {volar:function(){}}") "pepita.cantar" "DeclaresMethod" `shouldBe` False

  it "works with DeclaresFunction in any scope and empty predicate" $ do
    run (hs "f x = 2") "*" "DeclaresFunction" `shouldBe` True

  it "works with DeclaresFunction in any scope and explicitly anyone predicate" $ do
    run (hs "f x = 2") "*" "DeclaresFunction:*" `shouldBe` True

  it "works with DeclaresFunction with non-empty scope and except predicate" $ do
    run (hs "f x = 2") "f" "DeclaresFunction:^f" `shouldBe` False
    run (hs "f x = 2") "g" "DeclaresFunction:^f" `shouldBe` False

  it "works with DeclaresFunction in any scope and non-empty predicate" $ do
    run (hs "f x = 2") "*" "DeclaresFunction:f" `shouldBe` True
    run (hs "f x = 2") "*" "DeclaresFunction:g" `shouldBe` False

  it "works with DeclaresFunction in any scope and explicitly equal predicate" $ do
    run (hs "f x = 2") "*" "DeclaresFunction:=f" `shouldBe` True
    run (hs "f x = 2") "*" "DeclaresFunction:=g" `shouldBe` False

  it "works with DeclaresFunction in any scope and explicitly like predicate" $ do
    run (hs "foo x = 2") "*" "DeclaresFunction:~fo" `shouldBe` True
    run (hs "foo x = 2") "*" "DeclaresFunction:~go" `shouldBe` False

  it "works with UsesWhile" $ do
    run (hs "f x = 2") "*" "UsesWhile" `shouldBe` False

  it "works with Not" $ do
    run (hs "f x = 2") "*" "Not:UsesWhile" `shouldBe` True
    run (hs "f x = 2") "f" "Not:UsesWhile" `shouldBe` True
    run (hs "f x = 2") "g" "Not:UsesWhile" `shouldBe` True

  it "works with Not:Declares" $ do
    run (hs "f x = 2") "g" "Not:Declares" `shouldBe` True
    run (hs "f x = 2") "f" "Not:Declares:^f" `shouldBe` True
    run (hs "f x = m where m = 2") "f" "Not:Declares" `shouldBe` False

  it "works with Not:DeclaresFunction" $ do
    run (hs "f x = 2") "*"  "Not:DeclaresFunction" `shouldBe` False
    run (hs "f x = 2") "f" "Not:DeclaresFunction" `shouldBe` True
    run (hs "f x = 2") "g" "Not:DeclaresFunction" `shouldBe` True

  it "works with Not:DeclaresFunction:f" $ do
    run (hs "f x = 2") "*"  "Not:DeclaresFunction:f" `shouldBe` False
    run (hs "f x = 2") "*"  "Not:DeclaresFunction:g" `shouldBe` True

  it "works with UsesSwitch" $ do
    run (hs "f x = 2") "*" "UsesSwitch" `shouldBe` False

  it "works with UsesRepeat" $ do
    run (hs "f x = 2") "*" "UsesRepeat" `shouldBe` False

  it "works with UsesLoop" $ do
    run (js "function a() { a() }") "*" "UsesLoop" `shouldBe` False

  it "works with UsesPatternMatching" $ do
    run (hs "f x = 2") "*" "UsesPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "*" "UsesPatternMatching" `shouldBe` True

  it "works with UsesPatternMatching, with scope" $ do
    run (hs "f x = 2") "f" "UsesPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "f" "UsesPatternMatching" `shouldBe` True

  it "works with DeclaresVariable" $ do
    run (js "var f = 2") "g" "DeclaresVariable" `shouldBe` False
    run (js "var f = 2") "f" "DeclaresVariable:^f" `shouldBe` False
    run (js "function f() { var m = 2; }") "f" "DeclaresVariable" `shouldBe` True
    run (js "function f() { var m = 2; }") "f" "DeclaresVariable:m" `shouldBe` True

  it "works with DeclaresComputationWithArity1" $ do
    run (hs "f a b = 2") "*" "DeclaresComputationWithArity1:f" `shouldBe` False
    run (hs "f a = 2")   "*" "DeclaresComputationWithArity1:f" `shouldBe` True

  it "works with DeclaresComputationWithArity" $ do
    run (hs "f a b = 2") "*" "DeclaresComputationWithArity1:f" `shouldBe` False
    run (hs "f a = 2")   "*" "DeclaresComputationWithArity1:f" `shouldBe` True
    run (hs "f a b = 2") "*" "DeclaresComputationWithArity2:f" `shouldBe` True
    run (hs "f a = 2")   "*" "DeclaresComputationWithArity2:f" `shouldBe` False

  it "works with DeclaresObject" $ do
    run (hs "f a b = 2") "f" "DeclaresObject" `shouldBe` False

  it "works with DeclaresClass" $ do
    run (hs "f a b = 2") "f" "DeclaresClass" `shouldBe` False

  it "works with Uses" $ do
    run (hs "f a b = g") "m" "Uses:g" `shouldBe` False
    run (hs "f a b = h") "f" "Uses:g" `shouldBe` False
    run (hs "f a b = g") "f" "Uses:g" `shouldBe` True
    run (hs "x = (*y) 10") "x" "Uses:y" `shouldBe` True
    run (hs "x = (*z) 10") "x" "Uses:y" `shouldBe` False
    run (java "class Foo{ void a(){ var = b(); } }") "a" "Uses:b" `shouldBe` True
    run (java "class Foo{ void a(){ var = c(); } }") "a" "Uses:b" `shouldBe` False
    run (java "class Foo{ void a(){ try{ b(); } } }") "a" "Uses:b" `shouldBe` True
    run (java "class Foo{ void a(){ try{} catch(Exception e) { b(); } } }") "a" "Uses:b" `shouldBe` True
    run (java "class Foo{ void a(){ try{} catch(Exception e) {} finally { b(); } } }") "a" "Uses:b" `shouldBe` True

  it "works with Calls" $ do
    run (hs "f a b = g") "m" "Calls:g" `shouldBe` False
    run (hs "f a b = h") "f" "Calls:g" `shouldBe` False
    run (hs "f a b = g") "f" "Calls:g" `shouldBe` False
    run (hs "f a b = g b") "f" "Calls:g" `shouldBe` True

  it "works with Calls WithNumber" $ do
    run (hs "f a b = g 1") "f" "Calls:g:WithNumber:1" `shouldBe` True
    run (hs "f a b = g 2") "f" "Calls:g:WithNumber:1" `shouldBe` False

  it "works with Calls WithNumber and WithTrue" $ do
    run (hs "f a b = g 1 True") "f" "Calls:g:WithNumber:1:WithTrue" `shouldBe` True
    run (hs "f a b = g 1 False") "f" "Calls:g:WithNumber:1:WithFalse" `shouldBe` True
    run (hs "f a b = g 2") "f" "Calls:g:WithNumber:1:WithTrue" `shouldBe` False
    run (hs "f a b = g True") "f" "Calls:g:WithNumber:1:WithFalse" `shouldBe` False

  it "works with Calls WithMath" $ do
    run (hs "f a = g (a + 2)") "f" "Calls:WithMath" `shouldBe` True
    run (hs "f a = g (a + 2)") "f" "Calls:g:WithMath" `shouldBe` True
    run (hs "f a = g a") "f" "Calls:g:WithMath" `shouldBe` False
    run (hs "f a = g 2") "f" "Calls:g:WithMath" `shouldBe` False

  it "works with Calls WithLiteral" $ do
    run (hs "f a = g (a + 2)") "f" "Calls:WithLiteral" `shouldBe` False
    run (hs "f a = g (a + 2)") "f" "Calls:g:WithLiteral" `shouldBe` False
    run (hs "f a = g a") "f" "Calls:g:WithLiteral" `shouldBe` False
    run (hs "f a = g 2") "f" "Calls:g:WithLiteral" `shouldBe` True
    run (hs "f a = g 'a'") "f" "Calls:g:WithLiteral" `shouldBe` True

  it "works with Calls WithNonliteral" $ do
    run (hs "f a = g (a + 2)") "f" "Calls:WithNonliteral" `shouldBe` True
    run (hs "f a = g (a + 2)") "f" "Calls:g:WithNonliteral" `shouldBe` True
    run (hs "f a = g a") "f" "Calls:g:WithNonliteral" `shouldBe` True
    run (hs "f a = g 2") "f" "Calls:g:WithNonliteral" `shouldBe` False
    run (hs "f a = g 'a'") "f" "Calls:g:WithNonliteral" `shouldBe` False

  it "works with Calls WithNumber and WithLogic" $ do
    run (hs "f a = g (a || b)") "f" "Calls:g:WithLogic" `shouldBe` True
    run (hs "f a = g a") "f" "Calls:g:WithLogic" `shouldBe` False
    run (hs "f a = g True") "f" "Calls:g:WithLogic" `shouldBe` False

  it "works with Assigns WithNumber" $ do
    run (java "class Foo { int x = 4; }") "Foo" "Assigns:x:WithNumber:4" `shouldBe` True
    run (java "class Foo { int x = 4; }") "Foo" "Assigns:x:WithNumber:5" `shouldBe` False

  it "works with Assigns WithSymbol" $ do
    run (Assignment "x" (MuSymbol "foo")) "*" "Assigns:x:WithSymbol:foo" `shouldBe` True
    run (Assignment "x" (MuSymbol "foo")) "*" "Assigns:x:WithSymbol:bar" `shouldBe` False

  it "works with Assigns WithChar" $ do
    run (java "class Foo { char x = 'a'; }") "Foo" "Assigns:x:WithChar:'a'" `shouldBe` True
    run (java "class Foo { char x = 'b'; }") "Foo" "Assigns:x:WithChar:'a'" `shouldBe` False

    run (java "class Foo { char x = 'b'; }") "Foo" "Assigns:*:WithChar:'b'" `shouldBe` True
    run (java "class Foo { char x = 'b'; }") "Foo" "Assigns:*:WithChar:'c'" `shouldBe` False

  it "works with Assigns WithString" $ do
    run (java "class Foo { char x = \"hello\"; }") "Foo" "Assigns:x:WithString:\"hello\"" `shouldBe` True
    run (java "class Foo { char x = \"world\"; }") "Foo" "Assigns:x:WithString:\"hello\"" `shouldBe` False

    run (java "class Foo { char x = \"world\"; }") "Foo" "Assigns:*:WithString:\"world\"" `shouldBe` True
    run (java "class Foo { char x = \"world\"; }") "Foo" "Assigns:*:WithString:\"hello\"" `shouldBe` False

  it "works with Returns WithNumber" $ do
    run (java "class Foo { int foo() { return 9; } }") "Foo" "Returns:WithNumber:9" `shouldBe` True
    run (java "class Foo { int foo() { int x = 4; } }") "Foo" "Returns:WithNumber:4" `shouldBe` False
    run (java "class Foo { int foo() { return 3; } }") "Foo" "Returns:WithNumber:0" `shouldBe` False

  it "works with UsesInheritance" $ do
    run (java "class Foo extends Bar {}") "Foo" "UsesInheritance" `shouldBe` True
    run (java "class Foo {}") "Foo" "UsesInheritance" `shouldBe` False

  it "works with transitive inspections" $ do
    run (hs "f a b = g\ng = m") "f" "Uses:h" `shouldBe` False
    run (hs "f a b = g\ng = h") "f" "Uses:h" `shouldBe` True

  it "works with transitive inspections" $ do
    run (hs "f a b = g\ng = h") "Intransitive:f" "Uses:h" `shouldBe` False
    run (hs "f a b = h") "Intransitive:f" "Uses:h" `shouldBe` True

  it "works with Declares" $ do
    run (hs "f a b = g") "g" "Declares" `shouldBe` False
    run (hs "f a b = g") "f" "Declares:^f" `shouldBe` False
    run (hs "f a b = g") "*" "Declares" `shouldBe` True

  it "works with Declares with object" $ do
    run (hs "f a b = g") "*"  "Declares:f" `shouldBe` True
    run (hs "f a b = g") "*"  "Declares:g" `shouldBe` False
    run (hs "f a b = g where g = 2") "f" "Declares:h" `shouldBe` False
    run (hs "f a b = g where g = 2") "f" "Declares:g" `shouldBe` True

  it "works with DeclaresComputation" $ do
    run (hs "f a b = g") "g" "DeclaresComputation" `shouldBe` False
    run (hs "f a b = g") "f" "DeclaresComputation" `shouldBe` False
    run (hs "f a b = g") "*"  "DeclaresComputation:f" `shouldBe` True

  it "works with DeclaresTypeSignature" $ do
    run (hs "f x = 1")  "*" "DeclaresTypeSignature:f" `shouldBe` False
    run (hs "f :: Int") "*" "DeclaresTypeSignature:f" `shouldBe` True

  it "works with UsesForComprehension" $ do
    run (hs "f :: Int") "f" "UsesForComprehension" `shouldBe` False

  it "works with UsesForLoop" $ do
    run (hs "f x = 2") "*" "UsesForLoop" `shouldBe` False

  it "works with DeclaresRecursively" $ do
    run (hs "f :: Int") "f" "DeclaresRecursively" `shouldBe` False

  it "works with UsesComposition" $ do
    run (hs "f :: Int") "f" "UsesComposition" `shouldBe` False

  it "works with UsesGuards" $ do
    run (hs "f 3 = 3") "f" "UsesGuards" `shouldBe` False

  it "works with UsesIf" $ do
    run (hs "f 3 = 3") "f" "UsesIf" `shouldBe` False

  it "works with UsesConditional" $ do
    run (hs "f 3 = 3") "f" "UsesConditional" `shouldBe` False

  it "works with returns" $ do
    run (js "function m() { }") "m" "Returns" `shouldBe` False
    run (js "function m() { return 3 }") "m" "Returns" `shouldBe` True

  it "works with UsesLambda" $ do
    run (hs "f 3 = 3") "f" "UsesLambda" `shouldBe` False

  it "works with Implements" $ do
    run (java "class Foo implements Bar {}") "Foo" "Implements:Bar" `shouldBe` True
    run (java "class Foo implements Bar {}") "Foo" "Implements:Baz" `shouldBe` False

  it "works with Print" $ do
    run (java "public class Foo { public static void main() { } }") "*" "UsesPrint" `shouldBe` False
    run (java "public class Foo { public static void main() { System.out.println((5).equals(6)); } }") "*" "UsesPrint" `shouldBe` True

  it "works with Inherits" $ do
    run (java "class Foo extends Bar {}") "Foo" "Inherits:Bar" `shouldBe` True
    run (java "class Foo extends Bar {}") "Foo" "Inherits:Baz" `shouldBe` False

  it "works with primitive operators in js" $ do
    run (js "x == 4") "*" "UsesEqual" `shouldBe` True
    run (js "x != 4") "*" "UsesEqual" `shouldBe` False

  it "works with primitive operators in hs" $ do
    run (hs "x = x . y") "*" "UsesForwardComposition" `shouldBe` False
    run (hs "x = x . y") "*" "UsesBackwardComposition" `shouldBe` True

  it "works with primitive operators usages in java" $ do
    run (java "public class Foo { public static void main() { System.out.println((5).equals(6)); } }") "*" "UsesHash" `shouldBe` False
    run (java "public class Foo { public static void main() { System.out.println((5).equals(6)); } }") "*" "UsesEqual" `shouldBe` True

  it "works with primitive operators declarations in java" $ do
    run (java "public class Foo { public boolean equals(Object other) { return false; } }") "*" "DeclaresEqual" `shouldBe` True
    run (java "public class Foo { public boolean equals(Object other) { return false; } }") "*" "DeclaresHash" `shouldBe` False

  it "does not mix keywords with inspections" $ do
    run (hs "type X = Int") "*" "Uses:type" `shouldBe` False

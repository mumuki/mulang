module SmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Generic.Smell
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Parsers.Prolog (pl)


javaStatement m = java ("class Foo { void bar() { " ++ m ++ " } }")

spec :: Spec
spec = do
  describe "hasRedundantIf" $ do
    it "is True when both branches are boolean literal returns" $ do
      hasRedundantIf (js "function x() { if(m) return true else return false }") `shouldBe` True
      hasRedundantIf (js "function x() { if(m) return false else return true }") `shouldBe` True

    it "is True when return an if with boolean literals" $ do
      hasRedundantIf (js "function x() { return m ? true : false }") `shouldBe` True

    it "is True when return an if with boolean literals, top level" $ do
      hasRedundantIf (js "m ? true : false") `shouldBe` True

    it "is True when return an if with boolean literals, in method" $ do
      hasRedundantIf (js "var y = {x: function(m){ return m ? true : false }}") `shouldBe` True

    it "is False when there is no if" $ do
      hasRedundantIf (js "var x = false") `shouldBe` False

    it "is False when there are no literals" $ do
      hasRedundantIf (js "function x() { if(m) 2 else 4 }") `shouldBe` False

    it "is True even if a variable is used" $ do
      hasRedundantIf (js "function x(b) { var r = true; if(b) { r = true; } else { r = false; }; return r; }") `shouldBe` True

    it "is True even if there is no return" $ do
      hasRedundantIf (js "function x(b) { var r = true; if(b) { r = true; } else { r = false; }; console.log(r) }") `shouldBe` True

    it "is True even if there is variable declaration" $ do
      hasRedundantIf (js "function x(b) { if(b) { r = true; } else { r = false; } }") `shouldBe` True

    it "is True when if present and both branches are boolean literals, hs" $ do
      hasRedundantIf (hs "x = if m then True else False") `shouldBe` True
      hasRedundantIf (hs "x = if m then False else True") `shouldBe` True

    it "is True, no return" $ do
      hasRedundantIf (js "function foo(x) { var y = x; if (x) { y = true  } else { y = false } }") `shouldBe` True

    it "is False when there is no if, hs" $ do
      hasRedundantIf (hs "x = False") `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in a function" $ do
      hasRedundantBooleanComparison (js "function x(m) { return m == true }") `shouldBe` True

    it "is True when comparing a literal, top level" $ do
      hasRedundantBooleanComparison (js "m == true") `shouldBe` True

    it "is False when no comparison" $ do
      hasRedundantBooleanComparison (js "function x(m) { return m }") `shouldBe` False

    it "is True when comparing a literal in an if, hs" $ do
      hasRedundantBooleanComparison (hs "x = if m == True then 1 else 2") `shouldBe` True

    it "is True when comparing a literal in an unguarded expression, hs" $ do
      hasRedundantBooleanComparison (hs "f x = x == True") `shouldBe` True

    it "is False on normal comparison, hs" $ do
      hasRedundantBooleanComparison (hs "f x = x == 2") `shouldBe` False

    it "is False when no comparison, hs" $ do
      hasRedundantBooleanComparison (hs "f x = True") `shouldBe` False

    it "is True when comparing self with a boolean, using a message" $ do
      hasRedundantBooleanComparison (Send Self Equal [MuBool True]) `shouldBe` True

    it "is True when comparing a boolean with a reference, using a message" $ do
      hasRedundantBooleanComparison (Send (MuBool False) NotEqual [Reference "x"]) `shouldBe` True

    it "is False when comparing references" $ do
      hasRedundantBooleanComparison (Send (Reference "y") Equal [Reference "x"]) `shouldBe` False

  describe "hasRedundantLocalVariableReturn" $ do
    it "is True when local variable is not necessary" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x  = 5; return x; }") `shouldBe` True

    it "is False when local variable is not necessary, but there are many variables" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; var y = 2; return x; }") `shouldBe` False

    it "is False when local variable is necessary in return" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; return x + x; }") `shouldBe` False

    it "is False when local variable is updated" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; x+= 1; return x; }") `shouldBe` False

    it "is False when local variable is used as a cache" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; var y = 1 + x; g(y); return x; }") `shouldBe` False

  describe "hasAssignmentReturn" $ do
    it "is True when return contains assignment" $ do
      hasAssignmentReturn (js "function x(m) { return x = 4 }") `shouldBe` True

    it "is False when return does not contain assignment" $ do
      hasAssignmentReturn (js "function x(m) { return x == 4 }") `shouldBe` False

  describe "returnsNull" $ do
    it "is True when returns null" $ do
      returnsNull (js "function x(m) { return null }") `shouldBe` True

    it "is True when returns undefined" $ do
      returnsNull (js "function x(m) { return undefined }") `shouldBe` True

    it "is False when returns a number" $ do
      returnsNull (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesNullTest" $ do
    it "is True when tests for null" $ do
      doesNullTest (js "function x(m) { if ( m == null) 1 else 2 } ") `shouldBe` True

    it "is True when tests for null with ===" $ do
      doesNullTest (js "function x(m) { if ( m === null) 1 else 2 } ") `shouldBe` True

    it "is False when not does null test" $ do
      doesNullTest (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesTypeTest" $ do
    it "is True when tests for string" $ do
      doesTypeTest (js "function x(m) { if ( m == \"foo\") 1 else 2 } ") `shouldBe` True

    it "is True when tests for string flipped with ===" $ do
      doesTypeTest (js "function x(m) { if ( \"foo\" === m) 1 else 2 } ") `shouldBe` True

    it "is False when not does type test" $ do
      doesTypeTest (js "function x(m) { return 1 }") `shouldBe` False

  describe "hasRedundantLambda" $ do
    it "is True whn etha-conversion applies, hs" $ do
      hasRedundantLambda (hs "g = map (\\m -> f m)") `shouldBe` True

    it "is True whn etha-conversion applies, whith composition, hs" $ do
      hasRedundantLambda (hs "g = map (\\m -> (f.h) m)") `shouldBe` True

    it "is False when it is an unavoidable lambda, hs" $ do
      hasRedundantLambda (hs "g = map (\\f -> f m)") `shouldBe` False

    it "is True whn etha-conversion applies, js" $ do
      hasRedundantLambda (js "(function(m) { return f(m) })") `shouldBe` True

    it "is False when it is an unavoidable lambda, js" $ do
      hasRedundantLambda (js "(function(m) { return m(f) })") `shouldBe` False

  describe "hasRedundantParameter" $ do
    it "is True on trivial application expressions" $ do
      hasRedundantParameter (hs "foo x = bar x") `shouldBe` True

    it "is True on complex application expressions" $ do
      hasRedundantParameter (hs "foo x = bar y x") `shouldBe` True

    it "is True on complex application expressions with multiple parameters" $ do
      hasRedundantParameter (hs "foo z x = bar y x") `shouldBe` True

    it "is False when parameter is not avoidable" $ do
      hasRedundantParameter (hs "foo x y = foo y x") `shouldBe` False


  describe "hasRedundantGuards" $ do
    it "is True when present and both branches are boolean literals" $ do
      hasRedundantGuards (hs "f x | c x = True\n\
                            \    | otherwise = False") `shouldBe` True

    it "is False when present but branches do not answers booleans" $ do
      hasRedundantGuards (hs "f x | c x = 2\n\
                             \    | otherwise = 3") `shouldBe` False

    it "is False when there is no guard" $ do
      hasRedundantGuards (hs "x = False") `shouldBe` False

  describe "discardsExceptions" $ do
    it "is True when there is an empty catch" $ do
      discardsExceptions (javaStatement "try { new Bar().baz(); } catch (Exception e) { /*TODO handle exception*/ }") `shouldBe` True

    it "is False when catch is non-emty" $ do
      discardsExceptions (javaStatement "try { new Bar().baz(); } catch (Exception e) { throw e; }") `shouldBe` False

    it "is False when there is no catch" $ do
      discardsExceptions (javaStatement "new Bar().baz();")  `shouldBe` False

  describe "doesConsolePrint" $ do
    it "is True java's system.out is used" $ do
      doesConsolePrint (javaStatement "int i = 4; System.out.println(4);") `shouldBe` True

    it "is False when no print is used" $ do
      doesConsolePrint (javaStatement "int i = 4; System.err.println(4);") `shouldBe` False

  describe "hasLongParameterList" $ do
    it "is True when a function has over 4 parameters" $ do
      hasLongParameterList (hs "f a b c d e = a") `shouldBe` True

    it "is False when no functions have over 4 parameters" $ do
      hasLongParameterList (hs "f a = a") `shouldBe` False

    it "is True when a method has over 4 parameters" $ do
      hasLongParameterList (java "public class a{ public void a(A a,B b,C c,D d, E e){} }") `shouldBe` True

    it "is False when no methods have over 4 parameters" $ do
      hasLongParameterList (java "public class a{ public void a(A a,B b,C c,D d){} }") `shouldBe` False

    it "is True when a predicate has over 4 parameters" $ do
      hasLongParameterList (pl "foo(A, B, C, D, E):- bar(A).") `shouldBe` True

    it "is False when no predicates have over 4 parameters" $ do
      hasLongParameterList (pl "foo(A):- bar(A).") `shouldBe` False
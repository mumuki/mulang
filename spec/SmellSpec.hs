{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module SmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast hiding (Equal, NotEqual)
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Inspector.Generic.Smell
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)
import           Language.Mulang.Parsers.Python (py)
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Parsers.Prolog (pl)

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

javaStatement m = java ("class Foo { void bar() { " ++ m ++ " } }")

runHaskell :: Text -> Expression
runHaskell = hs . unpack

spec :: Spec
spec = do
  describe "hasEqualIfBranches" $ do
    it "is True when both branches contain the same expression" $ do
      hasEqualIfBranches (js "if(m) { console.log('ok') } else { console.log('ok') }") `shouldBe` True

    it "is False when branches contain different expressions" $ do
      hasEqualIfBranches (js "if(m) { console.log('ok') } else { console.log('ups') }") `shouldBe` False

    it "is False when branches are empty" $ do
      hasEqualIfBranches (js "if(m) {  } else { }") `shouldBe` False

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

    it "is True when in incomplete if structures with return" $ do
      hasRedundantIf (js "function x() { if(m) { return true }; return false; }") `shouldBe` True

    it "is True when in incomplete if structures with return, negated" $ do
      hasRedundantIf (js "function x() { if(m) { return false }; return true; }") `shouldBe` True

    it "is False when in incomplete if structures with non consecutive returns" $ do
      hasRedundantIf (js "function x() { if(m) { return false }; console.log('hello'); return true; }") `shouldBe` False
      hasRedundantIf (js "function x() { if(m) { console.log('hello'); return false }; return true; }") `shouldBe` False

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
      hasRedundantBooleanComparison (PrimitiveSend Self Equal [MuBool True]) `shouldBe` True

    it "is True when comparing a boolean with a reference, using a message" $ do
      hasRedundantBooleanComparison (PrimitiveSend (MuBool False) NotEqual [Reference "x"]) `shouldBe` True

    it "is False when comparing references" $ do
      hasRedundantBooleanComparison (PrimitiveSend (Reference "y") Equal [Reference "x"]) `shouldBe` False

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

  describe "hasBrokenReturn" $ do
    it "is False when there are no functions" $ do
      hasBrokenReturn (js "let y = 0; if (x) { y = 0 } ") `shouldBe` False

    it "is False when there is a function that has only a return" $ do
      hasBrokenReturn (js "function f(){ return 4 } ") `shouldBe` False

    it "is False when there is a function that has a single flow and return" $ do
      hasBrokenReturn (js "function f(){ console.log(2); return 4 } ") `shouldBe` False

    it "is False when there is a function that has an if with no returns " $ do
      hasBrokenReturn (js "function f(){ if (x) { console.log(4) } else { console.log(5) } return 4 } ") `shouldBe` False

    it "is False when there is a function that has an if with no returns and no else " $ do
        hasBrokenReturn (js "function f(){ if (x) { console.log(4) } return 4 } ") `shouldBe` False

    it "is True when there is a function that has an if with a return and no else and not trailing return " $ do
        hasBrokenReturn (js "function f(){ if (x) { return 4 } } ") `shouldBe` True

    it "is True when there is a function that has an if with a return in else but not then and not trailing return " $ do
        hasBrokenReturn (js "function f(){ if (x) { } else { return 4 } }") `shouldBe` True

    it "is True when there is a function that nested ifs with incomplete return and no trailing return " $ do
        hasBrokenReturn (js "function f(){ if (x) { if (y) { return 5 } } else { return 4 } }") `shouldBe` True

    it "is False when there is a function that has an if with a return and no else and trailing return " $ do
        hasBrokenReturn (js "function f(){ if (x) { return 4 } return 5; } ") `shouldBe` False

    it "is False when there is a function that has an if with a return in else but not then and trailing return " $ do
        hasBrokenReturn (js "function f(){ if (x) { } else { return 4 } return 5; } ") `shouldBe` False

    it "is False when there is a function that nested ifs with complete return and no trailing return " $ do
        hasBrokenReturn (js "function f(){ if (x) { if (y) { return 5 } else { return 6 } } else { return 4 } }") `shouldBe` False


  describe "hasAssignmentCondition" $ do
    it "is True when assigns within an if condition" $ do
      hasAssignmentCondition (js "if (x = 4) {}") `shouldBe` True

    it "is True when assigns within a while condition" $ do
      hasAssignmentCondition (js "while (x = 4) {}") `shouldBe` True

    it "is False when not assigns within an if condition" $ do
      hasAssignmentCondition (js "if (x == 4) {}") `shouldBe` False
      hasAssignmentCondition (js "if (x === 4) {}") `shouldBe` False

    it "is False when not assigns within a while condition" $ do
      hasAssignmentCondition (js "while (x == 4) {}") `shouldBe` False
      hasAssignmentCondition (js "while (x === 4) {}") `shouldBe` False

  describe "hasAssignmentReturn" $ do
    it "is True when return contains assignment" $ do
      hasAssignmentReturn (js "function x(m) { return x = 4 }") `shouldBe` True

    it "is False when return does not contain assignment" $ do
      hasAssignmentReturn (js "function x(m) { return x == 4 }") `shouldBe` False

  describe "returnsNil" $ do
    it "is True when returns null" $ do
      returnsNil (js "function x(m) { return null }") `shouldBe` True

    it "is True when returns undefined" $ do
      returnsNil (js "function x(m) { return undefined }") `shouldBe` False

    it "is False when returns a number" $ do
      returnsNil (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesNilTest" $ do
    it "is True when tests for null" $ do
      doesNilTest (js "function x(m) { if ( m == null) 1 else 2 } ") `shouldBe` True

    it "is True when tests for null with ===" $ do
      doesNilTest (js "function x(m) { if ( m === null) 1 else 2 } ") `shouldBe` True

    it "is False when not does null test" $ do
      doesNilTest (js "function x(m) { return 1 }") `shouldBe` False

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

    it "is False when parameter appears more than once in the function" $ do
      hasRedundantParameter (hs "foo a = bar a a") `shouldBe` False

    it "is does not break when return is an argument-less application" $ do
      hasRedundantParameter (js "function a() { return b(); }") `shouldBe` False

  describe "hasRedundantGuards" $ do
    it "is True when present and both branches are boolean literals" $ do
      hasRedundantGuards (hs "f x | c x = True\n\
                            \    | otherwise = False") `shouldBe` True

    it "is False when present but branches do not answers booleans" $ do
      hasRedundantGuards (hs "f x | c x = 2\n\
                             \    | otherwise = 3") `shouldBe` False

    it "is False when there is no guard" $ do
      hasRedundantGuards (hs "x = False") `shouldBe` False

  describe "shouldUseOtherwise" $ do
    it "is True when there are two guards and last one is a True" $ do
      shouldUseOtherwise (hs "f x | c x = True\n\
                             \    | True = False") `shouldBe` True

    it "is True when there are three guards and last one is a True" $ do
      shouldUseOtherwise (hs "f x | c x = True\n\
                             \    | g x = False\n\
                             \    | True = False") `shouldBe` True

    it "is False when last guard is an otherwsie" $ do
      shouldUseOtherwise (hs "f x | c x = True\n\
                             \    | otherwise = False") `shouldBe` False

    it "is False when there no guards" $ do
      shouldUseOtherwise (hs "f x = True") `shouldBe` False

  describe "discardsExceptions" $ do
    it "is True when there is an empty catch" $ do
      discardsExceptions (javaStatement "try { new Bar().baz(); } catch (Exception e) { /*TODO handle exception*/ }") `shouldBe` True

    it "is False when catch is non-emty" $ do
      discardsExceptions (javaStatement "try { new Bar().baz(); } catch (Exception e) { throw e; }") `shouldBe` False

    it "is False when there is no catch" $ do
      discardsExceptions (javaStatement "new Bar().baz();")  `shouldBe` False

  describe "doesConsolePrint, in java" $ do
    it "is True when java's system.out is used" $ do
      doesConsolePrint (javaStatement "int i = 4; System.out.println(4);") `shouldBe` True

    it "is False when no print is used" $ do
      doesConsolePrint (javaStatement "int i = 4; System.err.println(4);") `shouldBe` False

  describe "doesConsolePrint, in javascript" $ do
    it "is True when console.log is used" $ do
      doesConsolePrint (js "console.log(4);") `shouldBe` True

    it "is False when no print is used" $ do
      doesConsolePrint (js "mylog.log(4);") `shouldBe` False

  describe "doesConsolePrint, in python" $ do
    it "is True when print is used" $ do
      doesConsolePrint (py "print(4);") `shouldBe` True

    it "is False when no print is used" $ do
      doesConsolePrint (py "prune(4);") `shouldBe` False


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

  describe "hasTooManyMethods" $ do
    it "is True when a class has over 15 methods" $ do
      hasTooManyMethods (java ("public class A{ public void a(){}" ++ (concat.replicate 16 $ "\npublic void a(){}") ++ " }")) `shouldBe` True

    it "is False when no classes have over 15 methods" $ do
      hasTooManyMethods (java "public class A{ public void a(){}\npublic void a(){} }") `shouldBe` False

  describe "overridesEqualsOrHashCodeButNotTheOther" $ do
    it "is False when neither is overridden" $ do
      overridesEqualOrHashButNotBoth (java ("public class A{ public void a(){} }")) `shouldBe` False

    it "is True when equals is overridden but not hashCode" $ do
      overridesEqualOrHashButNotBoth (java "public class A{ public void equals(){}\npublic void a(){} }") `shouldBe` True

    it "is True when hashCode is overridden but not equals" $ do
      overridesEqualOrHashButNotBoth (java "public class A{ public void hashCode(){}\npublic void a(){} }") `shouldBe` True

    it "is False when both are overriden" $ do
      overridesEqualOrHashButNotBoth (java "public class A{ public void equals(){}\npublic void hashCode(){} }") `shouldBe` False


  describe "shouldInvertIfCondition" $ do
    it "is True when if branch is empty but else isn't" $ do
      shouldInvertIfCondition (javaStatement "if(true) { } else { i++; }") `shouldBe` True
      shouldInvertIfCondition (js "if(true) { } else { i++; }") `shouldBe` True

    it "is False when it has no branches" $ do
      shouldInvertIfCondition (javaStatement "if(true);") `shouldBe` False
      shouldInvertIfCondition (js "if(true);") `shouldBe` False

    it "is False when it has empty branches" $ do
      shouldInvertIfCondition (javaStatement "if(true) {} else {}") `shouldBe` False
      shouldInvertIfCondition (js "if(true) {} else {}") `shouldBe` False

    it "is False when if branch is not empty" $ do
      shouldInvertIfCondition (javaStatement "if(true) { j++; } else { i++; }") `shouldBe` False
      shouldInvertIfCondition (js "if(true) { j++; } else { i++; }") `shouldBe` False

  describe "UsesNamedSelfReference" $ do
    it "is True when an object references itself by its name instead of using self" $ do
      usesNamedSelfReference (js "var x = { foo: function () { return x.bar(); } }") `shouldBe` True

    it "is False when an object references itself by using self" $ do
      usesNamedSelfReference (js "var x = { foo: function () { return self.bar(); } }") `shouldBe` False

    it "is False when reference is nested in another object" $ do
      usesNamedSelfReference (Object "Aves" (Object "Pepita" (SimpleMethod "foo" [] (Reference "Aves")))) `shouldBe` False

  describe "hasEmptyIfBranches" $ do
    it "is True when if branch is empty but else isn't" $ do
      hasEmptyIfBranches (javaStatement "if(true) { } else { i++; }") `shouldBe` False
      hasEmptyIfBranches (js "if(true) { } else { i++; }") `shouldBe` False

    it "is True when it has no branches" $ do
      hasEmptyIfBranches (javaStatement "if(true);") `shouldBe` True
      hasEmptyIfBranches (js "if(true);") `shouldBe` True

    it "is True when it has empty branches" $ do
      hasEmptyIfBranches (javaStatement "if(true) {}") `shouldBe` True
      hasEmptyIfBranches (js "if(true) {}") `shouldBe` True

    it "is False when if branch is not empty" $ do
      hasEmptyIfBranches (javaStatement "if(true) { j++; } else { i++; }") `shouldBe` False
      hasEmptyIfBranches (js "if(true) { j++; } else { i++; }") `shouldBe` False

  describe "hasRedundantRepeat" $ do
    it "is True when it contains a repeat with just one iteration" $ do
      hasRedundantRepeat (Repeat (MuNumber 1) (Print (MuString "hello"))) `shouldBe` True
    it "is False when it contains a repeat with two iterations" $ do
      hasRedundantRepeat (Repeat (MuNumber 2) (Print (MuString "hello"))) `shouldBe` False
    it "is False when it contains a repeat with a non-literal expression" $ do
      hasRedundantRepeat (Repeat (Reference "times") (Print (MuString "hello"))) `shouldBe` False

  describe "hasEmptyRepeat" $ do
    it "is True when it contains a repeat with an empty block" $ do
      hasEmptyRepeat (Repeat (MuNumber 2) None) `shouldBe` True

    it "is False when it contains a repeat with a non-empty block" $ do
      hasEmptyRepeat (Repeat (MuNumber 2) (Print (MuString "hello"))) `shouldBe` False

  describe "hasUnreachableCode" $ do
    it "is True when there's an equation following one which matches any values" $ do
      hasUnreachableCode (runHaskell [text|
        foo _ _ = 1
        foo 2 3 = 3
        |]) `shouldBe` True

    it "is False when no equation follows " $ do
      hasUnreachableCode (runHaskell [text|
        foo _ = 1
        |]) `shouldBe` False

    it "is False when not all patterns match any value" $ do
      hasUnreachableCode (runHaskell [text|
        foo _ 4 _ = 1
        foo 1 2 3 = 3
        |]) `shouldBe` False

    it "is False when guarded equation body that does not catch all values" $ do
      hasUnreachableCode (runHaskell [text|
        foo n _
          | n < 0 = 5
        foo 1 2 = 3
        |]) `shouldBe` False

    it "is True when guarded equation body that catches all values" $ do
      hasUnreachableCode (runHaskell [text|
        foo n _
          | n < 0 = 5
          | otherwise = 5
        foo 1 2 = 3
        |]) `shouldBe` True

    it "is True when there are consecutive returns in function" $ do
      hasUnreachableCode (js "function foo() { return 4; return 5; }") `shouldBe` True

    it "is True when there is code after return" $ do
      hasUnreachableCode (js "function foo() { return 4; console.log('hello') }") `shouldBe` True

    it "is False when there is code before return" $ do
        hasUnreachableCode (js "function foo() { console.log('hello'); return 4;  }") `shouldBe` False

    it "is True when there are non-consecutive returns in function" $ do
      hasUnreachableCode (js "function foo() { return 4; console.log('hello'); return 5; }") `shouldBe` True

    it "is False when there return in different sequences" $ do
      hasUnreachableCode (js "function foo() { if (true) { return 4; } console.log('hello'); return 5; }") `shouldBe` False

    it "is True when there are consecutive returns within a control structure" $ do
      hasUnreachableCode (js "function foo() { if (true) { return 4; return 5; } }") `shouldBe` True

    it "is False when there are no consecutive returns within a control structure" $ do
      hasUnreachableCode (js "function foo() { if (true) { return 4 } }") `shouldBe` False

    it "is True when there are consecutive returns plain code" $ do
      hasUnreachableCode (js "return 4; return 5;") `shouldBe` True

    it "is False when it has only a single return" $ do
      hasUnreachableCode (js "function foo() { return 4; }") `shouldBe` False

    it "is False when if branch is not empty" $ do
      hasUnreachableCode (js "if(true) { j++; } else { i++; }") `shouldBe` False

  describe "detectDeclarationTypos" $ do
    it "is [] when the identifier has been declared" $ do
      detectDeclarationTypos "foo" (js "function foo() {}") `shouldBe` []

    it "is [] when the identifier has been declared even if there is a similar declaration" $ do
      detectDeclarationTypos "foo" (js "function foo() {}\nfunction Foo() {}") `shouldBe` []

    it "is [] when the identifier has not been declared but there is no other declaration" $ do
      detectDeclarationTypos "foobar" (js "function foo() {}") `shouldBe` []

    it "is [] when the identifier has not been declared but there is no similar declaration" $ do
      detectDeclarationTypos "foobar" (js "function foo() {}\nfunction bar() {}") `shouldBe` []

    it "is non empty when the identifier has not been declared and there is a similar declaration" $ do
      detectDeclarationTypos "foo"  (js "function Foo() {}\nfunction bar() {}") `shouldBe` ["Foo"]
      detectDeclarationTypos "bar"  (js "function Foo() {}\nfunction baar() {}") `shouldBe` ["baar"]
      detectDeclarationTypos "br"   (js "function Foo() {}\nfunction bar() {}") `shouldBe` ["bar"]
      detectDeclarationTypos "baz"  (js "function Foo() {}\nfunction bar() {}\nfunction baaz() {}") `shouldBe` ["baaz", "bar"]

  describe "detectUsageTypos" $ do
    it "is [] when the identifier has been used" $ do
      detectUsageTypos "foo" (js "foo()") `shouldBe` []

    it "is [] when the identifier has been used even if there is a similar declaration" $ do
      detectUsageTypos "foo" (js "foo()\nFoo()") `shouldBe` []

    it "is [] when the identifier has not been used but there is no other declaration" $ do
      detectUsageTypos "foobar" (js "foo()") `shouldBe` []

    it "is [] when the identifier has not been used but there is no similar declaration" $ do
      detectUsageTypos "foobar" (js "foo()\nbar()") `shouldBe` []

    it "is non empty when the identifier has not been used and there is a similar usage" $ do
      detectUsageTypos "foo"  (js "Foo()\nbar()") `shouldBe` ["Foo"]
      detectUsageTypos "bar"  (js "Foo()\nbaar()") `shouldBe` ["baar"]
      detectUsageTypos "br"   (js "Foo()\nbar()") `shouldBe` ["bar"]
      detectUsageTypos "baz"  (js "Foo()\nbar()\nbaaz()") `shouldBe` ["bar", "baaz"]

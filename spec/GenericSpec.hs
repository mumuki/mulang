{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module GenericSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Parsers.Python (py2, py3)
import           Language.Mulang.Identifier
import           Language.Mulang.Inspector.Literal
import           Language.Mulang.Inspector.Matcher
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Inspector.Contextualized
import           Language.Mulang.Inspector.Generic
import           Language.Mulang.Inspector.Generic.Smell

spec :: Spec
spec = do
  describe "declaresEntryPoint" $ do
    describe "with program declarations" $ do
      it "is True when program is declared" $ do
        let code = EntryPoint "main" None

        declaresEntryPoint anyone code `shouldBe` True

      it "is False when program is not declared" $ do
        let code = js "function(){}"

        declaresEntryPoint anyone code `shouldBe` False

  describe "declaresVariable" $ do
    it "is True when declare a variable" $ do
      let code = js "function f(){ var x = 2}"

      declaresVariable (named "x") code `shouldBe` True

    it "is False when variable is not declared" $ do
      let code = js "function f(){ var x = 2}"

      declaresVariable (named "y") code `shouldBe` False

  describe "assigns" $ do
    it "is True when initializes a variable" $ do
      assigns (named "x") (Variable "x" (MuTrue)) `shouldBe` True

    it "is True when declares an attribute" $ do
      assigns (named "x") (MuObject (Attribute "x" (MuTrue))) `shouldBe` True

    it "is True when assigns a variable" $ do
      assigns (named "x") (Assignment "x" (MuTrue)) `shouldBe` True

    it "is False otherwise" $ do
      assigns (named "x") (Assignment "y" (MuTrue)) `shouldBe` False
      assigns (named "x") (Other Nothing Nothing) `shouldBe` False
      assigns (named "x") (MuFalse) `shouldBe` False

  describe "declaresFunction" $ do
    describe "with function declarations, hs" $ do
      it "is True when functions is declared" $ do
        declaresFunction (named "f") (hs "f x = 1") `shouldBe` True

    describe "with constants, hs" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction (named "f") (hs "f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        declaresFunction (named "f") (hs "f = \\x -> x + 1") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        declaresFunction (named "f") (hs "f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        declaresFunction (named "f") (hs "f = []") `shouldBe` False

      it "is False when constant is declared with a variable literal" $ do
        declaresFunction (named "f") (hs "f = snd") `shouldBe` False

    describe "with function declarations, js" $ do
      it "is True when functions is declared" $ do
        declaresFunction (named "f") (js "function f(x) {return 1}") `shouldBe` True

      it "is True when functions is declared" $ do
        declaresFunction (named "f") (js "function f(x) {return 1}") `shouldBe` True

      it "is True when any functions is declared" $ do
        declaresFunction anyone (js "function f(x) {return 1}") `shouldBe` True

      it "is False when functions is not declared" $ do
        declaresFunction (named "g") (js "function f(x) {return 1}") `shouldBe` False

    describe "with variables, js" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction (named "f") (js "var f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        declaresFunction (named "f") (js "var f = function(x) {}") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        declaresFunction  (named "f") (js "var f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        declaresFunction (named "f") (js "var f = []") `shouldBe` False

      it "is False when is a method" $ do
        declaresFunction (named "f") (js "var o = {f: function(){}}") `shouldBe` False

    describe "with matcher" $ do
      it "is True when using a matcher that matches" $ do
        (declaresFunctionMatching (with isSelf) anyone) (js "function f(x) { this; return 0; }") `shouldBe` True

      it "is False when using a matcher that does not match" $ do
        (declaresFunctionMatching (with isSelf) anyone) (js "function f(x) { return 0; }") `shouldBe` False

      it "is True when using a non literal matcher that matches" $ do
        (declaresFunctionMatching (with (returnsMatching (with (isNumber 2)))) anyone) (js "function f() { return 2; }") `shouldBe` True

      it "is False when using a non literal matcher that doesn't match" $ do
        (declaresFunctionMatching (with (returnsMatching (with (isNumber 2)))) anyone) (js "function f() { return 3; }") `shouldBe` False

      it "is False when using a literal matcher and it does not match literally" $ do
        (declaresFunctionMatching (with . isNumber $ 2) anyone) (js "function f() { return 2; }") `shouldBe` False

  describe "declaresComputationWithArity" $ do
    describe "with function declarations, hs" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresComputationWithArity 1) (named "f") (hs "f x = x + 1") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresComputationWithArity 2) (named "f") (hs "f x = x + 1") `shouldBe` False

    describe "with constant declaration, hs" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresComputationWithArity 2) (named "f") (hs "f = \\x y -> x + y") `shouldBe` True

      it "is False when constant is declared with lambda of given arity" $ do
        (declaresComputationWithArity 3) (named "f") (hs "f = \\x y -> x + y") `shouldBe` False

      it "is False if it is a variable" $ do
        (declaresComputationWithArity 1) (named "f") (hs "f = snd") `shouldBe` False

    describe "with function declarations, js" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresComputationWithArity 1) (named "f") (js "function f(x) { return x + 1 }") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresComputationWithArity 2) (named "f") (js "function f(x) { x + 1}") `shouldBe` False

    describe "with constant declaration, js" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresComputationWithArity 2) (named "f") (js "var f = function(x, y) { return x + y }") `shouldBe` True

  describe "isLongCode" $ do
    it "is False when the program has less than 16 nodes" $ do
      isLongCode (js "function f() { while(true) { console.log('foo') }  }")  `shouldBe` False

    it "is True when the program contains 16 or more nodes" $ do
      isLongCode (js "function f(){Poner(Verde) Mover(Norte) Poner(Verde)Mover(Norte) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde) Poner(Verde) Mover(Norte) Poner(Verde)Mover(Norte) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde)}")  `shouldBe` True

  describe "uses" $ do
    it "is True on direct usage in entry point" $ do
      uses (named "m") (EntryPoint "main" (Reference "m")) `shouldBe` True

    it "is False if there is no usage" $ do
      uses (named "m") (EntryPoint "main" (Reference "f")) `shouldBe` False

  describe "delegates'" $ do
    it "is True when used with a scope" $ do
      decontextualize (contextualized (scoped "main") (delegates' anyone)) (
        Sequence [
          EntryPoint "main" (Application (Reference "m") []),
          SimpleProcedure "m" [] (Return (MuNumber 4))]) `shouldBe` True

  describe "delegates" $ do
    context "when subroutine is declared" $ do
      it "is False when used with a scope" $ do
        scoped "main" (delegates anyone) (
          Sequence [
            EntryPoint "main" (Application (Reference "m") []),
            SimpleProcedure "m" [] (Return (MuNumber 4))]) `shouldBe` False

      it "is True on function application in entry point" $ do
        delegates (named "m") (Sequence [
                                  EntryPoint "main" (Application (Reference "m") []),
                                  SimpleProcedure "m" [] (Return (MuNumber 4))]) `shouldBe` True

      it "is True on message send in entry point" $ do
        delegates (named "m") (Sequence [
                                  EntryPoint "main" (Send Self (Reference "m") []),
                                  SimpleMethod "m" [] (Return (MuNumber 4))]) `shouldBe` True

      it "is False on message send in entry point to an empty method" $ do
        delegates (named "m") (Sequence [
                                  EntryPoint "main" (Send Self (Reference "m") []),
                                  SimpleMethod "m" [] None]) `shouldBe` False

      it "is False on direct usage in entry point" $ do
        delegates (named "m") (Sequence [
                                  EntryPoint "main" (Reference "m"),
                                  Class "m" Nothing (Return (MuNumber 4))]) `shouldBe` False

      it "is False if there is no usage" $ do
        delegates (named "m") (Sequence [
                                  EntryPoint "main" (Reference "f"),
                                  SimpleProcedure "m" [] (Return (MuNumber 4))]) `shouldBe` False

      it "is True when delegated and a wildcard is used" $ do
        delegates anyone (Sequence [
                              EntryPoint "main" (Application (Reference "m") []),
                              SimpleProcedure "m" [] (Return (MuNumber 4))]) `shouldBe` True

      it "is False when not delegated and a wildcard is used" $ do
        delegates anyone (Sequence [
                              EntryPoint "main" (Application (Reference "g") []),
                              SimpleProcedure "m" [] (Return (MuNumber 4))]) `shouldBe` False


    context "when subroutine is not declared" $ do
      it "is False on function application in entry point" $ do
        delegates (named "m") (EntryPoint "main" (Application (Reference "m") [])) `shouldBe` False

      it "is False on message send application in entry point" $ do
        delegates (named "m") (EntryPoint "main" (Send Self (Reference "m") [])) `shouldBe` False

      it "is False on direct usage in entry point" $ do
        delegates (named "m") (EntryPoint "main" (Reference "m")) `shouldBe` False

      it "is False if there is no usage" $ do
        delegates (named "m") (EntryPoint "main" (Reference "f")) `shouldBe` False


  describe "calls" $ do
    it "is True on function application in entry point" $ do
      calls (named "m") (EntryPoint "main" (Application (Reference "m") [])) `shouldBe` True

    it "is True on message send application in entry point" $ do
      calls (named "m") (EntryPoint "main" (Send Self (Reference "m") [])) `shouldBe` True

    it "is False on direct usage in entry point" $ do
      calls (named "m") (EntryPoint "main" (Reference "m")) `shouldBe` False

    it "is False if there is no usage" $ do
      calls (named "m") (EntryPoint "main" (Reference "f")) `shouldBe` False

    it "is True when using a matcher that matches" $ do
      (callsMatching (with . isNumber $ 1) anyone) (hs "f = g 1") `shouldBe` True

    it "is False when using a matcher that does not match" $ do
      (callsMatching (with . isNumber $ 1) anyone) (hs "f = g 2") `shouldBe` False

  describe "usesLogic" $ do
    it "is when it is used" $ do
      usesLogic (hs "f x y = x || y")   `shouldBe` True
      usesLogic (hs "f x y = x && y")   `shouldBe` True
      usesLogic (hs "f x y = not x")    `shouldBe` True
      usesLogic (hs "f x y = (not) x")  `shouldBe` True
      usesLogic (hs "f x y = (&&) x y") `shouldBe` True

    it "is is not used otherwise" $ do
      usesLogic (hs "f x y = x + y") `shouldBe` False
      usesLogic (hs "f x y = x")     `shouldBe` False
      usesLogic (hs "f x y = and x") `shouldBe` False
      usesLogic (hs "f x y = or x")  `shouldBe` False

  describe "usesMath" $ do
    it "is when it is used in function bodies" $ do
      usesMath (hs "f x y = x + y")    `shouldBe` True
      usesMath (hs "f x y = x * y")    `shouldBe` True
      usesMath (hs "f x y = x / x")    `shouldBe` True
      usesMath (hs "f x y = div x z")  `shouldBe` False -- TODO support mod/div operators in the future
      usesMath (hs "f x y = x - y")    `shouldBe` True

    it "is when it is used in composite literals" $ do
      usesMath (py3 "{'hello': 4 + 5}") `shouldBe` True
      usesMath (py3 "{'hello': 4}")     `shouldBe` False

      usesMath (js "{x: 4 + 5}")        `shouldBe` True
      usesMath (js "{x: 4}")            `shouldBe` False

      usesMath (js "[4+5, 0]")          `shouldBe` True
      usesMath (js "[9, 0]")            `shouldBe` False

    it "is is not used otherwise" $ do
      usesMath (hs "f x y = x")       `shouldBe` False
      usesMath (hs "f x y = plus x")  `shouldBe` False
      usesMath (hs "f x y = minus x") `shouldBe` False
      usesMath (hs "f x y = x || y")  `shouldBe` False

  describe "usesExceptions" $ do
    it "is True when a raise is used, java" $ do
      usesExceptions (java "class Sample { void aMethod() { throw new RuntimeException(); } }") `shouldBe` True

    it "is True when a raise is used, js" $ do
      usesExceptions (js "throw new Error()") `shouldBe` True

    it "is True when undefined is used, hs" $ do
      usesExceptions (hs "f = undefined") `shouldBe` True

    it "is True when error is used, hs" $ do
      usesExceptions (hs "f = error \"ups\"") `shouldBe` True

    it "is False when no raise is used, java" $ do
      usesExceptions (java "class Sample { void aMethod() {} }") `shouldBe` False

    it "is False when a raise is used, js" $ do
      usesExceptions (js "new Error()") `shouldBe` False

    it "is False when no raise is used, hs" $ do
      usesExceptions (hs "f = 4") `shouldBe` False

  describe "raises" $ do
    it "is True when raises an expected instance exception" $ do
      raises (named "RuntimeException") (java "class Sample { void aMethod() { throw new RuntimeException(); } }") `shouldBe` True

    it "is True when raises an expected exception class" $ do
      raises (named "Exception") (py2 "raise Exception") `shouldBe` True

    it "is True when raises an expected exception application, python" $ do
      raises (named "Exception") (py3 "raise Exception('ups')") `shouldBe` True

    it "is True when raises an expected exception application, js" $ do
      raises (named "Error") (js "throw Error('ups')") `shouldBe` True

  describe "rescues" $ do
    it "is True when rescues an expected exception" $ do
      rescues (named "RuntimeException") (java "class Sample { void aMethod() { try { foo(); } catch (RuntimeException e) { } } }") `shouldBe` True

    it "is False when rescues an unexpected exception" $ do
      rescues (named "RuntimeException") (java "class Sample { void aMethod() { try { foo(); } catch (Exception e) { } } }") `shouldBe` False

  describe "uses, hs" $ do
    it "is True when required function is used on application" $ do
      uses (named "m") (hs "y x = m x") `shouldBe` True

    it "is True when required function is used as argument" $ do
      uses (named "m") (hs "y x = x m") `shouldBe` True

    it "is False with primitives" $ do
      uses (named "&&") (hs "y x = x && z") `shouldBe` False

    it "is True when required function is used as operator" $ do
      uses (named "<>") (hs "y x = x <> z") `shouldBe` True

    it "is False when required function is not used in constant" $ do
      uses (named "m") (hs "y = 3") `shouldBe` False

    it "is False when required function is not used in function" $ do
      uses (named "m") (hs "y = x 3") `shouldBe` False

    it "is False when reference is not present, scoped" $ do
      scoped "p" (uses (named "m")) (hs "z = m 3") `shouldBe` False

    it "is False when required function is blank" $ do
      uses (named "" )(hs "y = m 3") `shouldBe` False

    it "is False when not present in enum" $ do
      uses (named "h") (hs "y = [a..b]") `shouldBe` False

    it "is True when is present in enum" $ do
      uses (named "h") (hs "y = [a..h]") `shouldBe` True

    it "is True when required constructor is used on application" $ do
      uses (named "Foo") (hs "y x = Foo x") `shouldBe` True

    it "is False when required constructor is not used on application" $ do
      uses (named "Foo") (hs "y x = Bar x") `shouldBe` False

    it "is True when required function is used on list comprehension" $ do
      uses (named "f") (hs "y x = [ f m | m <- ms  ]") `shouldBe` True

    it "is False when required function is not used on list comprehension" $ do
      uses (named "f") (hs "y x = [ g m | m <- ms  ]") `shouldBe` False

    it "is True when an identifier is used within a New expression" $ do
      uses (named "LinkedList") (New (Reference "LinkedList") []) `shouldBe` True

    it "is True when an identifier is used within an Include expression" $ do
      uses (named "Enumerable") (Include (Reference "Enumerable")) `shouldBe` True

    it "is True when an identifier is used within an Implement expression" $ do
      uses (named "Iterator") (Implement (Reference  "Iterator")) `shouldBe` True

    it "is False when variable is defined within scope" $ do
      --uses (named "m") (hs "y x = [ g m | m <- ms  ]") `shouldBe` False
      pending

    it "is False when there is variable hiding in list comprehension generator" $ do
      --uses (named "m") (hs "y x = [ g x | m <- ms, x <- f m]") `shouldBe` False
      pending

    it "is True when a function is used in a list comprehension generator" $ do
      uses (named "f") (hs "y x = [ g x | m <- ms, x <- f m]") `shouldBe` True

  describe "uses, js" $ do

    it "is True on direct usage in function" $ do
      uses (named "m") (js "function f(x) { m }") `shouldBe` True

    it "is True on direct call in function" $ do
      uses (named "m") (js "function f(x) { m() }") `shouldBe` True

    it "is True on negated call in function" $ do
      uses (named "m") (js "function f(x) { !m() }") `shouldBe` True

    it "is True on direct usage of something like it in function" $ do
      uses (like "m") (js "function f(x) { m2 }") `shouldBe` True

    it "is True on direct usage in method" $ do
      uses (named "m") (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is True on direct usage in method, scoped" $ do
      scoped "o" (uses (named "m")) (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped" $ do
      scoped "o" (uses (named "p")) (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is True on usage in method, scoped twice" $ do
      scopedList ["o", "z"] (uses (named "m")) (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped twice" $ do
      scopedList ["o", "z"] (uses (named "p")) (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is False on usage in wrong method, scoped twice" $ do
      scopedList ["o", "z"] (uses (named "m")) (js "var o = {p: function(x) { m }}") `shouldBe` False

    it "is True on usage in method, scoped twice" $ do
      transitiveList ["o", "z"] (uses (named "m")) (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped twice" $ do
      transitiveList ["o", "z"] (uses (named "p")) (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is False on usage in wrong method, scoped twice" $ do
      transitiveList ["o", "z"] (uses (named "m")) (js "var o = {p: function(x) { m }}") `shouldBe` False

    it "is True through function application in function" $ do
      transitive "f" (uses (named "m")) (js "function g() { m }; function f(x) { g() }") `shouldBe` True

    it "is True through function application in function" $ do
      transitive "f" (uses (named "m")) (js "function g(p) { return m }; function f(x) { return g(2) }") `shouldBe` True

    it "is False through function application in function" $ do
      transitive "f" (uses (named "m")) (js "function g() { m }; function f(x) { k() }") `shouldBe` False

    it "is True through message send in function" $ do
      transitive "f" (uses (named "m")) (js "var o = {g: function(){ m }}; function f(x) { o.g() }") `shouldBe` True

    it "is True through message send in objects" $ do
      transitive "p" (uses (named "m")) (js "var o = {g: function(){ m }}\n\
                                        \var p = {n: function() { o.g() }}") `shouldBe` True

  describe "usesPrimitive, hs" $ do
    it "is True when required primitive is used on application" $ do
      usesPrimitive And (hs "y x = x && z") `shouldBe` True
      usesPrimitive BackwardComposition (hs "y x = x . z") `shouldBe` True
      usesPrimitive Negation (hs "y x = not z") `shouldBe` True

    it "is True when required primitive is used as argument" $ do
      usesPrimitive And (hs "y x = f (&&) y z") `shouldBe` True

    it "is False when primitive is just apparently used" $ do
      usesPrimitive And (hs "y x = and x") `shouldBe` False

    it "is False when primitive is not used" $ do
      usesPrimitive Negation (hs "y x = m x") `shouldBe` False

  describe "usesPrimitive, js" $ do
    it "is True when required primitive is used on application" $ do
      usesPrimitive And (js "x && z") `shouldBe` True
      usesPrimitive Negation (js "function () { return !z }") `shouldBe` True

    it "is False when primitive is just apparently used" $ do
      usesPrimitive Or (js "or(x)") `shouldBe` False

    it "is False when primitive is not used" $ do
      usesPrimitive ForwardComposition (js "f(g(x))") `shouldBe` False

  describe "declaresComputation" $ do
    describe "with constants" $ do
      it "is False when exists" $ do
        declaresComputation (named "x") (hs "x = 1") `shouldBe` False

    describe "with type declarations" $ do
      it "is False when exists" $ do
        declaresComputation (named "x") (hs "x :: Int -> Int") `shouldBe` False

    describe "with function declarations" $ do
      it "is True when exists" $ do
        declaresComputation (named "x") (hs "x _ = True") `shouldBe` True

  describe "declares" $ do
    describe "with constants" $ do
      it "is True when exists" $ do
        declares (named "x") (hs "x = 1") `shouldBe` True

      it "is False when reference doesnt exists" $ do
        declares (named "y") (hs "x = 1") `shouldBe` False

    describe "with types signatures" $ do
      it "is False when just type signature exists" $ do
        declares (named "x") (hs "x :: Int") `shouldBe` False

    describe "with functions" $ do
      it "is True when exists" $ do
        declares (named "x") (hs "x m = 1") `shouldBe` True

      it "is False when reference doesnt exists" $ do
        declares (named "y") (hs "x m = 1") `shouldBe` False

  describe "parses" $ do
    it "is True when similar" $ do
      parses hs "x = map f . map g" (hs "x = map f.map g") `shouldBe` True

    it "is False when differ" $ do
      parses hs "x = map g . map f" (hs "x = map f . map g") `shouldBe` False

  describe "declaresRecursively" $ do
    it "is True when has direct recursion in unguarded expresion" $ do
      declaresRecursively (named "y") (hs "y x = y x") `shouldBe` True

    it "is True when has direct recursion in guarded expresion" $ do
      declaresRecursively (named "y") (hs "y x | c x = y m\n\
                              \    | otherwise = 0") `shouldBe` True

    it "is False when there is no named recursion" $ do
      declaresRecursively (named "y") (hs "y = 3") `shouldBe` False

    it "is False when there is no named recursion, scoped" $ do
      declaresRecursively (named "y") (hs "y = 3\nf x = f 4") `shouldBe` False

    it "is True when there is any recursion" $ do
      declaresRecursively anyone (hs "y x = y 3") `shouldBe` True

    it "is False when there is no recursion" $ do
      declaresRecursively anyone (hs "y x = 3") `shouldBe` False

  describe "usesIf, hs" $ do
    it "is True when present" $ do
      usesIf (hs "f x = if c x then 2 else 3") `shouldBe` True

    it "is False when not present" $ do
      usesIf (hs "f x = x") `shouldBe` False

  describe "usesIf, js" $ do
    it "is True when present in function" $ do
      let code = js "function f(){if(true){}else{}; return x;}"

      usesIf code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = js "function f(x){return 1;}"

      usesIf code  `shouldBe` False

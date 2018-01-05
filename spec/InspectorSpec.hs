module InspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Inspector.Generic.Smell

spec :: Spec
spec = do
  describe "declaresEntryPoint" $ do
    describe "with program declarations" $ do
      it "is True when program is declared" $ do
        let code = EntryPoint "main" MuNull

        declaresEntryPoint anyone code `shouldBe` True

      it "is False when program is not declared" $ do
        let code = js "function(){}"

        declaresEntryPoint anyone code `shouldBe` False

  describe "declaresProcedure" $ do
    describe "with procedure declarations" $ do
      it "is True when procedure is declared" $ do
        let code =  js "function f(){}"

        declaresProcedure (named "f") code `shouldBe` True

      it "is False when procedures is not declared" $ do
        let code = js "function f(){}"

        declaresProcedure (named "g") code `shouldBe` False

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      usesWhile (js "function f() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in lambda" $ do
      usesWhile (js "var f = function() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in object" $ do
      usesWhile (js "var x = {f: function() { while(true) { console.log('foo') } }}")  `shouldBe` True

    it "is True when present in method" $ do
      usesWhile (js "var o = {f: function() { while(true) { console.log('foo') }  }}")  `shouldBe` True

    it "is False when not present in function" $ do
      usesWhile (js "function f() {}")  `shouldBe` False

  describe "usesSwitch" $ do
    it "is True when present in function" $ do
      let code = Switch (Reference "x") [(MuNull, MuNumber 0)] MuNull

      usesSwitch code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = js "function f(x){return 1;}"

      usesSwitch code  `shouldBe` False

  describe "usesRepeat" $ do
    it "is True when present in function" $ do
      let code = SimpleFunction "f" [] (Sequence [Repeat (MuNumber 2) MuNull, Return (MuNumber 2)])

      usesRepeat code  `shouldBe` True

    it "is False when not present in function" $ do
      let code = js "function f(x){return 1;}"

      usesRepeat code  `shouldBe` False

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
      assigns (named "x") (Other) `shouldBe` False
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

  describe "declaresObject" $ do
    it "is True when present" $ do
      declaresObject (named "f")  (js "var f = {x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresObject (named "f") (js "var f = 6")  `shouldBe` False

    it "is False when not present, scoped" $ do
      declaresObject (named "f") (js "var g = {}")  `shouldBe` False

    it "is True when present, scoped" $ do
      declaresObject (named "g") (js "var g = {}")  `shouldBe` True

    it "is True when anyone present, scoped" $ do
      declaresObject anyone (js "var g = {}")  `shouldBe` True

  describe "declaresEnumeration" $ do
    it "is True when present" $ do
      declaresEnumeration (named "Direction") (Enumeration "Direction" ["SOUTH", "EAST", "WEST", "NORTH"]) `shouldBe` True

    it "is False when not present" $ do
      declaresEnumeration (named "Bird") (Class "Bird" (Just "Animal") MuNull) `shouldBe` False

  describe "declaresInterface" $ do
    it "is True when present" $ do
      declaresInterface (named "Optional") (java "interface Optional { Object get(); }") `shouldBe` True

    it "is False when not present" $ do
      declaresInterface (named "Bird") (java "class Bird extends Animal {}") `shouldBe` False

  describe "instantiates" $ do
    it "is True when instantiates" $ do
      instantiates (named "Bird") (java "class Main {  void main(String[] args) { Animal a = new Bird(); }  }") `shouldBe` True

    it "is False when not instantiates" $ do
      instantiates (named "Bird") (java "class Main {  void main(String[] args) { Animal a = new Mammal(); }  }") `shouldBe` False

  describe "implements" $ do
    it "is True when implements" $ do
      implements (named "Bird") (java "class Eagle implements Bird {}") `shouldBe` True

    it "is False when implements declaration not present" $ do
      implements (named "Bird") (java "class Cell {}") `shouldBe` False

    it "is False when a superinterface is declares" $ do
      implements (named "Iterable") (java "interface Collection extends Iterable {}") `shouldBe` False

  describe "usesInheritance" $ do
    it "is True when present" $ do
      usesInheritance (Class "Bird" (Just "Animal") MuNull) `shouldBe` True

    it "is False when not present" $ do
      usesInheritance (Class "Bird" Nothing MuNull) `shouldBe` False

    it "is True when present, scoped" $ do
      (scoped usesInheritance "Bird") (Sequence [Class "Bird" (Just "Animal") MuNull, Class "Fox" (Just "Animal") MuNull])  `shouldBe` True

    it "is True when present, scoped" $ do
      (scoped usesInheritance "Hercules") (Sequence [Class "Hercules" Nothing MuNull, Class "Fox" (Just "Animal") MuNull])  `shouldBe` False

  describe "usesMixins" $ do
    it "is True when include present" $ do
      usesMixins (Class "Dragon" Nothing (Include "FlyingCreature")) `shouldBe` True

    it "is False when include not present" $ do
      usesMixins (Class "Dragon" Nothing (Implement "FlyingCreature")) `shouldBe` False

  describe "declaresMethod" $ do
    it "is True when present" $ do
      declaresMethod (named "x") (js "var f = {x: function(){}}")  `shouldBe` True

    it "is True when present anyOf Identifiers" $ do
      declaresMethod (anyOf ["x", "y"]) (js "var f = {x: function(){}}")  `shouldBe` True
      declaresMethod (anyOf ["x", "y"]) (js "var f = {y: function(){}}")  `shouldBe` True

    it "is True when not present anyOf Identifiers" $ do
      declaresMethod (anyOf ["x", "y"]) (js "var f = {z: function(){}}")  `shouldBe` False

    it "is True when any present" $ do
      declaresMethod anyone (js "var f = {x: function(){}}")  `shouldBe` True

    it "is True when scoped in a class" $ do
      scoped (declaresMethod (named "foo")) "A" (java "class A { void foo() {} }")  `shouldBe` True

    it "is False when scoped in a class and not present" $ do
      scoped (declaresMethod (named "foo")) "A" (java "class A { void foobar() {} }") `shouldBe` False

    it "is False when not present" $ do
      declaresMethod (named "m") (js "var f = {x: function(){}}")  `shouldBe` False

    it "is False when not a method" $ do
      declaresMethod (named "m") (js "var f = {x: 6}")  `shouldBe` False

    it "is True when object present, scoped" $ do
      scoped (declaresMethod (named "x")) "f"  (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when object not present, scoped" $ do
      scoped (declaresMethod (named "x")) "p"  (js "var f = {x: function(){}}")  `shouldBe` False

  describe "declaresAttribute" $ do
    it "is True when present" $ do
      declaresAttribute (named "x") (js "var f = {x: 6}")  `shouldBe` True

    it "is True when present and there are many" $ do
      declaresAttribute (named "x") (js "var f = {j: 20, x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresAttribute (named "m") (js "var f = {x: 6}")  `shouldBe` False

    it "is True when attribute present, scoped" $ do
      scoped (declaresAttribute (named "x")) "f"  (js "var f = {x: 6}")  `shouldBe` True

    it "is True when any attribute present, scoped" $ do
      scoped (declaresAttribute anyone) "f"  (js "var f = {x: 6}")  `shouldBe` True

    it "is False when attribute not present, scoped" $ do
      scoped (declaresAttribute (named "x")) "g" (js "var f = {x: 6}")  `shouldBe` False

  describe "uses" $ do
    it "is True on direct usage in entry point" $ do
      uses (named "m") (EntryPoint "main" (Reference "m")) `shouldBe` True

    it "is False if there is no usage" $ do
      uses (named "m") (EntryPoint "main" (Reference "f")) `shouldBe` False

  describe "calls" $ do
    it "is True on function application in entry point" $ do
      calls (named "m") (EntryPoint "main" (Application (Reference "m") [])) `shouldBe` True

    it "is True on message send application in entry point" $ do
      calls (named "m") (EntryPoint "main" (Send Self (Reference "m") [])) `shouldBe` True

    it "is False on direct usage in entry point" $ do
      calls (named "m") (EntryPoint "main" (Reference "m")) `shouldBe` False

    it "is False if there is no usage" $ do
      calls (named "m") (EntryPoint "main" (Reference "f")) `shouldBe` False

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
    it "is True when raises an expected exception" $ do
      raises (named "RuntimeException") (java "class Sample { void aMethod() { throw new RuntimeException(); } }") `shouldBe` True

    it "is False when raises an unexpected exception" $ do
      raises (named "RuntimeException") (java "class Sample { void aMethod() { throw new Exception(); } }") `shouldBe` False


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

    it "is True when required function is used as operator" $ do
      uses (named "&&" )(hs "y x = x && z") `shouldBe` True

    it "is False when required function is not used in constant" $ do
      uses (named "m") (hs "y = 3") `shouldBe` False

    it "is False when required function is not used in function" $ do
      uses (named "m") (hs "y = x 3") `shouldBe` False

    it "is False when reference is not present, scoped" $ do
      scoped (uses (named "m")) "p" (hs "z = m 3") `shouldBe` False

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
      uses (named "LinkedList") (New "LinkedList" []) `shouldBe` True

    it "is True when an identifier is used within an Include expression" $ do
      uses (named "Enumerable") (Include "Enumerable") `shouldBe` True

    it "is True when an identifier is used within an Implement expression" $ do
      uses (named "Iterator") (Implement "Iterator") `shouldBe` True

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
      scoped (uses (named "m")) "o" (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped" $ do
      scoped (uses (named "p")) "o" (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is True on usage in method, scoped twice" $ do
      scopedList (uses (named "m")) ["o", "z"] (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped twice" $ do
      scopedList (uses (named "p")) ["o", "z"] (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is False on usage in wrong method, scoped twice" $ do
      scopedList (uses (named "m")) ["o", "z"] (js "var o = {p: function(x) { m }}") `shouldBe` False

    it "is True through function application in function" $ do
      transitive (uses (named "m")) "f" (js "function g() { m }; function f(x) { g() }") `shouldBe` True

    it "is True through function application in function" $ do
      transitive (uses (named "m")) "f" (js "function g(p) { return m }; function f(x) { return g(2) }") `shouldBe` True

    it "is False through function application in function" $ do
      transitive (uses (named "m")) "f" (js "function g() { m }; function f(x) { k() }") `shouldBe` False

    it "is True through message send in function" $ do
      transitive (uses (named "m")) "f" (js "var o = {g: function(){ m }}; function f(x) { o.g() }") `shouldBe` True

    it "is True through message send in objects" $ do
      transitive (uses (named "m")) "p" (js "var o = {g: function(){ m }}\n\
                                        \var p = {n: function() { o.g() }}") `shouldBe` True


  describe "declaresTypeSignature" $ do
    it "is True when type signature is present" $ do
      declaresTypeSignature (named "x") (hs "x :: Int\n\
                           \x = 3") `shouldBe` True

    it "is True when type signature just signature is present" $ do
      declaresTypeSignature (named "x") (hs "x :: Int") `shouldBe` True

    it "is False when type signature is absent " $ do
      declaresTypeSignature (named "x") (hs "x = 2") `shouldBe` False

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

  describe "usesForComprehension" $ do
    it "is True when list comprehension exists" $ do
      usesForComprehension (hs "x = [m|m<-t]") `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      usesForComprehension (hs "x = []") `shouldBe` False

    it "is True when do syntax is used" $ do
      usesForComprehension (hs "y = do { x <- xs; return x }") `shouldBe` True

  describe "usesComprehension" $ do
    it "is True when list comprehension exists" $ do
      usesComprehension (hs "x = [m|m<-t]") `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      usesComprehension (hs "x = []") `shouldBe` False

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

  describe "usesComposition" $ do
    describe "when constant assignment" $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "x = y . z") `shouldBe` True

      it "is True when composition is present inside lambda" $ do
        usesComposition (hs "x = \\m -> y . z") `shouldBe` True

      it "is True when composition is present inside application" $ do
        usesComposition (hs "x = f (g.h) x") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "x = 1") `shouldBe` False

    describe "when unguarded function" $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "f x = (g . f) x") `shouldBe` True

      it "is True when composition is present within if" $ do
        usesComposition (hs "f x = if True then (g . f) x else 5") `shouldBe` True

      it "is True when composition is present within list" $ do
        usesComposition (hs "f x = [(g.h x), m]") `shouldBe` True

      it "is True when composition is present within comprehension" $ do
        usesComposition (hs "f x = [ (g.h x) m | m <- [1..20]]") `shouldBe` True

      it "is True when composition is present within where" $ do
        usesComposition (hs "f x = m\n\
                           \      where m = (f.g) ") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "f x = g x") `shouldBe` False

    describe "when guarded function " $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "f x | c x = g . f $ x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is True when composition is present on guard" $ do
        usesComposition (hs "f x | (c . g) x = g x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "f x | c x = f x\n\
                           \    | otherwise = 4") `shouldBe` False

  describe "usesGuards" $ do
    describe "detects guards when" $ do
      it "is present" $ do
        usesGuards (hs "f x | c x = 2\n\
                      \    | otherwise = 4") `shouldBe` True

      it "is present" $ do
        usesGuards (hs "f x = c x == 2") `shouldBe` False

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


  describe "lambda analyzer" $ do
    describe "detects lambdas when" $ do
      it "is present" $ do
        usesLambda (hs "f x = \\y -> 4") `shouldBe` True

      it "is present" $ do
        usesLambda (hs "f x = 4") `shouldBe` False


  describe "usesAnonymousVariable" $ do
    it "is True if _ is present in paramenters" $ do
      usesAnonymousVariable (hs "foo _ = 1") `shouldBe` True

    it "is True if _ is present in nested list patterns" $ do
      usesAnonymousVariable (hs "foo [3, _] = 1") `shouldBe` True

    it "is True if _ is present in nested infix application patterns" $ do
      usesAnonymousVariable (hs "foo (x:_) = 1") `shouldBe` True

    it "is True if _ is present in nested application patterns" $ do
      usesAnonymousVariable (hs "foo (F _ 1) = 1") `shouldBe` True

    it "is True if _ is present in nested tuple patterns" $ do
      usesAnonymousVariable (hs "foo (_, 1) = 1") `shouldBe` True

    it "is True if _ is present in nested at patterns" $ do
      usesAnonymousVariable (hs "foo x@(_, 1) = 1") `shouldBe` True

    it "is False if _ is not present in parameters" $ do
      usesAnonymousVariable (hs "foo x = 1") `shouldBe` False

    it "is False if _ is present only in seccond equation" $ do
      let code = hs . unlines $ ["foo False bool = bool", "foo True _ = True"]
      usesAnonymousVariable code `shouldBe` True

    it "is False if there is no _ but a comment" $ do
      usesAnonymousVariable (hs "foo x = 1\n--") `shouldBe` False

    it "is False if there is only a comment" $ do
      usesAnonymousVariable (hs "--") `shouldBe` False

  describe "usesPatternMatching" $ do
    it "is True when there Pattern Matching on List" $ do
      usesPatternMatching (hs "foo [] = 0\nfoo (x:xs) = 1 + foo xs") `shouldBe` True

    it "is False when there not Pattern Matching" $ do
      usesPatternMatching (hs "foo x = 2") `shouldBe` False

    it "is True when there Pattern Matching on Maybe" $ do
      usesPatternMatching (hs "foo Nothing = 0\nfoo (Just x) = 1") `shouldBe` True

    it "is True when there there Pattern Matching on anonima variable" $ do
      usesPatternMatching (hs "baz _ = 5 + 8") `shouldBe` True




module JSCompilerSpec (spec) where

  import           Test.Hspec
  import           Language.Mulang.Ast
  import           Language.Mulang.JSCompiler
  
  spec :: Spec
  spec = do
    -- It would be much better to test this by running the JS code instead of checking the string output...
    -- Also, I don't care about output format right now, so all \n are ignored.
    describe "JS Compilation" $ do

      it "MuNumber" $ do
        (MuNumber 5) `shouldBeCompiledTo` "new MuNumber(5.0)"

      it "MuNumber" $ do
        (MuBool True) `shouldBeCompiledTo` "new MuBool(true)"
        
      it "MuString" $ do
        (MuString "foo") `shouldBeCompiledTo` "new MuString(\"foo\")"

      it "MuSymbol" $ do
        (MuSymbol "foo") `shouldBeCompiledTo` "new MuSymbol(\"foo\")"

      it "MuTuple" $ do
        (MuTuple [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBeCompiledTo` "new MuTuple([new MuNumber(1.0), new MuNumber(2.0), new MuNumber(3.0)])"

      it "MuList" $ do
        (MuList [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBeCompiledTo` "new MuList([new MuNumber(1.0), new MuNumber(2.0), new MuNumber(3.0)])"

      it "EntryPoint" $ do
        (EntryPoint "foo" (MuBool True)) `shouldBeCompiledTo` "function foo() { return new MuBool(true) }"
        
      it "TypeSignature" $ do
        (TypeSignature "foo" [] "bar") `shouldBeCompiledTo` ""

      it "TypeAlias" $ do
        (TypeAlias "foo") `shouldBeCompiledTo` ""

      it "Record" $ do
        (Record "foo") `shouldBeCompiledTo` ""

      it "Function" $ do
        (Function "f" []) `shouldBeCompiledTo` "function f() {try {  throw new MuPatternMatchError() }catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }"
        (Function "f" [Equation [] (UnguardedBody (Return (MuNumber 5)))]) `shouldBeCompiledTo` (
          "function f() {try { " ++
            "if(arguments.length === 0){  return function(){ throw new MuReturn(new MuNumber(5.0)) }() } " ++
            "throw new MuPatternMatchError() " ++
          "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }"
          )
        (Function "f" [Equation [VariablePattern "x"] (UnguardedBody $ MuNumber 5)]) `shouldBeCompiledTo` (
          "function f() {try { " ++
            "if(arguments.length === 1 && function(){ return true }(arguments[0])){ var x = arguments[0]; return new MuNumber(5.0) } " ++
            "throw new MuPatternMatchError() " ++
          "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }"
          )
        (Function "f" [Equation [VariablePattern "x", VariablePattern "y"] (UnguardedBody $ MuNumber 5)]) `shouldBeCompiledTo` (
          "function f() {try { " ++
            "if(arguments.length === 2 && function(){ return true }(arguments[0]) && function(){ return true }(arguments[1])){ var x = arguments[0];var y = arguments[1]; return new MuNumber(5.0) } " ++
            "throw new MuPatternMatchError() " ++
          "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }"
          )
        (Function "f" [Equation [] (UnguardedBody (Sequence [If (Reference "c") (Return (Reference "x")) (Sequence []), Reference "y"]))]) `shouldBeCompiledTo` (
          "function f() {try { " ++
            "if(arguments.length === 0){  " ++
              "return function(){ " ++
                "function(){ if(c) { return function(){ throw new MuReturn(x) }() } else { return undefined } }(); " ++
                "return y " ++
              "}() " ++
            "} " ++
            "throw new MuPatternMatchError() " ++
          "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }"
          )
        pending --TODO: Other cases

      it "Procedures" $ do pending

      it "Variable" $ do
        (Variable "x" $ Reference "y") `shouldBeCompiledTo` "var x = y"

      it "Assignment" $ do
        (Assignment "x" $ Reference "y") `shouldBeCompiledTo` "x = y"

      it "Reference" $ do
        (Reference "x") `shouldBeCompiledTo` "x"

      it "Application" $ do
        (Application (Reference "f") []) `shouldBeCompiledTo` "f()"
        (Application (Reference "f") [Reference "x", Reference "y"]) `shouldBeCompiledTo` "f(x, y)"
        (Application (Application (Reference "f") [Reference "x"]) [Application (Reference "g") [Reference "y"]]) `shouldBeCompiledTo` "f(x)(g(y))"

      it "Send" $ do
        (Send (Reference "o") (Reference "m") []) `shouldBeCompiledTo` "o['m']()"
        (Send (Reference "o") (Reference "m") [Reference "x", Reference "y"]) `shouldBeCompiledTo` "o['m'](x, y)"
        (Send (Send (Reference "o") (Reference "m") [Reference "x"]) (Reference "n") [Reference "y"]) `shouldBeCompiledTo` "o['m'](x)['n'](y)"

      it "New" $ do
        (New "C" []) `shouldBeCompiledTo` "new C()"
        (New "C" [Reference "x", Reference "y"]) `shouldBeCompiledTo` "new C(x, y)"

      it "Raise" $ do
        (Raise $ New "E" []) `shouldBeCompiledTo` "function(){ throw new E() }()"

      it "Print" $ do
        (Print $ Reference "x") `shouldBeCompiledTo` "console.log(x)"

      it "MuNull" $ do
        MuNull `shouldBeCompiledTo` "new MuNull()"

      it "Sequence" $ do pending

      it "If" $ do
        (If (Reference "c") (Reference "x") (Reference "y")) `shouldBeCompiledTo` "function(){ if(c) { return x } else { return y } }()"

      it "While" $ do
        (While (Reference "c") (Reference "x")) `shouldBeCompiledTo` "function(){ while(c) { x } }()"

      it "Repeat" $ do
        (Repeat (Reference "c") (Reference "x")) `shouldBeCompiledTo` "function(){ for(var $$i=0; $$i < c; $$i++) { x } }()"

  shouldBeCompiledTo expression expected = (fmap (filter (/='\n')) . toJS) expression `shouldBe` Just expected
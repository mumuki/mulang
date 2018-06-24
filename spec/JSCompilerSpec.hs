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

      it "Enumeration" $ do
        (Enumeration "E" ["A", "B", "C"]) `shouldBeCompiledTo` (
          "var E = { A: 0, B: 1, C: 2 }; " ++
          "var A = E['A'];" ++
          "var B = E['B'];" ++
          "var C = E['C'];"
          )

      it "EntryPoint" $ do
        (EntryPoint "foo" (MuBool True)) `shouldBeCompiledTo` "function foo() { return new MuBool(true) }"

      it "TypeSignature" $ do
        (VariableSignature "foo" "bar" []) `shouldBeCompiledTo` ""

      it "TypeAlias" $ do
        (TypeAlias "foo" "bar") `shouldBeCompiledTo` ""

      it "Record" $ do
        (Record "foo") `shouldBeCompiledTo` ""

      describe "Function" $ do
        it "Function with no equations" $ do
          (Function "f" []) `shouldBeCompiledTo` "function f() {try {  throw new MuPatternMatchError() }catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }"

        it "Simple Function that returns 5" $ do
          (SimpleFunction "f" [] (Return (MuNumber 5))) `shouldBeCompiledTo` (
            "function f() {try { " ++
              "if(arguments.length === 0){  return function(){ throw new MuReturn(new MuNumber(5.0)) }() } " ++
              "throw new MuPatternMatchError() " ++
            "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }")

        it "Function with arguments" $ do
          (SimpleFunction "f" [VariablePattern "x"] (MuNumber 5)) `shouldBeCompiledTo` (
            "function f() {try { " ++
              "if(arguments.length === 1 && function(){ return true }(arguments[0])){ var x = arguments[0]; return new MuNumber(5.0) } " ++
              "throw new MuPatternMatchError() " ++
            "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }")

        it "Function two arguments" $ do
          (SimpleFunction "f" [VariablePattern "x", VariablePattern "y"] (MuNumber 5)) `shouldBeCompiledTo` (
            "function f() {try { " ++
              "if(arguments.length === 2 && function(){ return true }(arguments[0]) && function(){ return true }(arguments[1])){ var x = arguments[0];var y = arguments[1]; return new MuNumber(5.0) } " ++
              "throw new MuPatternMatchError() " ++
            "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }")

        it "Function with an if" $ do
          (SimpleFunction "f" [] (Sequence [If (Reference "c") (Return (Reference "x")) (Sequence []), Reference "y"])) `shouldBeCompiledTo` (
            "function f() {try { " ++
              "if(arguments.length === 0){  " ++
                "return function(){ " ++
                  "function(){ if(c) { return function(){ throw new MuReturn(x) }() } else { return undefined } }(); " ++
                  "return y " ++
                "}() " ++
              "} " ++
              "throw new MuPatternMatchError() " ++
            "}catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } }")

        it "Other scenarios" $ do
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
        (New (Reference "C") []) `shouldBeCompiledTo` "new C()"
        (New (Reference "C") [Reference "x", Reference "y"]) `shouldBeCompiledTo` "new C(x, y)"

      it "Raise" $ do
        (Raise $ New (Reference "E") []) `shouldBeCompiledTo` "function(){ throw new E() }()"

      it "Print" $ do
        (Print $ Reference "x") `shouldBeCompiledTo` "console.log(x)"

      it "MuNil" $ do
        MuNil `shouldBeCompiledTo` "new MuNil()"

      it "Sequence" $ do pending

      it "If" $ do
        (If (Reference "c") (Reference "x") (Reference "y")) `shouldBeCompiledTo` "function(){ if(c) { return x } else { return y } }()"

      it "While" $ do
        (While (Reference "c") (Reference "x")) `shouldBeCompiledTo` "function(){ while(c) { x } }()"

      it "Repeat" $ do
        (Repeat (Reference "c") (Reference "x")) `shouldBeCompiledTo` "function(){ for(var $$i=0; $$i < c; $$i++) { x } }()"

      it "Class" $ do
        (Class "C" Nothing MuNil) `shouldBeCompiledTo` (
          "function C() {  " ++
            "this.constructor.prototype = Object.create(MuObject.prototype);  MuObject.call(this);  " ++
          "}"
          )

        (Class "C" (Just "S") MuNil) `shouldBeCompiledTo` (
          "function C() {  " ++
            "this.constructor.prototype = Object.create(S.prototype);  S.call(this);  " ++
          "}"
          )

        (Class "C" Nothing $ Sequence [Method "m" [Equation [] (UnguardedBody $ Reference "x")]]) `shouldBeCompiledTo` (
          "function C() {  " ++
            "this.constructor.prototype = Object.create(MuObject.prototype);  MuObject.call(this);  " ++
            "this.constructor.prototype['m'] = function () {  " ++
              "try { if(arguments.length === 0){  return x } throw new MuPatternMatchError() }  " ++
              "catch($error) { if($error.constructor === MuReturn) { return $error.value } else { throw $error } } " ++
            "}" ++
          "}"
          )

        (Class "C" Nothing $ Sequence [Attribute "f" $ Reference "x"]) `shouldBeCompiledTo` (
          "function C() {  " ++
            "this.constructor.prototype = Object.create(MuObject.prototype);  MuObject.call(this);  " ++
            "this['f'] = x" ++
          "}"
          )

        (Class "C" Nothing $ Sequence [Include (Reference "M")]) `shouldBeCompiledTo` (
          "function C() {  " ++
            "this.constructor.prototype = Object.create(MuObject.prototype);  MuObject.call(this);  " ++
            "this.constructor.prototype = Object.assign(this.constructor.prototype, Object.create(M.prototype));  M.call(this)" ++
          "}"
          )

      it "Object" $ do
        (Object "o" MuNil) `shouldBeCompiledTo` (
          "var o = new function () {  " ++
            "this.constructor.prototype = Object.create(MuObject.prototype);  MuObject.call(this);  " ++
          "}()"
          )

  shouldBeCompiledTo expression expected = (fmap (filter (/='\n')) . toJS) expression `shouldBe` Just expected

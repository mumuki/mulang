module JSCompilerSpec (spec) where

  import           Test.Hspec
  import           Language.Mulang.Ast
  import           Language.Mulang.JSCompiler
  
  spec :: Spec
  spec = do
    -- It would be much better to test this by running the JS code instead of checking the string output...
    -- Also, I don't care about output format right now, so all \n are there because they want to.
    describe "JS Compilation" $ do

      it "MuNumber" $ do
        toJS (MuNumber 5) `shouldBe` Just "new MuNumber(5.0)\n"

      it "MuNumber" $ do
        toJS (MuBool True) `shouldBe` Just "new MuBool(true)\n"
        
      it "MuString" $ do
        toJS (MuString "foo") `shouldBe` Just "new MuString(\"foo\")\n"

      it "MuSymbol" $ do
        toJS (MuSymbol "foo") `shouldBe` Just "new MuSymbol(\"foo\")\n"

      it "MuTuple" $ do
        toJS (MuTuple [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` Just "new MuTuple([new MuNumber(1.0)\n, new MuNumber(2.0)\n, new MuNumber(3.0)])\n"

      it "MuList" $ do
        toJS (MuList [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` Just "new MuList([new MuNumber(1.0)\n, new MuNumber(2.0)\n, new MuNumber(3.0)])\n"

      it "EntryPoint" $ do
        toJS (EntryPoint "foo" (MuBool True)) `shouldBe` Just "function foo() { return new MuBool(true) }\n"
        
      it "TypeSignature" $ do
        toJS (TypeSignature "foo" [] "bar") `shouldBe` Just ""

      it "TypeAlias" $ do
        toJS (TypeAlias "foo") `shouldBe` Just ""

      it "Record" $ do
        toJS (Record "foo") `shouldBe` Just ""

      it "Function" $ do
        toJS (Function "f" []) `shouldBe` Just "function f() {  throw new MuPatternMatchError() }\n"
        toJS (Function "f" [Equation [] (UnguardedBody (Return (MuNumber 5)))]) `shouldBe` Just (
          "function f() { " ++
            "if(arguments.length === 0 && true){  return new MuNumber(5.0) } " ++
            "throw new MuPatternMatchError() " ++
          "}\n"
          )
        toJS (Function "f" [Equation [VariablePattern "x"] (UnguardedBody (Return (MuNumber 5)))]) `shouldBe` Just (
          "function f() { " ++
            "if(arguments.length === 1 && true && true){ var x = arguments[0]; return new MuNumber(5.0) } " ++
            "throw new MuPatternMatchError() " ++
          "}\n"
          )
        toJS (Function "f" [Equation [VariablePattern "x", VariablePattern "y"] (UnguardedBody (Return (MuNumber 5)))]) `shouldBe` Just (
          "function f() { " ++
            "if(arguments.length === 2 && true && true\n && true){ var x = arguments[0];\nvar y = arguments[1]; return new MuNumber(5.0) } " ++
            "throw new MuPatternMatchError() " ++
          "}\n"
          )
      
      it "Procedures" $ do pending

      it "Variable" $ do
        toJS (Variable "x" $ Reference "y") `shouldBe` Just "var x = y\n"

      it "Assignment" $ do
        toJS (Assignment "x" $ Reference "y") `shouldBe` Just "x = y\n"

      it "Reference" $ do
        toJS (Reference "x") `shouldBe` Just "x\n"

      it "Application" $ do
        toJS (Application (Reference "f") []) `shouldBe` Just "f()\n"
        toJS (Application (Reference "f") [Reference "x", Reference "y"]) `shouldBe` Just "f(x\n, y)\n"
        toJS (Application (Application (Reference "f") [Reference "x"]) [Application (Reference "g") [Reference "y"]]) `shouldBe` Just "f(x)(g(y))\n"

      it "Send" $ do
        toJS (Send (Reference "o") (Reference "m") []) `shouldBe` Just "o['m']()\n"
        toJS (Send (Reference "o") (Reference "m") [Reference "x", Reference "y"]) `shouldBe` Just "o['m'](x\n, y)\n"
        toJS (Send (Send (Reference "o") (Reference "m") [Reference "x"]) (Reference "n") [Reference "y"]) `shouldBe` Just "o['m'](x)['n'](y)\n"

      it "New" $ do
        toJS (New "C" []) `shouldBe` Just "new C()\n"
        toJS (New "C" [Reference "x", Reference "y"]) `shouldBe` Just "new C(x\n, y)\n"

      it "Raise" $ do
        toJS (Raise $ New "E" []) `shouldBe` Just "function(){ throw new E() }()\n"

      it "Print" $ do
        toJS (Print $ Reference "x") `shouldBe` Just "console.log(x)\n"

      it "MuNull" $ do
        toJS MuNull `shouldBe` Just "new MuNull()\n"
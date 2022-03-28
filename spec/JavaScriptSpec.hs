{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module JavaScriptSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Inspector.Literal (isOther)
import           Control.Exception (evaluate)

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = js . unpack

spec :: Spec
spec = do
  describe "simple assignation, with var" $ do
    it "simple assignation" $ do
      js "var x = 1" `shouldBe` (other "JSVar" (Variable "x" (MuNumber 1)))

    it "simple assignation, with let" $ do
      js "let x = 1" `shouldBe` (Variable "x" (MuNumber 1))

    it "simple declaration, with const" $ do
      js "const x = 1" `shouldBe` (Constant "x" (MuNumber 1))

    it "simple string assignment" $ do
      js "let x = 'hello'" `shouldBe` hs "x = \"hello\""

    it "number parsing" $ do
      js "1" `shouldBe` (MuNumber 1)
      js "1.0" `shouldBe` (MuNumber 1)
      js "1." `shouldBe` (MuNumber 1.0)
      js ".1" `shouldBe` (MuNumber 0.1)

    it "string parsing" $ do
      js "var x = \"hello\"" `shouldBe` js "var x = 'hello'"
      js "let x = \"hello\"" `shouldBe` js "let x = 'hello'"

    it "simple assignation 2" $ do
      js "x" `shouldBe` Reference "x"

    it "simple function application in var declaration" $ do
      js "let x = m(1, 2)" `shouldBe` hs "x = m 1 2"

    it "differentiates procedures and functions" $ do
      (js "function f() { return 1 }" /= js "function f() { 1 }") `shouldBe` True

    it "handles lambdas as functions" $ do
      js "let m = function(x) { return 1 }" `shouldBe` js "function m(x) { return 1 }"
      js "var m = function(x) { return 1 }" `shouldBe` (other "JSVar" (js "function m(x) { return 1 }"))

    it "handles arrow functions with explicit returns" $ do
      js "var m = (x) => { return 1 }" `shouldBe` js "var m = function(x) { return 1 }"

    it "handles arrow functions" $ do -- TODO this is not entirely true
      js "var m = (x) => { 1 }" `shouldBe` js "var m = function(x) { 1 }"

    it "simple function declaration" $ do
      js "function f(x) { return 1 }" `shouldBe` hs "f x = 1"

    it "comments do not affect return semantics" $ do
      run [text|
      function f(x) {
        return x
      }|] `shouldBe` SimpleFunction "f" [VariablePattern "x"] (Return (Reference "x"))

      run [text|
      function f(x) {
        return
        x
      }|] `shouldBe` SimpleFunction "f" [VariablePattern "x"] (Sequence [Return None, Reference "x"])

      run [text|
      function f(x) {
        return //
        x
      }|] `shouldBe` SimpleFunction "f" [VariablePattern "x"] (Sequence [Return None, Reference "x"])

      run [text|
      function f(x) {
        return /*
        */ x
      }|] `shouldBe` SimpleFunction "f" [VariablePattern "x"] (Sequence [Return None, Reference "x"])

      run [text|
      function f(x) {
        return /**/ x
      }|] `shouldBe` SimpleFunction "f" [VariablePattern "x"] (Return (Reference "x"))

    it "simple procedure declaration" $ do
      js "function f(x) { console.log('fruit') }" `shouldBe` (
                SimpleProcedure
                    "f"
                    [VariablePattern "x"]
                    (Print (MuString "fruit")))

    it "multiple params function declaration" $ do
      js "function f(x, y) { return 1 }" `shouldBe` hs "f x y = 1"

    it "numeric top level expression" $ do
      js "8" `shouldBe` MuNumber 8

    it "assignment should be Assignment" $ do
      js "x = 8" `shouldBe` (Assignment "x" (MuNumber 8))

    it "update should be Assignment" $ do
      js "x += 8" `shouldBe` (Assignment "x" (Application (Primitive Plus) [Reference "x", MuNumber 8.0]))

    it "increment should be Assignment" $ do
      js "x++" `shouldBe` (Assignment "x" (Application (Primitive Plus) [Reference "x", MuNumber 1]))

    it "decrement should be Assignment" $ do
      js "x--" `shouldBe` (Assignment "x" (Application (Primitive Minus) [Reference "x", MuNumber 1]))

    it "sum should be parseable" $ do
      js "x + y" `shouldBe` (Application (Primitive Plus) [Reference "x",Reference "y"])

    it "parses bitwise operations correctly" $ do
      js "x & y" `shouldBe` (Application (Primitive BitwiseAnd) [Reference "x", Reference "y"])
      js "x | y" `shouldBe` (Application (Primitive BitwiseOr) [Reference "x", Reference "y"])

    it "list literal top level expression" $ do
      js "[8, 7]" `shouldBe` MuList [MuNumber 8, MuNumber 7]

    it "compacts everything" $ do
      js "8; 9" `shouldBe`  Sequence [MuNumber 8, MuNumber 9]

    it "handles blocks as sequences" $ do
      js "8; 9" `shouldBe` js "{8; 9}"

    it "handles this" $ do
      js "this" `shouldBe` Self

    it "handles field reference" $ do
      js "x.y" `shouldBe` (FieldReference (Reference "x") "y")

    it "handles dict access" $ do
      js "x['y']" `shouldBe` (Application (Primitive GetAt) [Reference "x", MuString "y"])

    it "handles field assignment" $ do
      js "x.y = 4;" `shouldBe` (FieldAssignment (Reference "x") "y" (MuNumber (4.0)))

    it "handles field assignment after field reference" $ do
      js "x.y.z = 4;" `shouldBe` (FieldAssignment (FieldReference (Reference "x") "y") "z" (MuNumber (4.0)))

    it "handles dict assignment" $ do
      -- js "x['y'] = 4;" `shouldBe` (Application  (Primitive SetAt) [Reference "x", MuString "y", MuNumber (4.0)])
      pending

    it "handles booleans" $ do
      js "true" `shouldBe` MuTrue

    it "handles length" $ do
      js "[1, 2].length" `shouldBe` (Application (Primitive Size) [MuList [MuNumber 1, MuNumber 2]])

    it "handles length following function call" $ do
      js "f().length" `shouldBe` (Application (Primitive Size) [Application (Reference "f") []])

    it "handles length following field reference" $ do
      js "a.b.length" `shouldBe` (Application (Primitive Size) [FieldReference (Reference "a") "b"])

    it "handles push" $ do
      js "[1, 2].push(r)" `shouldBe` (Application (Primitive Push) [MuList [MuNumber 1, MuNumber 2], Reference "r"])

    it "handles push following function call" $ do
      js "f().push(r)" `shouldBe` (Application (Primitive Push) [Application (Reference "f") [], Reference "r"])

    it "handles push following field reference" $ do
      js "a.b.push(r)" `shouldBe` (Application (Primitive Push) [FieldReference (Reference "a") "b",Reference "r"])

    it "handles parenthesis around variables" $ do
      js "function f() { return (x) } " `shouldBe` (SimpleFunction "f" [] (Return (Reference "x")))
      js "let y = (x)" `shouldBe` (Variable "y" (Reference "x"))
      js "(x)" `shouldBe` (Reference "x")

    it "handles negation" $ do
      js "!true" `shouldBe` (Application (Primitive Negation) [MuTrue])

    it "handles boolean binary operations" $ do
      js "true || false " `shouldBe` (Application (Primitive Or) [MuTrue, MuFalse])

    it "handles lambdas" $ do
      js "(function(x, y) { 1 })" `shouldBe` (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1))

    it "handles arrow lambdas" $ do
      js "((x, y) => { 1 })" `shouldBe` (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1))

    it "handles arrow lambdas without body" $ do
      js "((x, y) => 1 )" `shouldBe` (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1))

    it "handles arrow lambdas without body nor parenthesis" $ do
      js "(x => 1 )" `shouldBe` (Lambda [VariablePattern "x"] (MuNumber 1))

    it "handles method calls with arrow lambdas without body" $ do
      js "list.map(x => x + 1)" `shouldBe` ( Send (Reference "list") (Reference "map") [Lambda [VariablePattern "x"] (Application (Primitive Plus) [Reference "x",MuNumber 1.0])])

    it "handles method calls with arrow lambdas without body following a function call" $ do
      js "f(xs).map(b => b.m)" `shouldBe` (Send (Application (Reference "f") [Reference "xs"]) (Reference "map") [Lambda [VariablePattern "b"] (FieldReference (Reference "b") "m")])

    it "handles application" $ do
      js "f(2)" `shouldBe` (Application (Reference "f") [MuNumber 2])

    it "handles application with multiple args" $ do
      js "f(2, 4)" `shouldBe` (Application (Reference "f") [MuNumber 2, MuNumber 4])

    it "handles message send" $ do
      js "o.f(2)" `shouldBe` (Send (Reference "o") (Reference "f") [(MuNumber 2)])

    it "handles message send following a function call" $ do
      js "o().f(2)" `shouldBe` (Send (Application (Reference "o") []) (Reference "f") [(MuNumber 2)])

    it "handles message send following a field reference" $ do
      js "a.b.f(2)" `shouldBe` (Send (FieldReference (Reference "a") "b") (Reference "f") [MuNumber 2.0])

    it "handles message send followed by another message send" $ do
      js "a.o().f(2)" `shouldBe` (Send (Send (Reference "a") (Reference "o") []) (Reference "f") [MuNumber 2.0])

    it "handles message send followed by a field reference" $ do
      js "a.f(2).b" `shouldBe` (FieldReference (Send (Reference "a") (Reference "f") [MuNumber 2.0]) "b")

    it "handles message send followed by a field assignment" $ do
      js "a.f(2).b = 3" `shouldBe` (FieldAssignment (Send (Reference "a") (Reference "f") [MuNumber 2.0]) "b" (MuNumber 3.0))

    it "handles ifs" $ do
      js "if(x) y; else z" `shouldBe` If (Reference "x") (Reference "y") (Reference "z")

    it "handles partial ifs" $ do
      js "if(x) y" `shouldBe` If (Reference "x") (Reference "y") None

    it "handles ternarys as ifs" $ do
      js "x ? y : z " `shouldBe` js "if (x) { y } else { z }"

    it "handles whiles" $ do
      js "while (x) { y }" `shouldBe` While (Reference "x") (Reference "y")

    it "foo" $ do
      evaluate (js ("function f(xs){\n"
        ++ " let s= 0;\n"
        ++ "  if (xs.y > 0)\n"
        ++ " s = s + xs.ys {\n"
        ++ "   return s; \n"
        ++ " }\n"
        ++ " }\n")) `shouldThrow` anyException


    it "handles objects" $ do
      js "({x: 6})" `shouldBe` MuObject (Variable "x" (MuNumber 6))

    it "handles objects with numeric keys" $ do
      js "({3: 6})" `shouldBe` MuObject (Variable "3" (MuNumber 6))

    it "handles objects with string keys" $ do
      js "({\"asd\": 6})" `shouldBe` MuObject (Variable "asd" (MuNumber 6))

    it "handles empty objects" $ do
      js "({})" `shouldBe` MuObject None

    it "handles object declarations" $ do
      js "let x = {}" `shouldBe` (Object "x" None)
      js "var x = {}" `shouldBe` (other "JSVar" (Object "x" None))

    it "handles function declarations as vars" $ do
      js "let x = function(){}" `shouldBe` (SimpleFunction "x" [] None)

    it "handles attribute and method declarations" $ do
      js "let x = {y: 2, z: function(){}}" `shouldBe` Object "x" (Sequence [
                                                            Attribute "y" (MuNumber 2.0),
                                                            SimpleMethod "z" [] None])

    it "handles new parentheses-less" $ do
      js "new Foo" `shouldBe` New (Reference "Foo") []

    it "handles new with parentheses" $ do
      js "new Foo()" `shouldBe` New (Reference "Foo") []

    it "handles new with args" $ do
      js "new Foo(1, 2)" `shouldBe` New (Reference "Foo") [MuNumber 1, MuNumber 2]

    it "handles new with args" $ do
      js "new Foo(1, 2)" `shouldBe` New (Reference "Foo") [MuNumber 1, MuNumber 2]

    it "handles switch" $ do
      run [text|
      switch(a){
        case 1: return 1;
        case 2: return 2;
      }|] `shouldBe` Switch (Reference "a") [(MuNumber 1, Return (MuNumber 1)), (MuNumber 2, Return (MuNumber 2))] (None)

    it "handles new with args" $ do
      run [text|
      switch(a){
        case 1: return 1;
        default: return 3;
      }|] `shouldBe` Switch (Reference "a") [(MuNumber 1, Return (MuNumber 1))] (Return (MuNumber 3))

    it "handles c-style for" $ do
      run "for(i = 0; i < 3; i++) i;" `shouldBe` ForLoop (Assignment "i" (MuNumber 0)) (js "i < 3") (js "i++") (Reference "i")

    it "handles c-style for with var" $ do
      -- run "for(var i = 0; i < 3; i++) i;" `shouldBe` ForLoop (other "JSVar" (Variable "i" (MuNumber 0))) (js "i < 3") (js "i++") (Reference "i")
      pending

    it "handles c-style for with let" $ do
      run "for(let i = 0; i < 3; i++) i;" `shouldBe` ForLoop (Variable "i" (MuNumber 0)) (js "i < 3") (js "i++") (Reference "i")

    describe "for generator" $ do
      let generatorWithLetAst = (For
                                  [Generator (VariablePattern "i") (MuList [MuNumber 1, MuNumber 2])]
                                  (Reference "i"))

      let generatorWithVarAst = (For
                                  [Generator (otherPattern "JSVar" (VariablePattern "i")) (MuList [MuNumber 1, MuNumber 2])]
                                  (Reference "i"))

      let generatorWithConstAst = (For
                                    [Generator (ConstantPattern "i") (MuList [MuNumber 1, MuNumber 2])]
                                    (Reference "i"))

      it "NOT handles for in" $ do
        isOther (run "for(i in [1,2]) i;") `shouldBe` True

      it "NOT handles for var in" $ do
        isOther (run "for(var i in [1,2]) i;") `shouldBe` True

      it "NOT handles for let in" $ do
        isOther (run "for(let i in [1,2]) i;") `shouldBe` True

      it "handles for let of" $ do
        run "for(let i of [1,2]) i;" `shouldBe` generatorWithLetAst

      it "handles for const of" $ do
        run "for(const i of [1,2]) i;" `shouldBe` generatorWithConstAst

      it "handles for var of" $ do
        run "for(var i of [1,2]) i;" `shouldBe` generatorWithVarAst

      it "handles for var of" $ do
        run "for(i of [1,2]) i;" `shouldBe` generatorWithVarAst


    context "handles assertions" $ do
      it "handles truth assertions" $ do
        run "assert(true)" `shouldBe` Assert False (Truth $ MuBool True)

      it "handles equality assertions" $ do
        run "assert.equals(123, 321)" `shouldBe` Assert False (Equality (MuNumber 123) (MuNumber 321))

      it "handles equality assertions" $ do
        run "assert.notEquals(123, 321)" `shouldBe` Assert True (Equality (MuNumber 123) (MuNumber 321))

      it "handles failure assertions" $ do
        run "assert.throws(function() { throw('abc') }, 'abc')" `shouldBe` Assert False (Failure (Lambda [] (Raise (MuString "abc"))) (MuString "abc"))

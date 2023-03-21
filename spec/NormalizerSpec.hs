module NormalizerSpec (spec) where

  import           Test.Hspec
  import           Language.Mulang.Ast
  import           Language.Mulang.Ast.Operator
  import           Language.Mulang.Parsers.Haskell (hs)
  import           Language.Mulang.Parsers.Java (java)
  import           Language.Mulang.Parsers.JavaScript (js)
  import           Language.Mulang.Parsers.Python (npy, py)
  import           Language.Mulang.Normalizers.Java (javaNormalizationOptions)
  import           Language.Mulang.Normalizers.Haskell (haskellNormalizationOptions)
  import           Language.Mulang.Transform.Normalizer

  njava = normalize javaNormalizationOptions . java
  nhs = normalize haskellNormalizationOptions . hs

  spec :: Spec
  spec = do
    describe "can convert dicts" $ do
      let options = unnormalized { convertObjectIntoDict = True }
      let n = normalize options

      it "converts dict and its var contents" $ do
        n (MuObject (Variable "x" (MuNumber 5))) `shouldBe` (MuDict (Arrow (MuString "x") (MuNumber 5)))
        n (MuObject (Sequence [Variable "x" (MuNumber 5), Variable "y" (MuNumber 6)])) `shouldBe` (MuDict (Sequence [
                                                                                          Arrow (MuString "x") (MuNumber 5),
                                                                                          Arrow (MuString "y") (MuNumber 6)]))

    describe "can trim code" $ do
      let options = unnormalized { trimSequences = True }
      let n = normalize options

      it "removes nones from sequences" $ do
        n (Object "X" (Sequence [None, None, MuNumber 5, None])) `shouldBe`  (Object "X" (Sequence [MuNumber 5]))

      it "does not remove nones from literals" $ do
        n (MuList [None, None, MuNumber 5, None]) `shouldBe`  (MuList [None, None, MuNumber 5, None])

    describe "can sort commutative operations" $ do
      let options = unnormalized { sortCommutativeApplications = True }
      let n = normalize options

      it "sorts references" $ do
        n (Application (Primitive Equal) [Reference "a", Reference "b"])  `shouldBe` n (Application (Primitive Equal) [Reference "b", Reference "a"])
        n (Application (Primitive Max) [Reference "a", Reference "b"])  `shouldBe` n (Application (Primitive Max) [Reference "b", Reference "a"])
        n (Application (Primitive Similar) [Reference "a", Reference "b"])  `shouldBe` n (Application (Primitive Similar) [Reference "b", Reference "a"])

    describe "can compact code" $ do
      let options = unnormalized { compactSequences = True }
      let n = normalize options


      it "compacts sequences" $ do
        n (Object "X" (Sequence [MuNumber 5])) `shouldBe`  (Object "X" (MuNumber 5))
        n (Object "X" (Sequence [])) `shouldBe`  (Object "X" None)
        n (Object "X" (Sequence [MuNumber 5, MuNumber 6])) `shouldBe`  (Object "X" (Sequence [MuNumber 5, MuNumber 6]))

        n (Object "X" (Sequence [None])) `shouldBe`  (Object "X" None)
        n (Object "X" (Sequence [None, None])) `shouldBe`  (Object "X" (Sequence [None, None]))
        n (Object "X" (Sequence [None, None, MuNumber 5])) `shouldBe`  (Object "X" (Sequence [None, None, MuNumber 5]))

    describe "can trim and compact code" $ do
      let options = unnormalized { trimSequences = True, compactSequences = True }
      let n = normalize options

      it "trims first, compacts later" $ do
        n (Object "X" (Sequence [MuNumber 5])) `shouldBe`  (Object "X" (MuNumber 5))
        n (Object "X" (Sequence [])) `shouldBe`  (Object "X" None)
        n (Object "X" (Sequence [MuNumber 5, MuNumber 6])) `shouldBe`  (Object "X" (Sequence [MuNumber 5, MuNumber 6]))

        n (Object "X" (Sequence [None])) `shouldBe`  (Object "X" None)
        n (Object "X" (Sequence [None, None])) `shouldBe`  (Object "X" None)
        n (Object "X" (Sequence [None, None, MuNumber 5])) `shouldBe`  (Object "X" (MuNumber 5))

    describe "can insert implicit retuns" $ do
      let options = unnormalized { insertImplicitReturn = True }
      let n = normalize options

      it "does not insert return in single literal statement" $ do
        n (npy "def x(): x = 1") `shouldBe`  SimpleProcedure "x" [] (Assignment "x" (MuNumber 1.0))

      it "inserts return in single literal expression" $ do
        n (npy "def x(): 3") `shouldBe`  SimpleProcedure "x" [] (Return (MuNumber 3.0))

      it "does not insert return in empty block" $ do
        n (SimpleFunction "x" [] None) `shouldBe`  (SimpleFunction "x" [] None)

      it "does not insert return in singleton nil block" $ do
        let expression = SimpleFunction "x" [] MuNil
        n expression `shouldBe`  expression

      it "inserts return in non-singleton nil block" $ do
        let expression = SimpleFunction "x" [] (Sequence [Print (MuString "hello"),  MuNil])
        n expression `shouldBe`  expression


      it "inserts return in last literal expression" $ do
        n (js "function x() { let x = 1; x += 1; x }") `shouldBe`  SimpleProcedure "x" [] (Sequence [
                                                                                          Variable "x" (MuNumber 1.0),
                                                                                          Assignment "x" (Application (Primitive Plus) [Reference "x",MuNumber 1.0]),
                                                                                          Return (Reference "x")])

    describe "sorts declarations by default" $ do
      it "sorts functions on Python" $ do
        npy "def foo():\n  return 2\n\ndef bar():\n  return 1\n\n" `shouldBe` py "def bar():\n  return 1\n\ndef foo():\n  return 2\n\n"

      it "sorts classes on Java" $ do
        njava "class Foo {} class Bar {}" `shouldBe` java "class Bar {} class Foo {}"

      it "sorts functions on haskell" $ do
        nhs "g 2 = 2\nf 1 = 1" `shouldBe` hs "f 1 = 1\ng 2 = 2"

      it "sorts declarations on haskell even when there are variables" $ do
        nhs "g 2 = 2\nf 1 = 1\nn = 1" `shouldBe` hs "f 1 = 1\ng 2 = 2\nn = 1"

      it "sorts functions on javascript if there are only functions" $ do
        js "function f() {} function g() {}" `shouldBe` js "function g() {} function f() {}"

      it "doesn't sort functions on javascript if there are duplicates names" $ do
        js "function f() { return 1 } function g() {} function f() { return 2 }" `shouldNotBe` js "function g() {} function f() { return 2 } function f() { return 1 } "

      it "doesn't sort declarations on javascript if there are also statements" $ do
        js "function f() {}; let x = 2; function g() {}" `shouldNotBe` js "function g() {}; let x = 2; function f() {}"

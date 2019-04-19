module UnpythonSpec (spec) where

import           Test.Hspec
import           Language.Mulang

import           Language.Mulang.Parsers.Python (py)
import           Language.Mulang.Unparsers.Python (unpy)

shouldRoundTrip expression =  (py.unpy) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "roundTrip" $ do
    it "numbers" $ do
      shouldRoundTrip (MuNumber 1.0)

    it "integers" $ do
      --shouldRoundTrip (MuNumber 1)
      pending

    it "booleans" $ do
      shouldRoundTrip MuTrue

    it "strings" $ do
      shouldRoundTrip (MuString "some string")

    it "lists" $ do
      shouldRoundTrip (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0])

    it "sets as lists" $ do
      --shouldRoundTrip (MuList [MuNumber 1, MuNumber 2, MuNumber 3])
      pending

    it "assignment" $ do
      shouldRoundTrip (Assignment "one" (MuNumber 1.0))

    it "references" $ do
      shouldRoundTrip ((Reference "x"))

    it "application" $ do
      shouldRoundTrip ((Application (Reference "f") [MuNumber 2.0]))

    it "message sending" $ do
      shouldRoundTrip ((Send (Reference "o") (Reference "f") [(MuNumber 2)]))

    it "assign-operators" $ do
      shouldRoundTrip ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0])))

    it "binary operators" $ do
      shouldRoundTrip ((Application (Reference "+") [Reference "x",Reference "y"]))

    it "sequences" $ do
      shouldRoundTrip (Sequence [MuNumber 1, MuNumber 2, MuNumber 3])

    it "unary operators" $ do
      shouldRoundTrip ((Application (Primitive Negation) [MuTrue]))

    it "classes" $ do
      shouldRoundTrip (Class "DerivedClassName" Nothing None)

    it "inheritance" $ do
      shouldRoundTrip (Class "DerivedClassName" (Just "BaseClassName") None)

    it "if, elif and else" $ do
      shouldRoundTrip (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3)))

    it "functions" $ do
      shouldRoundTrip (SimpleFunction "foo" [] (Return (MuNumber 1.0)))

    it "procedures" $ do
      shouldRoundTrip (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param")))

    it "whiles" $ do
      shouldRoundTrip (While MuTrue None)

    it "fors" $ do
      shouldRoundTrip (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None)

    it "raise expressions" $ do
      shouldRoundTrip (Raise None)

    it "raise expressions with exception" $ do
      shouldRoundTrip (Raise (Application (Reference "Exception") [MuString "something"]))

    it "lambdas with one argument" $ do
      shouldRoundTrip (Lambda [VariablePattern "x"] (MuNumber 1))

    it "lambdas with zero arguments" $ do
      shouldRoundTrip (Lambda [] (Reference "foo"))

    it "tuples" $ do
      shouldRoundTrip (MuTuple [MuNumber 1, MuString "something"])

    it "yields" $ do
      shouldRoundTrip (Yield (MuNumber 1.0))


  describe "unpy" $ do
    it "numbers" $ do
      unpy  (MuNumber 1.0) `shouldBe` "1.0"

    it "integers" $ do
      --unpy  (MuNumber 1) `shouldBe` "1"
      pending

    it "booleans" $ do
      unpy  MuTrue `shouldBe` "True"

    it "strings" $ do
      unpy  (MuString "some string") `shouldBe` "\"some string\""

    it "lists" $ do
      unpy  (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0]) `shouldBe` "[1.0,2.0,3.0]"

    it "sets as lists" $ do
      --unpy  (MuList [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "{1,2,3}"
      pending

    it "assignment" $ do
      unpy  (Assignment "one" (MuNumber 1.0)) `shouldBe` "one = 1.0"

    it "references" $ do
      unpy  ((Reference "x")) `shouldBe` "x"

    it "application" $ do
      unpy  ((Application (Reference "f") [MuNumber 2.0])) `shouldBe` "f(2.0)"

    it "message sending" $ do
      unpy  ((Send (Reference "o") (Reference "f") [(MuNumber 2)])) `shouldBe` "o.f(2.0)"

    it "assign-operators" $ do
      unpy  ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))) `shouldBe` "x = x + 8.0"

    it "binary operators" $ do
      unpy  ((Application (Reference "+") [Reference "x",Reference "y"])) `shouldBe` "x + y"

    it "sequences" $ do
      unpy  (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "1.0\n2.0\n3.0"

    it "unary operators" $ do
      unpy  ((Application (Primitive Negation) [MuTrue])) `shouldBe` "not True"

    it "classes" $ do
      unpy  (Class "DerivedClassName" Nothing None) `shouldBe` "class DerivedClassName:\n\tpass\n"

    it "inheritance" $ do
      unpy  (Class "DerivedClassName" (Just "BaseClassName") None) `shouldBe` "class DerivedClassName(BaseClassName):\n\tpass\n"

    it "if, elif and else" $ do
      pending
      -- unpy  (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3))) `shouldBe` "if True: 1\nelif False: 2\nelse: 3"

    it "functions" $ do
      unpy  (SimpleFunction "foo" [] (Return (MuNumber 1.0))) `shouldBe` "def foo():\n\treturn 1.0\n"

    it "procedures" $ do
      unpy  (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))) `shouldBe` "def foo(param):\n\tprint(param)\n"

    it "whiles" $ do
      unpy  (While MuTrue None) `shouldBe` "while True:\n\tpass\n"

    it "fors" $ do
      unpy  (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None) `shouldBe` "for x in range(0, 3): pass"

    it "raise expressions" $ do
      unpy  (Raise None) `shouldBe` "raise"

    it "raise expressions with exception" $ do
      unpy  (Raise (Application (Reference "Exception") [MuString "something"])) `shouldBe` "raise Exception(\"something\")"

    it "lambdas with one arg" $ do
      unpy  (Lambda [VariablePattern "x"] (Reference "x")) `shouldBe` "lambda x: x"

    it "lambdas with two args" $ do
      unpy  (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1)) `shouldBe` "lambda x,y: 1.0"

    it "lambdas with zero args" $ do
      unpy  (Lambda [] MuNil) `shouldBe` "lambda : None"

    it "tuples" $ do
      unpy  (MuTuple [MuNumber 1.0, MuString "something"]) `shouldBe` "(1.0,\"something\")"

    it "yields" $ do
      unpy  (Yield (MuNumber 1.0)) `shouldBe` "yield 1.0"

    it "None" $ do
      unpy  MuNil `shouldBe` "None"

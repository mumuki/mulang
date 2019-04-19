module UnpythonSpec (spec) where

import           Test.Hspec
import           Language.Mulang

import           Language.Mulang.Parsers.Python (py)
import           Language.Mulang.Unparsers.Python (unpy)

shouldRoundTrip expression =  (py.unpy) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "roundTrip" $ do
    let itWorksWith expr = it (show expr) (shouldRoundTrip expr)
    let itDoesntWorkYetWith expr = it (show expr) pending

    describe "literals" $ do
      itWorksWith $ (MuNumber 1.0)
      itDoesntWorkYetWith $ (MuNumber 1)
      itWorksWith $ MuTrue
      itWorksWith $ (MuString "some string")
      itWorksWith $ (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0])
      itDoesntWorkYetWith $ (MuList [MuNumber 1, MuNumber 2, MuNumber 3])

    itWorksWith $ (Assignment "one" (MuNumber 1.0))
    itWorksWith $ ((Reference "x"))
    itWorksWith $ ((Application (Reference "f") [MuNumber 2.0]))
    itWorksWith $ ((Send (Reference "o") (Reference "f") [(MuNumber 2)]))
    itWorksWith $ ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0])))
    itWorksWith $ ((Application (Reference "+") [Reference "x",Reference "y"]))
    itWorksWith $ (Sequence [MuNumber 1, MuNumber 2, MuNumber 3])
    itWorksWith $ ((Application (Primitive Negation) [MuTrue]))

    describe "classes" $ do
      itWorksWith $ (Class "DerivedClassName" Nothing None)
      itWorksWith $ (Class "DerivedClassName" (Just "BaseClassName") None)

    itWorksWith $ (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3)))
    itWorksWith $ (SimpleFunction "foo" [] (Return (MuNumber 1.0)))
    itWorksWith $ (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param")))
    itWorksWith $ (While MuTrue None)
    itWorksWith $ (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None)
    itWorksWith $ (Raise None)
    itWorksWith $ (Raise (Application (Reference "Exception") [MuString "something"]))

    describe "lambdas" $ do
      itWorksWith $ (Lambda [VariablePattern "x"] (MuNumber 1))
      itWorksWith $ (Lambda [] (Reference "foo"))

    itWorksWith $ (MuTuple [MuNumber 1, MuString "something"])
    itWorksWith $ (Yield (MuNumber 1.0))

    describe "boolean operations" $ do
      let muand x y = (Application (Primitive And) [x, y])
      let muor  x y = (Application (Primitive Or) [x, y])
      let muneg x = (Application (Primitive Negation) [x])

      itWorksWith $ (Reference "a") `muand` (Reference "b")
      itWorksWith $ (Reference "a") `muor` (Reference "b")
      itWorksWith $ (muneg (Reference "a")) `muor` (Reference "b")
      itWorksWith $ muneg ((Reference "a") `muor` (Reference "b"))
      itWorksWith $ muneg ((Reference "a") `muand` (Reference "b")) `muor` (Reference "c")
      itWorksWith $ (Reference "a") `muand` ((Reference "b") `muor` (Reference "c"))

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
      unpy  ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))) `shouldBe` "x = (x + 8.0)"

    it "binary operators" $ do
      unpy  ((Application (Reference "+") [Reference "x",Reference "y"])) `shouldBe` "(x + y)"

    it "sequences" $ do
      unpy  (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "1.0\n2.0\n3.0"

    it "unary operators" $ do
      unpy  ((Application (Primitive Negation) [MuTrue])) `shouldBe` "(not True)"

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

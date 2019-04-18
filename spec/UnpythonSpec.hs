module UnpythonSpec (spec) where

import           Test.Hspec
import           Language.Mulang

import           Language.Mulang.Parsers.Python (py)
import           Language.Mulang.Unparsers.Python (unpy)

shouldRoundTrip expression =  (py.unpy) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "roundTrip" $ do
    it "roundTrips numbers" $ do
      shouldRoundTrip (MuNumber 1.0)

    it "roundTrips integers" $ do
      --shouldRoundTrip (MuNumber 1)
      pending

    it "roundTrips booleans" $ do
      shouldRoundTrip MuTrue

    it "roundTrips strings" $ do
      shouldRoundTrip (MuString "some string")

    it "roundTrips lists" $ do
      shouldRoundTrip (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0])

    it "roundTrips sets as lists" $ do
      --shouldRoundTrip (MuList [MuNumber 1, MuNumber 2, MuNumber 3])
      pending

    it "roundTrips assignment" $ do
      shouldRoundTrip (Assignment "one" (MuNumber 1.0))

    it "roundTrips references" $ do
      shouldRoundTrip ((Reference "x"))

    it "roundTrips application" $ do
      shouldRoundTrip ((Application (Reference "f") [MuNumber 2.0]))

    it "roundTrips message sending" $ do
      shouldRoundTrip ((Send (Reference "o") (Reference "f") [(MuNumber 2)]))

    it "roundTrips assign-operators" $ do
      shouldRoundTrip ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0])))

    it "roundTrips binary operators" $ do
      shouldRoundTrip ((Application (Reference "+") [Reference "x",Reference "y"]))

    it "roundTrips sequences" $ do
      shouldRoundTrip (Sequence [MuNumber 1, MuNumber 2, MuNumber 3])

    it "roundTrips unary operators" $ do
      shouldRoundTrip ((Application (Primitive Negation) [MuTrue]))

    it "roundTrips classes" $ do
      shouldRoundTrip (Class "DerivedClassName" Nothing None)

    it "roundTrips inheritance" $ do
      shouldRoundTrip (Class "DerivedClassName" (Just "BaseClassName") None)

    it "roundTrips if, elif and else" $ do
      shouldRoundTrip (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3)))

    it "roundTrips functions" $ do
      shouldRoundTrip (SimpleFunction "foo" [] (Return (MuNumber 1.0)))

    it "roundTrips procedures" $ do
      shouldRoundTrip (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param")))

    it "roundTrips whiles" $ do
      shouldRoundTrip (While MuTrue None)

    it "roundTrips fors" $ do
      shouldRoundTrip (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None)

    it "roundTrips raise expressions" $ do
      shouldRoundTrip (Raise None)

    it "roundTrips raise expressions with exception" $ do
      shouldRoundTrip (Raise (Application (Reference "Exception") [MuString "something"]))

    it "roundTrips lambdas" $ do
      shouldRoundTrip (Lambda [VariablePattern "x"] (MuNumber 1))

    it "roundTrips tuples" $ do
      shouldRoundTrip (MuTuple [MuNumber 1, MuString "something"])

    it "roundTrips yields" $ do
      shouldRoundTrip (Yield (MuNumber 1.0))

  describe "unpy" $ do
    it "unpies numbers" $ do
      unpy  (MuNumber 1.0) `shouldBe` "1.0"

    it "unpies integers" $ do
      --unpy  (MuNumber 1) `shouldBe` "1"
      pending

    it "unpies booleans" $ do
      unpy  MuTrue `shouldBe` "True"

    it "unpies strings" $ do
      unpy  (MuString "some string") `shouldBe` "\"some string\""

    it "unpies lists" $ do
      unpy  (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0]) `shouldBe` "[1.0,2.0,3.0]"

    it "unpies sets as lists" $ do
      --unpy  (MuList [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "{1,2,3}"
      pending

    it "unpies assignment" $ do
      unpy  (Assignment "one" (MuNumber 1.0)) `shouldBe` "one = 1.0"

    it "unpies references" $ do
      unpy  ((Reference "x")) `shouldBe` "x"

    it "unpies application" $ do
      unpy  ((Application (Reference "f") [MuNumber 2.0])) `shouldBe` "f(2.0)"

    it "unpies message sending" $ do
      unpy  ((Send (Reference "o") (Reference "f") [(MuNumber 2)])) `shouldBe` "o.f(2.0)"

    it "unpies assign-operators" $ do
      unpy  ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))) `shouldBe` "x = x + 8.0"

    it "unpies binary operators" $ do
      unpy  ((Application (Reference "+") [Reference "x",Reference "y"])) `shouldBe` "x + y"

    it "unpies sequences" $ do
      unpy  (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "1.0\n2.0\n3.0"

    it "unpies unary operators" $ do
      unpy  ((Application (Primitive Negation) [MuTrue])) `shouldBe` "not True"

    it "unpies classes" $ do
      unpy  (Class "DerivedClassName" Nothing None) `shouldBe` "class DerivedClassName:\n\tpass\n"

    it "unpies inheritance" $ do
      unpy  (Class "DerivedClassName" (Just "BaseClassName") None) `shouldBe` "class DerivedClassName(BaseClassName):\n\tpass\n"

    it "unpies if, elif and else" $ do
      pending
      -- unpy  (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3))) `shouldBe` "if True: 1\nelif False: 2\nelse: 3"

    it "unpies functions" $ do
      unpy  (SimpleFunction "foo" [] (Return (MuNumber 1.0))) `shouldBe` "def foo(): return 1"

    it "unpies procedures" $ do
      unpy  (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))) `shouldBe` "def foo(param): print(param)"

    it "unpies whiles" $ do
      unpy  (While MuTrue None) `shouldBe` "while True:\n\tpass\n"

    it "unpies fors" $ do
      unpy  (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None) `shouldBe` "for x in range(0, 3): pass"

    it "unpies raise expressions" $ do
      unpy  (Raise None) `shouldBe` "raise"

    it "unpies raise expressions with exception" $ do
      unpy  (Raise (Application (Reference "Exception") [MuString "something"])) `shouldBe` "raise Exception(\"something\")"

    it "unpies lambdas" $ do
      unpy  (Lambda [VariablePattern "x"] (MuNumber 1)) `shouldBe` "lambda x: 1"

    it "unpies tuples" $ do
      unpy  (MuTuple [MuNumber 1.0, MuString "something"]) `shouldBe` "(1.0,\"something\")"

    it "unpies yields" $ do
      unpy  (Yield (MuNumber 1.0)) `shouldBe` "yield 1.0"


module UnpythonSpec (spec) where

import           Test.Hspec
import           Language.Mulang

import           Language.Mulang.Parsers.Python (py)
import           Language.Mulang.Unparsers.Python (unparsePython)

shouldRoundTrip expression =  (py.unparsePython) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "roundTrip" $ do
    let itWorksWith expr = it (show expr) (shouldRoundTrip expr)

    describe "literals" $ do
      itWorksWith $ (MuNumber 1.5)
      itWorksWith $ (MuNumber 1)
      itWorksWith $ MuTrue
      itWorksWith $ (MuString "some string")
      itWorksWith $ (MuList [MuNumber 1, MuNumber 2, MuNumber 3])
      itWorksWith $ (MuList [MuNumber 1, MuNumber 2, MuNumber 3])

    itWorksWith $ (Assignment "one" (MuNumber 1))
    itWorksWith $ ((Reference "x"))
    itWorksWith $ ((Application (Reference "f") [MuNumber 2]))
    itWorksWith $ ((Send (Reference "o") (Reference "f") [(MuNumber 2)]))
    itWorksWith $ ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8])))
    itWorksWith $ ((Application (Reference "+") [Reference "x",Reference "y"]))
    itWorksWith $ (Sequence [MuNumber 1, MuNumber 2, MuNumber 3])
    itWorksWith $ ((Application (Primitive Negation) [MuTrue]))

    describe "classes" $ do
      itWorksWith $ (Class "DerivedClassName" Nothing None)
      itWorksWith $ (Class "DerivedClassName" (Just "BaseClassName") None)

    itWorksWith $ (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3)))
    itWorksWith $ (SimpleFunction "foo" [] (Return (MuNumber 1)))
    itWorksWith $ (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param")))
    itWorksWith $ (While MuTrue None)
    itWorksWith $ (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None)
    itWorksWith $ (Raise None)
    itWorksWith $ (Raise (Application (Reference "Exception") [MuString "something"]))

    describe "lambdas" $ do
      itWorksWith $ (Lambda [VariablePattern "x"] (MuNumber 1))
      itWorksWith $ (Lambda [] (Reference "foo"))

    itWorksWith $ (MuTuple [MuNumber 1, MuString "something"])
    itWorksWith $ (Yield (MuNumber 1))

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

  describe "unparsePython" $ do
    let itWorksWith expr expectedCode = it (show expr) (unparsePython expr `shouldBe` expectedCode)

    describe "literals" $ do
      itWorksWith (MuNumber 1) "1"
      itWorksWith (MuNumber 1.5) "1.5"
      itWorksWith (MuNumber 1) "1"
      itWorksWith MuTrue "True"
      itWorksWith (MuString "some string") "\"some string\""
      itWorksWith (MuList [MuNumber 1, MuNumber 2, MuNumber 3]) "[1,2,3]"
      itWorksWith MuNil "None"

    itWorksWith (Assignment "one" (MuNumber 1)) "one = 1"
    itWorksWith ((Reference "x")) "x"
    itWorksWith ((Application (Reference "f") [MuNumber 2])) "f(2)"
    itWorksWith ((Send (Reference "o") (Reference "f") [(MuNumber 2)])) "o.f(2)"
    itWorksWith ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8]))) "x = (x + 8)"
    itWorksWith ((Application (Reference "+") [Reference "x",Reference "y"])) "(x + y)"
    itWorksWith (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) "1\n2\n3"
    itWorksWith ((Application (Primitive Negation) [MuTrue])) "(not True)"

    describe "clases" $ do
      itWorksWith (Class "DerivedClassName" Nothing None) "class DerivedClassName:\n\tpass\n"
      itWorksWith (Class "DerivedClassName" (Just "BaseClassName") None) "class DerivedClassName(BaseClassName):\n\tpass\n"

    describe "defs" $ do
      itWorksWith (SimpleFunction "foo" [] (Return (MuNumber 1))) "def foo():\n\treturn 1\n"
      itWorksWith (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))) "def foo(param):\n\tprint(param)\n"

    itWorksWith (While MuTrue None) "while True:\n\tpass\n"
    itWorksWith (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None) "for x in range(0,3): pass"
    itWorksWith (Raise None) "raise"
    itWorksWith (Raise (Application (Reference "Exception") [MuString "something"])) "raise Exception(\"something\")"

    describe "lambdas" $ do
      itWorksWith (Lambda [VariablePattern "x"] (Reference "x")) "lambda x: x"
      itWorksWith (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1)) "lambda x,y: 1"
      itWorksWith (Lambda [] MuNil) "lambda : None"

    itWorksWith (MuTuple [MuNumber 1, MuString "something"]) "(1,\"something\")"
    itWorksWith (Yield (MuNumber 1)) "yield 1"

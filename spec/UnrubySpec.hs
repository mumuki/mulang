module UnrubySpec (spec) where

import           Test.Hspec
import           Language.Mulang

import           Language.Mulang.Unparsers.Ruby (unrb)

spec :: Spec
spec = do
  describe "unrb" $ do
    it "numbers" $ do
      unrb  (MuNumber 1.0) `shouldBe` "1.0"

    it "integers" $ do
      --unrb  (MuNumber 1) `shouldBe` "1"
      pending

    it "booleans" $ do
      unrb  MuTrue `shouldBe` "true"
      unrb  MuFalse `shouldBe` "false"

    it "strings" $ do
      unrb  (MuString "some string") `shouldBe` "\"some string\""

    it "lists" $ do
      unrb  (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0]) `shouldBe` "[1.0,2.0,3.0]"

    it "assignment" $ do
      unrb  (Assignment "one" (MuNumber 1.0)) `shouldBe` "one = 1.0"

    it "references" $ do
      unrb  ((Reference "x")) `shouldBe` "x"

    it "application" $ do
      unrb  ((Application (Reference "f") [MuNumber 2.0])) `shouldBe` "f(2.0)"

    it "message sending" $ do
      unrb  ((Send (Reference "o") (Reference "f") [(MuNumber 2)])) `shouldBe` "o.f(2.0)"

    it "assign-operators" $ do
      unrb  ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))) `shouldBe` "x = x + 8.0"

    it "binary operators" $ do
      unrb  ((Application (Reference "+") [Reference "x",Reference "y"])) `shouldBe` "x + y"

    it "sequences" $ do
      unrb  (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "1.0\n2.0\n3.0"

    it "unary operators" $ do
      unrb  ((Application (Primitive Negation) [MuTrue])) `shouldBe` "!true"

    it "classes" $ do
      unrb  (Class "DerivedClassName" Nothing None) `shouldBe` "class DerivedClassName\nend\n"

    it "inheritance" $ do
      unrb  (Class "DerivedClassName" (Just "BaseClassName") None) `shouldBe` "class DerivedClassName < BaseClassName\nend\n"

    it "if, elif and else" $ do
      unrb  (If MuTrue (MuNumber 1.0) (MuNumber 3.0)) `shouldBe` "if true\n\t1.0\nelse\n\t3.0\nend\n"

    it "functions" $ do
      unrb  (SimpleFunction "foo" [] (Return (MuNumber 1.0))) `shouldBe` "def foo()\n\treturn 1.0\nend\n"

    it "functions with args" $ do
      unrb  (SimpleFunction "foo" [VariablePattern "x"] (Return (Reference "x"))) `shouldBe` "def foo(x)\n\treturn x\nend\n"

    it "procedures" $ do
      unrb  (SimpleProcedure "foo" [] (Print (Reference "param"))) `shouldBe` "def foo()\n\tputs(param)\nend\n"

    it "procedures with arguments" $ do
      unrb  (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))) `shouldBe` "def foo(param)\n\tputs(param)\nend\n"

    it "whiles" $ do
      unrb  (While MuTrue None) `shouldBe` "while true\nend\n"

    it "whiles with body" $ do
      unrb  (While MuTrue (Print (MuString "hi"))) `shouldBe` "while true\n\tputs(\"hi\")\nend\n"

    it "raise expressions" $ do
      unrb  (Raise None) `shouldBe` "raise"

    it "raise expressions with exception" $ do
      unrb  (Raise (MuString "something")) `shouldBe` "raise \"something\""

    it "lambdas with one arg" $ do
      unrb (Lambda [VariablePattern "x"] (Reference "x")) `shouldBe` "lambda { |x| x }"

    it "lambdas with two args" $ do
      unrb (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1)) `shouldBe` "lambda { |x,y| 1.0 }"

    it "lambdas with zero args" $ do
      unrb (Lambda [] MuNil) `shouldBe` "lambda { || nil }"

    it "yields" $ do
      unrb  (Yield (MuNumber 1.0)) `shouldBe` "yield 1.0"

    it "nil" $ do
      unrb  MuNil `shouldBe` "nil"

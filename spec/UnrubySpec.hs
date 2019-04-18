module UnrubySpec (spec) where

import           Test.Hspec
import           Language.Mulang

import           Language.Mulang.Unparsers.Ruby (unrb)

spec :: Spec
spec = do
  describe "unrb" $ do
    it "unrbies numbers" $ do
      unrb  (MuNumber 1.0) `shouldBe` "1.0"

    it "unrbies integers" $ do
      --unrb  (MuNumber 1) `shouldBe` "1"
      pending

    it "unrbies booleans" $ do
      unrb  MuTrue `shouldBe` "true"
      unrb  MuFalse `shouldBe` "false"

    it "unrbies strings" $ do
      unrb  (MuString "some string") `shouldBe` "\"some string\""

    it "unrbies lists" $ do
      unrb  (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0]) `shouldBe` "[1.0,2.0,3.0]"

    it "unrbies assignment" $ do
      unrb  (Assignment "one" (MuNumber 1.0)) `shouldBe` "one = 1.0"

    it "unrbies references" $ do
      unrb  ((Reference "x")) `shouldBe` "x"

    it "unrbies application" $ do
      unrb  ((Application (Reference "f") [MuNumber 2.0])) `shouldBe` "f(2.0)"

    it "unrbies message sending" $ do
      unrb  ((Send (Reference "o") (Reference "f") [(MuNumber 2)])) `shouldBe` "o.f(2.0)"

    it "unrbies assign-operators" $ do
      unrb  ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))) `shouldBe` "x = x + 8.0"

    it "unrbies binary operators" $ do
      unrb  ((Application (Reference "+") [Reference "x",Reference "y"])) `shouldBe` "x + y"

    it "unrbies sequences" $ do
      unrb  (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "1.0\n2.0\n3.0"

    it "unrbies unary operators" $ do
      unrb  ((Application (Primitive Negation) [MuTrue])) `shouldBe` "!true"

    it "unrbies classes" $ do
      unrb  (Class "DerivedClassName" Nothing None) `shouldBe` "class DerivedClassName\nend\n"

    it "unrbies inheritance" $ do
      unrb  (Class "DerivedClassName" (Just "BaseClassName") None) `shouldBe` "class DerivedClassName < BaseClassName\nend\n"

    it "unrbies if, elif and else" $ do
      unrb  (If MuTrue (MuNumber 1.0) (MuNumber 3.0)) `shouldBe` "if true\n\t1.0\nelse\n\t3.0\nend\n"

    it "unrbies functions" $ do
      unrb  (SimpleFunction "foo" [] (Return (MuNumber 1.0))) `shouldBe` "def foo()\n\treturn 1\nend\n"

    it "unrbies procedures" $ do
      unrb  (SimpleProcedure "foo" [] (Print (Reference "param"))) `shouldBe` "def foo()\n\tputs(param)\nend\n"
      unrb  (SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))) `shouldBe` "def foo(param)\n\tputs(param)\nend\n"


    it "unrbies whiles" $ do
      unrb  (While MuTrue None) `shouldBe` "while true\nend\n"

    it "unrbies whiles with body" $ do
      unrb  (While MuTrue (Print (MuString "hi"))) `shouldBe` "while true\nputs \"hi\"\n"

    it "unrbies raise expressions" $ do
      unrb  (Raise None) `shouldBe` "raise"

    it "unrbies raise expressions with exception" $ do
      unrb  (Raise (MuString "something")) `shouldBe` "raise \"something\""

    it "unrbies lambdas" $ do
      unrb  (Lambda [VariablePattern "x"] (MuNumber 1)) `shouldBe` "lambda { |x| 1}"

    it "unrbies yields" $ do
      unrb  (Yield (MuNumber 1.0)) `shouldBe` "yield 1.0"


{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module PythonSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast hiding (Equal, NotEqual)
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.Python

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)
import           Control.Exception (evaluate)

run :: Text -> Expression
run = py . unpack


spec :: Spec
spec = do
  describe "parse" $ do
    it "parses numbers" $ do
      py "1" `shouldBe` MuNumber 1

    it "parses booleans" $ do
      py "True" `shouldBe` MuBool True
      py "False" `shouldBe` MuBool False

    it "parses booleans in version2" $ do
      py2 "True" `shouldBe` MuBool True
      py2 "False" `shouldBe` MuBool False

    it "parses strings" $ do
      py "\"some string\"" `shouldBe` MuString "some string"

    it "parses strings sequences" $ do
      py "\"hello\" \"world\"" `shouldBe` MuString "helloworld"

    it "parses multi-line strings" $ do
      run [text|"""some
      string"""|] `shouldBe` MuString "some\nstring"

    it "parses lists" $ do
      py "[1,2,3]" `shouldBe` MuList [MuNumber 1, MuNumber 2, MuNumber 3]

    it "parses dictionaries" $ do
      py "{}" `shouldBe` MuDict None
      py "{'foo': 1}" `shouldBe` MuDict (Arrow (MuString "foo") (MuNumber 1))
      py "{'foo': 1, 'bar': 2}" `shouldBe` MuDict (Sequence [Arrow (MuString "foo") (MuNumber 1), Arrow (MuString "bar") (MuNumber 2)])

    it "parses sets as lists" $ do
      py "{1,2,3}" `shouldBe` MuList [MuNumber 1, MuNumber 2, MuNumber 3]

    it "parses assignment" $ do
      py "one = 1" `shouldBe` Assignment "one" (MuNumber 1.0)

    it "allows parentheses" $ do
      py "(123)" `shouldBe` MuNumber 123

    it "parses references" $ do
      py "x" `shouldBe` (Reference "x")

    it "parses application" $ do
      py "f(2)" `shouldBe` (Application (Reference "f") [MuNumber 2])

    it "parses message sending" $ do
      py "o.f(2)" `shouldBe` (Send (Reference "o") (Reference "f") [(MuNumber 2)])

    it "parses assign-operators" $ do
      py "x += 8" `shouldBe` (Assignment "x" (Application (Primitive Plus) [Reference "x",MuNumber 8.0]))

    it "parses binary operators" $ do
      py "x + y" `shouldBe` (Application (Primitive Plus) [Reference "x",Reference "y"])

    it "parses sequences" $ do
      py "1;2;3" `shouldBe` Sequence [MuNumber 1, MuNumber 2, MuNumber 3]

    it "parses unary operators" $ do
      py "not True" `shouldBe` (Application (Primitive Negation) [MuBool True])

    it "parses classes" $ do
      py "class DerivedClassName: pass" `shouldBe` Class "DerivedClassName" Nothing None

    it "parses inheritance" $ do
      py "class DerivedClassName(BaseClassName): pass" `shouldBe` Class "DerivedClassName" (Just "BaseClassName") None

    it "parses if, elif and else" $ do
      run [text|if True: 1
        elif False: 2
        else: 3|] `shouldBe` If (MuBool True) (MuNumber 1) (If (MuBool False) (MuNumber 2) (MuNumber 3))

    it "parses functions" $ do
      py "def foo(): return 1" `shouldBe` SimpleFunction "foo" [] (Return (MuNumber 1.0))

    it "parses procedures" $ do
      py "def foo(param): print(param)" `shouldBe` SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))

    it "parses whiles" $ do
      py "while True: pass" `shouldBe` While (MuBool True) None

    it "parses fors" $ do
      py "for x in range(0, 3): pass" `shouldBe` For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None

    it "parses tries" $ do
      run [text|
try:
    1
except IOError as e:
    2
except ValueError:
    3
except:
    4|] `shouldBe` Try (MuNumber 1) [
                    (AsPattern "e" (TypePattern "IOError"), MuNumber 2),
                    (TypePattern "ValueError", MuNumber 3),
                    (WildcardPattern, MuNumber 4)] None

    it "parses raise expressions" $ do
      py "raise" `shouldBe` Raise None

    it "parses raise expressions with exception" $ do
      py "raise Exception('something')" `shouldBe` Raise (Application (Reference "Exception") [MuString "something"])

    it "parses raise expressions with exception in version2 format" $ do
      py2 "raise Exception('something')" `shouldBe` Raise (Application (Reference "Exception") [MuString "something"])
      py2 "raise Exception, 'something'" `shouldBe` Raise (Application (Reference "Exception") [MuString "something"])
      py2 "raise Exception" `shouldBe` Raise (Reference "Exception")

    it "parses raise expressions with exception in version3 format" $ do
      py3 "raise Exception('something')" `shouldBe` Raise (Application (Reference "Exception") [MuString "something"])
      evaluate (py3 "raise Exception, 'something'") `shouldThrow` anyErrorCall
      py3 "raise Exception" `shouldBe` Raise (Reference "Exception")

    it "parses lambdas" $ do
      py "lambda x: 1" `shouldBe` Lambda [VariablePattern "x"] (MuNumber 1)

    it "parses tuples" $ do
      py "(1, \"something\")" `shouldBe` MuTuple [MuNumber 1, MuString "something"]

    it "parses yields" $ do
      py "yield 1" `shouldBe` Yield (MuNumber 1)

    it "parses field access" $ do
      py "x.y" `shouldBe` (FieldReference (Reference "x") "y")

    it "parses field assignment" $ do
      py "x.y = 2" `shouldBe` (FieldAssignment (Reference "x") "y" (MuNumber 2))

    it "parses test groups" $ do
      run [text|
        class TestGroup(unittest.TestCase):
          def test_something():
            self.assertTrue(True)
      |] `shouldBe` TestGroup (MuString "TestGroup")
                      (Test (MuString "test_something")
                        (Assert False $ Truth (MuBool True)))

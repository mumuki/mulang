{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module PythonSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
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

    it "parses arg keywords" $ do
      py "f(x=1)" `shouldBe` Application (Reference "f") [As "x" (MuNumber 1.0)]

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

    it "parses global variable access" $ do
      py "def foo(): \n\tglobal x\n\tx = 0" `shouldBe` (
        Procedure "foo" [Equation [] (UnguardedBody (Sequence [
          Application (Reference "global") [Reference "x"],
          Assignment "x" (MuNumber 0.0)
        ]))])

    it "parses binary operators" $ do
      py "x + y" `shouldBe` (Application (Primitive Plus) [Reference "x",Reference "y"])

    it "parses sequences" $ do
      py "1;2;3" `shouldBe` Sequence [MuNumber 1, MuNumber 2, MuNumber 3]

    it "parses unary operators" $ do
      py "not True" `shouldBe` (Application (Primitive Negation) [MuBool True])

    it "parses classes" $ do
      py "class Something: pass" `shouldBe` Class "Something" Nothing None

    it "parses classes with methods" $ do
      py "class Something:\n  def __init__(self): pass\n  def foo(self, x): return x" `shouldBe` (
        Class "Something" Nothing (Sequence [
          (SimpleMethod "__init__" [VariablePattern "self"] None),
          (SimpleMethod "foo" [VariablePattern "self", VariablePattern "x"] (Return (Reference "x")))
        ]))

    it "parses classes with static methods" $ do
      py "class Something:\n  @staticmethod\n  def foo(x): return x" `shouldBe` (
        Class "Something" Nothing (Decorator [Static]
          (SimpleMethod "foo" [VariablePattern "x"] (Return (Reference "x")))
        ))

    it "parses classes with class methods" $ do
      py "class Something:\n  @classmethod\n  def foo(cls, x): return x" `shouldBe` (
        Class "Something" Nothing (Decorator [Classy]
          (SimpleMethod "foo" [VariablePattern "cls", VariablePattern "x"] (Return (Reference "x")))
        ))

    it "does parse methods without self" $ do
      py "class Something:\n  def bar(): return None\n" `shouldBe` (
        Class "Something" Nothing (SimpleMethod "bar" [] (Return MuNil)))

    it "doesn't parse methods pseudo-methods outside a class" $ do
      py "def bar(self): return None\n" `shouldBe` (SimpleFunction "bar" [VariablePattern "self"] (Return MuNil))

    it "parses inheritance" $ do
      py "class DerivedClassName(BaseClassName): pass" `shouldBe` Class "DerivedClassName" (Just "BaseClassName") None


    it "parses inline if" $ do
      py "1 if cond else 0" `shouldBe` If (Reference "cond") (MuNumber 1) (MuNumber 0)


    it "parses if, elif and else" $ do
      run [text|if True: 1
        elif False: 2
        else: 3|] `shouldBe` If (MuBool True) (MuNumber 1) (If (MuBool False) (MuNumber 2) (MuNumber 3))

    it "parses functions" $ do
      py "def foo(): return 1" `shouldBe` SimpleFunction "foo" [] (Return (MuNumber 1.0))

    it "parses functions with optional args" $ do
      py "def foo(x=1): return x" `shouldBe` Function "foo" [
          Equation [DefaultPattern (VariablePattern "x") (MuNumber 1)] (UnguardedBody (Return (Reference "x")))
        ]

    it "parses print" $ do
      py "print()" `shouldBe` (Print None)
      py "print(x)" `shouldBe` (Print (Reference "x"))
      py "print(x, y)" `shouldBe` (Print (MuTuple [Reference "x", Reference "y"]))

    it "parses procedures" $ do
      py "def foo(param): print(param)" `shouldBe` SimpleProcedure "foo" [VariablePattern "param"] (Print (Reference "param"))

    it "parses whiles" $ do
      py "while True: pass" `shouldBe` While (MuBool True) None

    it "parses fors" $ do
      py "for x in range(0, 3): pass" `shouldBe` For [Generator (VariablePattern "x") (Application (Reference "range") [MuNumber 0.0,MuNumber 3.0])] None

    it "parses fors with patterns" $ do
      py "for (x, y) in enumerate(range(10, 30)): print('x', x, 'y', y)" `shouldBe` For [
          Generator (TuplePattern [
            VariablePattern "x",
            VariablePattern "y"
          ]) (Application (Reference "enumerate") [Application (Reference "range") [MuNumber 10, MuNumber 30]])
        ] (Print (MuTuple [MuString "x", Reference "x", MuString "y", Reference "y"]))

    it "parses fors with multiple vars" $ do
      py "for x, y in enumerate(range(10, 30)): print('x', x, 'y', y)" `shouldBe` For [
          Generator (TuplePattern [
            VariablePattern "x",
            VariablePattern "y"
          ]) (Application (Reference "enumerate") [Application (Reference "range") [MuNumber 10, MuNumber 30]])
        ] (Print (MuTuple [MuString "x", Reference "x", MuString "y", Reference "y"]))


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

    it "parses indexed dict access" $ do
      py "x['y']" `shouldBe` (Application (Primitive GetAt) [Reference "x", MuString "y"])

    it "parses indexed dict assignments" $ do
      py "x['y'] = 2" `shouldBe` Application (Primitive SetAt) [Reference "x", MuString "y", MuNumber 2.0]

    it "parses indexed dict removal" $ do
      py "del x['y']" `shouldBe` Application (Reference "del") [Application (Primitive GetAt) [Reference "x",MuString "y"]]

    it "parses list length" $ do
      py "len(x)" `shouldBe` Application (Primitive Size) [Reference "x"]

    it "parses slices with steps" $ do
      py "x[0:10:2]" `shouldBe` (Application (Primitive Slice) [Reference "x", MuNumber 0, MuNumber 10, MuNumber 2])

    it "parses slices without start" $ do
      py "x[:10]" `shouldBe` (Application (Primitive Slice) [Reference "x", None, MuNumber 10, None ])

    it "parses slices without end" $ do
      py "x[4:]" `shouldBe` (Application (Primitive Slice) [Reference "x", MuNumber 4, None, None ])

    it "parses slices with start and end" $ do
      py "x[4:9]" `shouldBe` (Application (Primitive Slice) [Reference "x", MuNumber 4, MuNumber 9, None ])

    it "parses slices without start and end" $ do
      py "x[:]" `shouldBe` (Application (Primitive Slice) [Reference "x", None, None, None ])

    it "parses indexed list access" $ do
      py "x[0]" `shouldBe` (Application (Primitive GetAt) [Reference "x", MuNumber 0.0])

    it "parses indexed list assignments" $ do
      py "x[0] = 2" `shouldBe` (Application (Primitive SetAt) [Reference "x", MuNumber 0.0, MuNumber 2.0])

    it "parses indexed list increments" $ do
      py "x[0] += 2" `shouldBe` (Application (Primitive SetAt) [
                                                Reference "x",
                                                MuNumber 0.0,
                                                Application (Primitive Plus) [
                                                  Application (Primitive GetAt) [Reference "x", MuNumber 0.0],
                                                  MuNumber 2.0
                                                ]
                                              ])

    it "parses indexed list decrements" $ do
      py "x[5] -= 10" `shouldBe` (Application (Primitive SetAt) [
                                                Reference "x",
                                                MuNumber 5.0,
                                                Application (Primitive Minus) [
                                                  Application (Primitive GetAt) [Reference "x", MuNumber 5.0],
                                                  MuNumber 10.0
                                                ]
                                              ])

    it "parses list comprehensions with one variable" $ do
      py "[x for x in xs]" `shouldBe` Application (Reference "list") [
          For [Generator (VariablePattern "x") (Reference "xs")] (Yield (Reference "x"))
        ]

    it "parses list comprehensions with two variables" $ do
      py "[x for x, y in xs]" `shouldBe` Application (Reference "list") [
          For [Generator (TuplePattern [VariablePattern "x",VariablePattern "y"]) (Reference "xs")] (Yield (Reference "x"))
        ]

    it "parses list comprehensions with tuple" $ do
      py "[x for (x, y) in xs]" `shouldBe` Application (Reference "list") [
          For [Generator (TuplePattern [VariablePattern "x", VariablePattern "y"]) (Reference "xs")] (Yield (Reference "x"))
        ]

    it "parses list comprehensions with if" $ do
      py "[x for x in xs if x]" `shouldBe` Application (Reference "list") [
          For [Generator (VariablePattern "x") (Reference "xs"), Guard (Reference "x")] (Yield (Reference "x"))
        ]

    it "parses list comprehensions with multiple if" $ do
      py "[x for x in xs if x if x > 0]" `shouldBe` Application (Reference "list") [
          For [
            Generator (VariablePattern "x") (Reference "xs"),
            Guard (Reference "x"),
            Guard (Application (Primitive GreaterThan) [Reference "x", MuNumber 0])
          ] (Yield (Reference "x"))
        ]

    it "parses list comprehensions with multiple if" $ do
      py "[(x, y) for x in xs if x > 0 for y in ys if y > 0]" `shouldBe` Application (Reference "list") [
          For [
            Generator (VariablePattern "x") (Reference "xs"),
            Guard (Application (Primitive GreaterThan) [Reference "x",MuNumber 0.0]),
            Generator (VariablePattern "y") (Reference "ys"),
            Guard (Application (Primitive GreaterThan) [Reference "y",MuNumber 0.0])
          ] (Yield (MuTuple [Reference "x",Reference "y"]))
        ]

    it "parses plain generators comprehensions" $ do
      py "(x for x in xs)" `shouldBe` (
          For [Generator (VariablePattern "x") (Reference "xs")] (Yield (Reference "x"))
        )

    it "parses set comprehensions" $ do
      py "{x for x in xs}" `shouldBe` Application (Reference "set") [
          For [Generator (VariablePattern "x") (Reference "xs")] (Yield (Reference "x"))
        ]

    it "parses dict comprehensions" $ do
      -- py "{x:y for (x,y) in xs}"}
      pending

    it "parses test groups" $ do
      run [text|
        class TestGroup(unittest.TestCase):
          def test_something():
            self.assertTrue(True)
      |] `shouldBe` TestGroup (MuString "TestGroup")
                      (Test (MuString "test_something")
                        (Assert False $ Truth (MuBool True)))

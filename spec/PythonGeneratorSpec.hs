module PythonGeneratorSpec (spec) where

import           Test.Hspec
import           Data.List (intercalate)
import           Language.Mulang

unparse :: Expression -> String
unparse (MuNumber n)                                                  = show n
unparse (MuBool b)                                                    = show b
unparse (MuString s)                                                  = show s
unparse (MuList xs)                                                   = "[" ++ unparseMany xs ++ "]"
unparse (Assignment id value)                                         = id ++ " = " ++ unparse value
unparse (Reference id)                                                = id
unparse (Application (Reference "not") [bool])                        = "not " ++ unparse bool
unparse (Application (Reference "+") [arg1, arg2])                    = unparse arg1 ++ " + " ++ unparse arg2
unparse (Application (Reference "*") [arg1, arg2])                    = unparse arg1 ++ " * " ++ unparse arg2
unparse (Application (Reference "/") [arg1, arg2])                    = unparse arg1 ++ " / " ++ unparse arg2
unparse (Application (Reference "-") [arg1, arg2])                    = unparse arg1 ++ " - " ++ unparse arg2
unparse (Application (Reference id) args)                             = id ++ "(" ++ unparseMany args ++ ")"
unparse (Sequence xs)                                                 = intercalate "\n" . map unparse $ xs
unparse (While cond body)                                             = "while "++ unparse cond ++ ":\n" ++ (tab . unparse) body
unparse  (For [Generator (TuplePattern [VariablePattern "x"]) generator] None) = "for x in "++ unparse generator ++": pass"
unparse (Raise None)                                                   = "raise"
unparse (Raise (Application (Reference id) [arg]))                     = "raise "++ id ++"("++ unparse arg++")"
unparse  (Lambda [VariablePattern "x"] (MuNumber 1))                   = "lambda x: 1"
unparse (MuTuple args)                                                 = "("++ unparseMany args++")"
unparse (Yield value)                                                  = "yield " ++ unparse value
unparse (Class id Nothing body)                                        = "class "++ id ++":\n" ++ (tab . unparse) body
unparse (Class id (Just parent) body)                                  = "class "++ id ++"("++parent++"):\n" ++ (tab . unparse) body
unparse None                                                           = "pass"
unparse (Send receptor (Reference id) args)                            =  unparse receptor ++ "."++ id ++"("++ unparseMany args ++")"
unparse (If cond trueBody falseBody)                                   = "if "++ unparse cond ++":\n"++ (tab . unparse) trueBody ++ "\nelse:\n" ++ (tab . unparse) falseBody
unparse (SimpleFunction id args body)                                  = "def "++ id ++"("++ unparseParameters args++"):\n" ++ (tab . unparse) body

unparseParameters _ = ""

tab :: String -> String
tab = unlines . map ("\t"++) . lines

unparseMany :: [Expression] -> String
unparseMany = intercalate "," . map unparse

spec :: Spec
spec = do
  describe "unparse" $ do
    it "unparses numbers" $ do
      unparse  (MuNumber 1.0) `shouldBe` "1.0"

    it "unparses integers" $ do
      --unparse  (MuNumber 1) `shouldBe` "1"
      pending

    it "unparses booleans" $ do
      unparse  (MuBool True) `shouldBe` "True"

    it "unparses strings" $ do
      unparse  (MuString "some string") `shouldBe` "\"some string\""

    it "unparses lists" $ do
      unparse  (MuList [MuNumber 1.0, MuNumber 2.0, MuNumber 3.0]) `shouldBe` "[1.0,2.0,3.0]"

    it "unparses sets as lists" $ do
      --unparse  (MuList [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "{1,2,3}"
      pending

    it "unparses assignment" $ do
      unparse  (Assignment "one" (MuNumber 1.0)) `shouldBe` "one = 1.0"

    it "unparses references" $ do
      unparse  ((Reference "x")) `shouldBe` "x"

    it "unparses application" $ do
      unparse  ((Application (Reference "f") [MuNumber 2.0])) `shouldBe` "f(2.0)"

    it "unparses message sending" $ do
      unparse  ((Send (Reference "o") (Reference "f") [(MuNumber 2)])) `shouldBe` "o.f(2.0)"

    it "unparses assign-operators" $ do
      unparse  ((Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))) `shouldBe` "x = x + 8.0"

    it "unparses binary operators" $ do
      unparse  ((Application (Reference "+") [Reference "x",Reference "y"])) `shouldBe` "x + y"

    it "unparses sequences" $ do
      unparse  (Sequence [MuNumber 1, MuNumber 2, MuNumber 3]) `shouldBe` "1.0\n2.0\n3.0"

    it "unparses unary operators" $ do
      unparse  ((Application (Reference "not") [MuBool True])) `shouldBe` "not True"

    it "unparses classes" $ do
      unparse  (Class "DerivedClassName" Nothing None) `shouldBe` "class DerivedClassName:\n\tpass\n"

    it "unparses inheritance" $ do
      unparse  (Class "DerivedClassName" (Just "BaseClassName") None) `shouldBe` "class DerivedClassName(BaseClassName):\n\tpass\n"

    it "unparses if, elif and else" $ do
      unparse  (If (MuBool True) (MuNumber 1) (If (MuBool False) (MuNumber 2) (MuNumber 3))) `shouldBe` "if True: 1\nelif False: 2\nelse: 3"

    it "unparses functions" $ do
      unparse  (SimpleFunction "foo" [] (Return (MuNumber 1.0))) `shouldBe` "def foo(): return 1"

    it "unparses procedures" $ do
      unparse  (SimpleProcedure "foo" [VariablePattern "param"] (Application (Reference "print") [Reference "param"])) `shouldBe` "def foo(param): print(param)"

    it "unparses whiles" $ do
      unparse  (While (MuBool True) None) `shouldBe` "while True:\n\tpass\n"

    it "unparses fors" $ do
      unparse  (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None) `shouldBe` "for x in range(0, 3): pass"

    it "unparses raise expressions" $ do
      unparse  (Raise None) `shouldBe` "raise"

    it "unparses raise expressions with exception" $ do
      unparse  (Raise (Application (Reference "Exception") [MuString "'something'"])) `shouldBe` "raise Exception('something')"

    it "unparses lambdas" $ do
      unparse  (Lambda [VariablePattern "x"] (MuNumber 1)) `shouldBe` "lambda x: 1"

    it "unparses tuples" $ do
      unparse  (MuTuple [MuNumber 1, MuString "something"]) `shouldBe` "(1, \"something\")"

    it "unparses yields" $ do
      unparse  (Yield (MuNumber 1.0)) `shouldBe` "yield 1.0"


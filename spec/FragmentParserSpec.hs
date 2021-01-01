module FragmentParserSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.FragmentParser
import           Language.Mulang.Transform.Normalizer


spec :: Spec
spec = do
  it "parses well formed expressions" $ do
    let fragment = CodeSample Haskell "x = 1"

    parseFragment Nothing fragment `shouldBe` Right (Variable "x" (MuNumber 1.0))

  it "parses well formed expressions with normalization options" $ do
    let convert = Just (defaultNormalizationOptions { convertObjectVariableIntoObject = True })
    let keep    = Just (defaultNormalizationOptions { convertObjectVariableIntoObject = False })
    let asDict  = Just (defaultNormalizationOptions { convertObjectIntoDict = True })
    let fragment = CodeSample JavaScript "let x = {}"

    parseFragment convert fragment `shouldBe` (Right (Object "x" None))
    parseFragment keep fragment `shouldBe` (Right (Variable "x" (MuObject None)))
    parseFragment asDict fragment `shouldBe` (Right (Variable "x" (MuDict None)))
    parseFragment Nothing fragment `shouldBe` (Right (Variable "x" (MuObject None)))

  it "parses malformed Haskell expressions" $ do
    let fragment = CodeSample Haskell "x 1"

    parseFragment Nothing fragment `shouldBe` Left "Parse error"

  it "parses malformed Java expressions" $ do
    let fragment = CodeSample Java "class Foo {  void  foo( {}   }"

    parseFragment Nothing fragment `shouldBe` Left "(line 1, column 25):\nunexpected OpenCurly\nexpecting refType"

  it "parses malformed JavaScript expressions" $ do
    let fragment = CodeSample Prolog "foo () :- 4 > 5."

    parseFragment Nothing fragment `shouldBe` Left "(line 1, column 5):\nunexpected \"(\"\nexpecting space or \":-\""

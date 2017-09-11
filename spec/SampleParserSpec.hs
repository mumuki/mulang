module SampleParserSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.SampleParser


spec :: Spec
spec = do
  it "parses well formed expressions" $ do
    let sample = CodeSample Haskell "x = 1"

    parseSample sample `shouldBe` Right (Variable "x" (MuNumber 1.0))

  it "parses malformed Haskell expressions" $ do
    let sample = CodeSample Haskell "x 1"

    parseSample sample `shouldBe` Left "Parse error"

  it "parses malformed Java expressions" $ do
    let sample = CodeSample Java "class Foo {  void  foo( {}   }"

    parseSample sample `shouldBe` Left "(line 1, column 25):\nunexpected OpenCurly\nexpecting refType"

  it "parses malformed JavaScript expressions" $ do
    let sample = CodeSample Prolog "foo () :- 4 > 5."

    parseSample sample `shouldBe` Left "(line 1, column 5):\nunexpected \"(\"\nexpecting space or \":-\""

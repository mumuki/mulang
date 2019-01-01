module FragmentParserSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.FragmentParser


spec :: Spec
spec = do
  it "parses well formed expressions" $ do
    let fragment = CodeFragment Haskell "x = 1"

    parseFragment fragment `shouldBe` Right (Variable "x" (MuNumber 1.0))

  it "parses malformed Haskell expressions" $ do
    let fragment = CodeFragment Haskell "x 1"

    parseFragment fragment `shouldBe` Left "Parse error"

  it "parses malformed Java expressions" $ do
    let fragment = CodeFragment Java "class Foo {  void  foo( {}   }"

    parseFragment fragment `shouldBe` Left "(line 1, column 25):\nunexpected OpenCurly\nexpecting refType"

  it "parses malformed JavaScript expressions" $ do
    let fragment = CodeFragment Prolog "foo () :- 4 > 5."

    parseFragment fragment `shouldBe` Left "(line 1, column 5):\nunexpected \"(\"\nexpecting space or \":-\""

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module CSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast hiding (Equal, NotEqual)
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.C

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = c . unpack

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses var declaration" $ do
      run "int a;" `shouldBe` Sequence [
          TypeSignature "a" (SimpleType "int" []),
          Variable "a" None
        ]

    it "parses pointer to var declaration" $ do
      run "int *a;" `shouldBe` Sequence [
          TypeSignature "*a" (SimpleType "int" []),
          Variable "a" None
        ]

    it "parses array without size var declaration" $ do
      run "int a[];" `shouldBe` Sequence [
          TypeSignature "a[]" (SimpleType "int" []),
          Variable "a" None
        ]

    it "parses array with size var declaration" $ do
      run "int a[10];" `shouldBe` Sequence [
          TypeSignature "a[10]" (SimpleType "int" []),
          Variable "a" None
        ]
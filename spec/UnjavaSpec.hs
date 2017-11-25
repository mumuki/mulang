{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module UnjavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Unparsers.Java (unjava)

import           Data.Text (Text, unpack, pack)
import           NeatInterpolation (text)

checkRoundTrip expression =  (java.unjava) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "unparse" $ do
    it "unparses simple Class" $ do
      checkRoundTrip (Class "Foo" Nothing MuNull)

    it "unparses simple Class with superclass" $ do
      checkRoundTrip (Class "Foo" (Just "Bar") MuNull)

    it "unparses Simple Interface" $ do
      checkRoundTrip (Interface "Foo" [] MuNull)

    it "unparses Interface with messages" $ do
      checkRoundTrip (Interface "Foo" [] (TypeSignature "foo" [] "void"))

    it "unparses Interface with non-void messages" $ do
      checkRoundTrip (Interface "Foo" [] (TypeSignature "foo" [] "int"))

    it "unparses Interface with Many messages" $ do
      checkRoundTrip (Interface "Foo" [] (Sequence [
                                  TypeSignature "foo" [] "void",
                                  TypeSignature "bar" [] "int"]))

    it "unparses Interface with messages With Params" $ do
      checkRoundTrip (Interface "Foo" [] (TypeSignature "foo" ["String", "int"] "void"))

    it "unparses Interface with superinterfaces" $ do
      checkRoundTrip (Interface "Foo" ["Bar", "Baz"] MuNull)

    it "unparses Class With Methods" $ do
      checkRoundTrip (Class "Foo" Nothing (SimpleMethod "hello" [] MuNull))

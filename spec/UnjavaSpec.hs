{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module UnjavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Unparsers.Java (unparseJava)

shouldRoundTrip expression =  (java.unparseJava) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "roundTrip" $ do
    it "roundTrips simple Class" $ do
      shouldRoundTrip (Class "Foo" Nothing None)

    it "roundTrips simple Class with superclass" $ do
      shouldRoundTrip (Class "Foo" (Just "Bar") None)

    it "roundTrips Simple Interface" $ do
      shouldRoundTrip (Interface "Foo" [] None)

    it "roundTrips Interface with messages" $ do
      shouldRoundTrip (Interface "Foo" [] (SubroutineSignature "foo" [] "void" []))

    it "roundTrips Interface with non-void messages" $ do
      shouldRoundTrip (Interface "Foo" [] (SubroutineSignature "foo" [] "int" []))

    it "roundTrips Interface with Many messages" $ do
      shouldRoundTrip (Interface "Foo" [] (Sequence [
                                  SubroutineSignature "foo" [] "void" [],
                                  SubroutineSignature "bar" [] "int" []]))

    it "roundTrips Interface with messages With Params" $ do
      shouldRoundTrip (Interface "Foo" [] (SubroutineSignature "foo" ["String", "int"] "void" []))

    it "roundTrips Interface with superinterfaces" $ do
      shouldRoundTrip (Interface "Foo" ["Bar", "Baz"] None)

    it "roundTrips Class With Methods" $ do
      pending
      -- shouldRoundTrip (Class "Foo" Nothing (Sequence [
      --                   TypeSignature "hello" (ParameterizedType [] "void" []),
      --                   SimpleMethod "hello" [] None
      --                 ]))

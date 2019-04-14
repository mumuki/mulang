{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module UnjavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Unparsers.Java (unjava)

checkRoundTrip expression =  (java.unjava) expression `shouldBe` expression

spec :: Spec
spec = do
  describe "unparse" $ do
    it "unparses simple Class" $ do
      checkRoundTrip (Class "Foo" Nothing None)

    it "unparses simple Class with superclass" $ do
      checkRoundTrip (Class "Foo" (Just "Bar") None)

    it "unparses Simple Interface" $ do
      checkRoundTrip (Interface "Foo" [] None)

    it "unparses Interface with messages" $ do
      checkRoundTrip (Interface "Foo" [] (SubroutineSignature "foo" [] "void" []))

    it "unparses Interface with non-void messages" $ do
      checkRoundTrip (Interface "Foo" [] (SubroutineSignature "foo" [] "int" []))

    it "unparses Interface with Many messages" $ do
      checkRoundTrip (Interface "Foo" [] (Sequence [
                                  SubroutineSignature "foo" [] "void" [],
                                  SubroutineSignature "bar" [] "int" []]))

    it "unparses Interface with messages With Params" $ do
      checkRoundTrip (Interface "Foo" [] (SubroutineSignature "foo" ["String", "int"] "void" []))

    it "unparses Interface with superinterfaces" $ do
      checkRoundTrip (Interface "Foo" ["Bar", "Baz"] None)

    it "unparses Class With Methods" $ do
      pending
      -- checkRoundTrip (Class "Foo" Nothing (Sequence [
      --                   TypeSignature "hello" (ParameterizedType [] "void" []),
      --                   SimpleMethod "hello" [] None
      --                 ]))

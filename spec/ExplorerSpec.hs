{-# LANGUAGE OverloadedStrings #-}

module ExplorerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Explorer
import           Language.Mulang.Parsers.Haskell ()

spec :: Spec
spec = do
  describe "bindingsOf" $ do
    it "answers bindings for binding" $ do
      let code = "f x =  (:[]) . m x y . g h 2"
      (bindingsOf "f" code) `shouldBe` [".","m","x","y", "g","h"]

  describe "transitiveBindingsOf" $ do
    it "answers transitive bindings for binding" $ do
      let code = "f x = m x\n\
                 \m 0 = p 0\n\
                 \p x = g x"
      (transitiveBindingsOf "f" code) `shouldBe` ["f","m","x","p","g"]


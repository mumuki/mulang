{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ReplSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Interpreter.Repl

spec :: Spec
spec = do
  describe "evalNext" $ do
    let session = newSession js
    it "evals and returns" $ do
      let (_, r1) = evalNext newSession "1"
      r1 `shouldBe` (MuNumber 1)


module HaskellSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Haskell

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses left infix partial application" $ do
      hs "f = (1+)" `shouldBe` Variable "f" (Application (Reference "+") [MuNumber 1.0])

    it "parses right infix partial application" $ do
      hs "f = (+1)" `shouldBe` Variable "f" (Application (Reference "+") [MuNumber 1.0])

module CompilerSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import           Data.Aeson
import           Language.Mulang.Analyzer.Compiler as C
import           Test.Hspec

spec = describe "Compiler" $ do
  describe "advanced expectations" $ do
    let advancedExpJson = LBS.pack "{\"subject\":[\"x\"],\"tag\":\"Advanced\",\"transitive\":false,\"negated\":true,\"object\":{\"tag\":\"Anyone\",\"contents\":[]},\"verb\":\"uses\"}"
    let advancedExp = C.Advanced ["x"] "uses" Anyone False True

    it "can be decoded from JSON" $ do
      (decode advancedExpJson :: Maybe C.Expectation) `shouldBe` Just advancedExp

    it "can be encoded to JSON" $ do
      encode advancedExp `shouldBe` advancedExpJson

  describe "basic expectations" $ do
    let basicExpJson = LBS.pack "{\"tag\":\"Basic\",\"inspection\":\"HasBinding\",\"binding\":\"x\"}"
    let basicExp = C.Basic "x" "HasBinding"

    it "can be decoded from JSON" $ do
      (decode basicExpJson :: Maybe C.Expectation) `shouldBe` Just basicExp

    it "can be encoded to JSON" $ do
      encode basicExp `shouldBe` basicExpJson
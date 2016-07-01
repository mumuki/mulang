module CompilerSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import           Data.Aeson
import           Language.Mulang.Cli.Compiler as C
import           Test.Hspec

expectationJson = LBS.pack "{\"subject\":[\"x\"],\"transitive\":false,\"negated\":true,\"object\":{\"tag\":\"Anyone\",\"contents\":[]},\"verb\":\"uses\"}"
expectation = C.Expectation ["x"] "uses" Anyone True False

spec = describe "Compiler" $ do
  it "can decode expectation JSON" $ do
    (decode expectationJson :: Maybe C.Expectation) `shouldBe` Just expectation

  it "can encode expectation JSON" $ do
    encode expectation `shouldBe` expectationJson
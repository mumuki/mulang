module CompilerSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import           Data.Aeson
import           Language.Mulang.Cli.Compiler as C
import           Test.Hspec

spec = describe "Compiler" $ do
  it "can decode expectation JSON" $ do
    let json = "{\"subject\":[\"x\"],\"transitive\":false,\"negated\":true,\"object\":{\"tag\":\"Anyone\",\"contents\":[]},\"verb\":\"uses\"}"
    let decoded = (decode . LBS.pack) json :: Maybe C.Expectation
    decoded `shouldBe` Just (C.Expectation ["x"] "uses" Anyone True False)
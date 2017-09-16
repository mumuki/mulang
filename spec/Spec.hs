module Main where

import Test.Hspec
import qualified UnfoldSpec
import qualified TokenizerSpec
import qualified JavaSpec
import qualified ExpectationsCompilerSpec
import qualified CombinerSpec
import qualified InspectorSpec
import qualified AnalysisJsonSpec
import qualified JavaScriptSpec
import qualified LogicSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "UnfoldSpec"     UnfoldSpec.spec
  describe "CombinerSpec"     CombinerSpec.spec
  describe "InspectorSpec"     InspectorSpec.spec
  describe "ExpectationsCompilerSpec"     ExpectationsCompilerSpec.spec
  describe "JavaSpec"     JavaSpec.spec
  describe "TokenizerSpec"     TokenizerSpec.spec
  describe "AnalysisJsonSpec"     AnalysisJsonSpec.spec
  describe "LogicSpec"     LogicSpec.spec
  describe "JavaScriptSpec"     JavaScriptSpec.spec

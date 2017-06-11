 {-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module AnalysisJsonSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.Analysis.Json ()
import           Language.Mulang.Analyzer (noSmells, onlySmells, allSmells)
import           Language.Mulang.Ast
import           Data.Maybe (fromJust)
import           Data.Aeson
import           Test.Hspec
import           NeatInterpolation

run json = fromJust . decode $ json

spec = describe "AnalysisJson" $ do
  it "works with advanced expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (AnalysisSpec [Advanced ["x"] "uses" Anyone False False] noSmells NoSignatures)
    let json = LBS.pack [string|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "tag" : "Advanced",
            "subject" : [ "x" ],
            "object" : { "tag" : "Anyone" },
            "negated" : false,
            "verb" : "uses",
            "transitive" : false
         }
      ],
      "smellsSet" : { "tag" : "NoSmells" },
      "signatureAnalysisType" : { "tag" : "NoSignatures" }
   }
} |]
    run json `shouldBe` analysis

  it "works with basic expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (AnalysisSpec [Basic "x" "HasBinding"] noSmells NoSignatures)
    let json = LBS.pack [string|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "signatureAnalysisType" : { "tag" : "NoSignatures" },
      "smellsSet" : { "tag" : "NoSmells" },
      "expectations" : [
         {
            "tag" : "Basic",
            "binding" : "x",
            "inspection" : "HasBinding"
         }
      ]
   }
} |]

    run json `shouldBe` analysis

  it "works with signature analysis" $ do
    let json = LBS.pack [string|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return x + y; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : { "tag" : "NoSmells" },
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function foo(x, y) { return x + y; }")
                            (AnalysisSpec [] noSmells (StyledSignatures HaskellStyle))

    run json `shouldBe` analysis

  it "works with mulang code" $ do
    let json = LBS.pack [string|
{
   "sample" : {
      "tag" : "MulangSample",
      "ast" : {
         "tag" : "Sequence",
         "sequenceElements" : [
            {
               "variableInitializer" : {
                  "tag" : "MuNumber",
                  "numberValue" : 1
               },
               "variableId" : "x",
               "tag" : "Variable"
            },
            {
               "tag" : "Variable",
               "variableId" : "y",
               "variableInitializer" : {
                  "tag" : "MuNumber",
                  "numberValue" : 2
               }
            }
         ]
      }
   },
   "spec" : {
      "smellsSet" : {
        "tag" : "NoSmells"
      },
      "signatureAnalysisType" : {
         "tag" : "StyledSignatures",
         "style" : "HaskellStyle"
      },
      "expectations" : []
   }
}|]
    let analysis = Analysis (MulangSample (Sequence [Variable "x" (MuNumber 1), Variable "y" (MuNumber 2)]))
                            (AnalysisSpec [] noSmells (StyledSignatures HaskellStyle))

    run json `shouldBe` analysis

  it "works with inclusion smells analysis" $ do
    let json = LBS.pack [string|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "tag" : "OnlySmells",
        "include" : [
          "ReturnsNull",
          "DoesNullTest"
        ]
      },
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function foo(x, y) { return null; }")
                            (AnalysisSpec [] onlySmells { include = [ReturnsNull, DoesNullTest]} (StyledSignatures HaskellStyle))

    run json `shouldBe` analysis

  it "works with exclusion smells analysis" $ do
    let json = LBS.pack [string|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "tag" : "AllSmells",
        "exclude" : [
          "ReturnsNull"
        ]
      },
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function foo(x, y) { return null; }")
                            (AnalysisSpec [] allSmells { exclude = [ReturnsNull]} (StyledSignatures HaskellStyle))

    run json `shouldBe` analysis

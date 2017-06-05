 {-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module AnalysisJsonSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.Analysis.Json ()
import           Language.Mulang.Analyzer (noSmells, allSmells)
import           Language.Mulang.Ast
import           Data.Maybe (fromJust)
import           Data.Aeson
import           Test.Hspec
import           NeatInterpolation

run json = fromJust . decode $ json

spec = describe "AnalisisJson" $ do
  it "works with advanced expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (AnalysisSpec [Advanced ["x"] "uses" Anyone False False] noSmells NoSignatures)
    let json = LBS.pack [string|
{
   "sample" : {
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "subject" : [
               "x"
            ],
            "object" : "Anyone",
            "negated" : false,
            "verb" : "uses",
            "transitive" : false
         }
      ],
      "smellsSet" : {
        "include" : []
      },
      "signatureAnalysisType" : "NoSignatures"
   }
} |]
    run json `shouldBe` analysis

  it "works with basic expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (AnalysisSpec [Basic "x" "HasBinding"] noSmells NoSignatures)
    let json = LBS.pack [string|
{
   "sample" : {
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "signatureAnalysisType" : "NoSignatures",
      "smellsSet" : {
        "include" : []
      },
      "expectations" : [
         {
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
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return x + y; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "include" : []
      },
      "signatureAnalysisType" : {
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
      "ast" : {
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
         ],
         "tag" : "Sequence"
      }
   },
   "spec" : {
      "smellsSet" : {
        "include" : []
      },
      "signatureAnalysisType" : {
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
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "include" : [
          "ReturnsNull",
          "DoesNullTest"
        ]
      },
      "signatureAnalysisType" : {
        "style" : "HaskellStyle"
      }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function foo(x, y) { return null; }")
                            (AnalysisSpec [] noSmells { include = [ReturnsNull, DoesNullTest]} (StyledSignatures HaskellStyle))

    run json `shouldBe` analysis

  it "works with exclusion smells analysis" $ do
    let json = LBS.pack [string|
{
   "sample" : {
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "exclude" : [
          "ReturnsNull"
        ]
      },
      "signatureAnalysisType" : {
        "style" : "HaskellStyle"
      }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function foo(x, y) { return null; }")
                            (AnalysisSpec [] allSmells { exclude = [ReturnsNull]} (StyledSignatures HaskellStyle))

    run json `shouldBe` analysis

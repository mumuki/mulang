{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module AnalysisJsonSpec(spec) where

import           Test.Hspec

import           Data.Text (unpack, Text)
import           Data.ByteString.Lazy.Char8 (pack, ByteString)

import           Language.Mulang.Analyzer.Analysis.Json ()
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Ast

import           Data.Maybe (fromJust)
import           Data.Aeson (decode)
import           NeatInterpolation (text)

run :: Text -> Analysis
run = fromJust . decode . textToLazyByteString

textToLazyByteString :: Text -> ByteString
textToLazyByteString = pack . unpack

spec = describe "AnalysisJson" $ do
  it "works with Intransitive expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (AnalysisSpec [Expectation "Intransitive:x" "Uses:*"] noSmells Nothing Nothing)
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : "Intransitive:x",
            "inspection" : "Uses:*"
         }
      ],
      "smellsSet" : { "tag" : "NoSmells" }
   }
} |]
    run json `shouldBe` analysis

  it "works with transitive expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (AnalysisSpec [Expectation "x" "Declares"] noSmells Nothing Nothing)
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "smellsSet" : { "tag" : "NoSmells" },
      "expectations" : [
         {
            "binding" : "x",
            "inspection" : "Declares"
         }
      ]
   }
} |]

    run json `shouldBe` analysis

  it "works with signature analysis" $ do
    let json = [text|
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
                            (emptyAnalysisSpec { signatureAnalysisType = Just (StyledSignatures HaskellStyle) })

    run json `shouldBe` analysis

  it "works with mulang code" $ do
    let json = [text|
{
   "sample" : {
      "tag" : "MulangSample",
      "ast" : {
         "tag" : "Sequence",
         "contents" : [
            {
              "tag" : "Variable",
              "contents" : [
                "x",
                { "tag" : "MuNumber", "contents" : 1 }
              ]
            },
            {
              "tag" : "Variable",
              "contents" : [
                "y",
                { "tag" : "MuNumber", "contents" : 2 }
              ]
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
                            (emptyAnalysisSpec { signatureAnalysisType = Just (StyledSignatures HaskellStyle) })

    run json `shouldBe` analysis

  it "works with inclusion smells analysis" $ do
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "tag" : "NoSmells",
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
                            (emptyAnalysisSpec {
                              smellsSet = noSmells { include = Just ["ReturnsNull", "DoesNullTest"]},
                              signatureAnalysisType = Just (StyledSignatures HaskellStyle) })

    run json `shouldBe` analysis

  it "works with exclusion smells analysis" $ do
    let json = [text|
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
                            (emptyAnalysisSpec {
                              smellsSet = allSmells { exclude = Just ["ReturnsNull"]},
                              signatureAnalysisType = Just (StyledSignatures HaskellStyle) })

    run json `shouldBe` analysis

  it "works with caseStyle and minimumIdentifierSize" $ do
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Prolog",
      "content" : "son(Parent, Son):-parentOf(Son, Parent).parentOf(bart, homer)."
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : {
         "caseStyle" : "SnakeCase",
         "minimumIdentifierSize" : 4,
         "jargon" : ["id"]
      }
   }
} |]
    let analysis = Analysis (CodeSample Prolog "son(Parent, Son):-parentOf(Son, Parent).parentOf(bart, homer).")
                            (emptyAnalysisSpec {
                              smellsSet = allSmells,
                              domainLanguage = Just emptyDomainLanguage {
                                caseStyle = Just SnakeCase,
                                minimumIdentifierSize = Just 4,
                                jargon = Just ["id"] } })

    run json `shouldBe` analysis

  it "works with dictionary path" $ do
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function f(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : { "dictionaryFilePath" : "/usr/share/dict/words" }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function f(x, y) { return null; }")
                            (emptyAnalysisSpec {
                              smellsSet = allSmells,
                              domainLanguage = Just emptyDomainLanguage { dictionaryFilePath = Just "/usr/share/dict/words" } })

    run json `shouldBe` analysis

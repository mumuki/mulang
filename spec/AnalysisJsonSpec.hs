{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module AnalysisJsonSpec(spec) where

import           Test.Hspec

import qualified Data.Map.Strict as Map
import           Data.Text (unpack, Text)
import           Data.ByteString.Lazy.Char8 (pack, ByteString)

import           Language.Mulang.Analyzer.Analysis.Json ()
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Ast
import           Language.Mulang.Ast.Operator (Operator(..))
import           Language.Mulang.Transform.Normalizer (NormalizationOptions (..), unnormalized)

import           Data.Maybe (fromJust)
import           Data.Aeson (decode)
import           NeatInterpolation (text)

run :: Text -> Analysis
run = fromJust . decode . textToLazyByteString

textToLazyByteString :: Text -> ByteString
textToLazyByteString = pack . unpack

spec = describe "AnalysisJson" $ do
  it "works with Intransitive expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = z + 1")
                            (emptyAnalysisSpec { expectations = Just [Expectation "Intransitive:x" "Uses:z"] })
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = z + 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : "Intransitive:x",
            "inspection" : "Uses:z"
         }
      ]
   }
} |]
    run json `shouldBe` analysis

  it "works with transitive expectations" $ do
    let analysis = Analysis (CodeSample Haskell "x = 1")
                            (emptyAnalysisSpec { expectations = Just [Expectation "x" "Declares"] })
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
            "binding" : "x",
            "inspection" : "Declares"
         }
      ]
   }
} |]

    run json `shouldBe` analysis

  it "works with custom expectations" $ do
      let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function plusOne(x) { return x + 1 }"
   },
   "spec" : {
      "customExpectations" : "expectation: declares function `plusOne` that (returns with math);\nexpectation: !declares variable with literal"
   }
} |]
      let analysis = Analysis (CodeSample JavaScript "function plusOne(x) { return x + 1 }")
                              (emptyAnalysisSpec {
                                customExpectations = Just "expectation: declares function `plusOne` that (returns with math);\nexpectation: !declares variable with literal" })

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
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function foo(x, y) { return x + y; }")
                            (emptyAnalysisSpec { signatureAnalysisType = Just (StyledSignatures HaskellStyle) })

    run json `shouldBe` analysis

  it "works with trasnform analysis" $ do
    let json = [text|
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "plus(count([]), 1)"
   },
   "spec" : {
      "transformationSpecs" : [
        [
          {"tag" : "Crop", "contents": "IsVariable:y"},
          {"tag" : "RenameVariables"},
          {"tag" : "Alias", "contents": { "count": "Size", "plus": "Plus" }}
        ]
      ]
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "plus(count([]), 1)")
                            (emptyAnalysisSpec { transformationSpecs = Just [
                              [
                                Crop "IsVariable:y",
                                RenameVariables,
                                Alias (Map.fromList [("count",Size),("plus",Plus)])
                              ]
                            ]})

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
      "signatureAnalysisType" : {
         "tag" : "StyledSignatures",
         "style" : "HaskellStyle"
      }
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
      "smellsSet" : {
        "tag" : "NoSmells",
        "include" : [
          "ReturnsNil",
          "DoesNilTest"
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
                              smellsSet = (noSmellsBut ["ReturnsNil", "DoesNilTest"]),
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
      "smellsSet" : {
        "tag" : "AllSmells",
        "exclude" : [
          "ReturnsNil"
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
                              smellsSet = allSmellsBut ["ReturnsNil"],
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
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : { "dictionaryFilePath" : "/usr/share/dict/words" }
   }
} |]
    let analysis = Analysis (CodeSample JavaScript "function f(x, y) { return null; }")
                            (emptyAnalysisSpec {
                              smellsSet = allSmells,
                              domainLanguage = Just emptyDomainLanguage { dictionaryFilePath = Just "/usr/share/dict/words" } })

    run json `shouldBe` analysis

  it "works with normalization and ast generation" $ do
    let json = [text|
{
  "sample": {
    "tag": "MulangSample",
    "ast": {
      "tag": "Procedure",
      "contents": [
        "foo",
        [
          [
            [
              {
                "tag": "VariablePattern",
                "contents": "x"
              }
            ],
            {
              "tag": "UnguardedBody",
              "contents": {
                "tag": "Application",
                "contents": [
                  {
                    "tag": "Primitive",
                    "contents": "Multiply"
                  },
                  [
                    {
                      "tag": "MuNumber",
                      "contents": 2
                    },
                    {
                      "tag": "Reference",
                      "contents": "x"
                    }
                  ]
                ]
              }
            }
          ]
        ]
      ]
    }
  },
  "spec": {
    "normalizationOptions": {
      "insertImplicitReturn": true
    },
    "includeOutputAst": true
  }
} |]
    let analysis = Analysis (MulangSample (SimpleProcedure "foo" [VariablePattern "x"]
                                              (Application (Primitive Multiply) [MuNumber 2.0,Reference "x"])))
                            (emptyAnalysisSpec {
                              normalizationOptions = Just (unnormalized {
                                insertImplicitReturn = True
                              }),
                              includeOutputAst = Just True
                            })

    run json `shouldBe` analysis

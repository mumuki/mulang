module AutocorrectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer.Autocorrector
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import qualified Language.Mulang.Analyzer.Analysis as A
import           Data.Maybe (fromJust)

run language = head . fromJust . expectations . A.spec . autocorrect . expectationsAnalysis (CodeSample language "foo") . (:[])

spec :: Spec
spec = do
  describe "correct primitive usages" $ do
    it "corrects haskell otherwise negated" $ do
      run Haskell (Expectation "*" "Not:Uses:otherwise")  `shouldBe` (Expectation "*" "Not:UsesOtherwise")

    it "corrects haskell otherwise" $ do
      run Haskell (Expectation "*" "Uses:otherwise")  `shouldBe` (Expectation "*" "UsesOtherwise")

    it "corrects haskell and" $ do
      run Haskell (Expectation "*" "Uses:and") `shouldBe` (Expectation "*" "Uses:and")
      run Haskell (Expectation "*" "Uses:&&") `shouldBe` (Expectation "*" "UsesAnd")

    it "corrects haskell or" $ do
      run Haskell (Expectation "*" "Uses:or") `shouldBe` (Expectation "*" "Uses:or")
      run Haskell (Expectation "*" "Uses:||")`shouldBe` (Expectation "*" "UsesOr")

    it "corrects haskell not" $ do
      run Haskell (Expectation "*" "Uses:not") `shouldBe` (Expectation "*" "UsesNegation")
      run Haskell (Expectation "*" "Uses:!")  `shouldBe` (Expectation "*" "Uses:!")

    it "corrects java and" $ do
      run Java (Expectation "*" "Uses:and") `shouldBe` (Expectation "*" "Uses:and")
      run Java (Expectation "*"  "Uses:&&") `shouldBe` (Expectation "*" "UsesAnd")

    it "corrects python and" $ do
      run Python (Expectation "*" "Uses:and") `shouldBe` (Expectation "*" "UsesAnd")
      run Python (Expectation "*"  "Uses:&&") `shouldBe` (Expectation "*" "Uses:&&")

    it "corrects ruby and" $ do
      run Ruby (Expectation "*" "Uses:and") `shouldBe` (Expectation "*" "UsesAnd")
      run Ruby (Expectation "*"  "Uses:&&") `shouldBe` (Expectation "*" "UsesAnd")

  describe "correct primitive declarations" $ do
    it "corrects haskell otherwise negated" $ do
      run Haskell (Expectation "*" "Not:Declares:otherwise")  `shouldBe` (Expectation "*" "Not:DeclaresOtherwise")

    it "corrects haskell otherwise" $ do
      run Haskell (Expectation "*" "Declares:otherwise")  `shouldBe` (Expectation "*" "DeclaresOtherwise")

    it "corrects haskell and" $ do
      run Haskell (Expectation "*" "Declares:and") `shouldBe` (Expectation "*" "Declares:and")
      run Haskell (Expectation "*" "Declares:&&") `shouldBe` (Expectation "*" "DeclaresAnd")

    it "corrects haskell or" $ do
      run Haskell (Expectation "*" "Declares:or") `shouldBe` (Expectation "*" "Declares:or")
      run Haskell (Expectation "*" "Declares:||")`shouldBe` (Expectation "*" "DeclaresOr")

    it "corrects haskell not" $ do
      run Haskell (Expectation "*" "Declares:not") `shouldBe` (Expectation "*" "DeclaresNegation")
      run Haskell (Expectation "*" "Declares:!")  `shouldBe` (Expectation "*" "Declares:!")

    it "corrects java and" $ do
      run Java (Expectation "*" "Declares:and") `shouldBe` (Expectation "*" "Declares:and")
      run Java (Expectation "*"  "Declares:&&") `shouldBe` (Expectation "*" "DeclaresAnd")

    it "corrects python and" $ do
      run Python (Expectation "*" "Declares:and") `shouldBe` (Expectation "*" "DeclaresAnd")
      run Python (Expectation "*"  "Declares:&&") `shouldBe` (Expectation "*" "Declares:&&")

    it "corrects ruby and" $ do
      run Ruby (Expectation "*" "Declares:and") `shouldBe` (Expectation "*" "DeclaresAnd")
      run Ruby (Expectation "*"  "Declares:&&") `shouldBe` (Expectation "*" "DeclaresAnd")

  describe "corrects keyword usages" $ do
    it "corrects haskell type usage with negation" $ do
      run Haskell (Expectation "*" "Not:Uses:type") `shouldBe` (Expectation "*" "Not:DeclaresTypeAlias")

    it "corrects haskell type usage" $ do
      run Haskell (Expectation "*" "Uses:type") `shouldBe` (Expectation "*" "DeclaresTypeAlias")

    it "corrects java if usage" $ do
      run Java (Expectation "*" "Uses:if") `shouldBe` (Expectation "*"  "UsesIf")

    it "corrects java class usage" $ do
      run Java (Expectation "*" "Uses:class") `shouldBe` (Expectation "*" "DeclaresClass")

    it "corrects java interface usage" $ do
      run Java (Expectation "*" "Uses:interface") `shouldBe` (Expectation "*" "DeclaresInterface")

    it "corrects java for usage" $ do
      run Java (Expectation "*" "Uses:for") `shouldBe` (Expectation "*" "UsesForLoop")

    it "corrects python def usage" $ do
      run Python (Expectation "*" "Uses:def") `shouldBe` (Expectation "*" "DeclaresComputation")

    it "corrects ruby class usage" $ do
      run Ruby (Expectation "*" "Uses:class") `shouldBe` (Expectation "*" "DeclaresClass")

    it "corrects ruby include usage" $ do
      run Ruby (Expectation "*" "Uses:include") `shouldBe` (Expectation "*" "Includes")

    it "corrects ruby def usage" $ do
      run Ruby (Expectation "*" "Uses:def") `shouldBe` (Expectation "*" "DeclaresComputation")


  describe "corrects keyword declarations" $ do
    it "corrects haskell type declaration with negation" $ do
      run Haskell (Expectation "*" "Not:Declares:type") `shouldBe` (Expectation "*" "Not:DeclaresTypeAlias")

    it "corrects haskell type declaration" $ do
      run Haskell (Expectation "*" "Declares:type") `shouldBe` (Expectation "*" "DeclaresTypeAlias")

    it "corrects java if declaration" $ do
      run Java (Expectation "*" "Declares:if") `shouldBe` (Expectation "*"  "UsesIf")

    it "corrects java class declaration" $ do
      run Java (Expectation "*" "Declares:class") `shouldBe` (Expectation "*" "DeclaresClass")

    it "corrects java interface declaration" $ do
      run Java (Expectation "*" "Declares:interface") `shouldBe` (Expectation "*" "DeclaresInterface")

    it "corrects java for declaration" $ do
      run Java (Expectation "*" "Declares:for") `shouldBe` (Expectation "*" "UsesForLoop")

    it "corrects python def declaration" $ do
      run Python (Expectation "*" "Declares:def") `shouldBe` (Expectation "*" "DeclaresComputation")

    it "corrects ruby class declaration" $ do
      run Ruby (Expectation "*" "Declares:class") `shouldBe` (Expectation "*" "DeclaresClass")

    it "corrects ruby include declaration" $ do
      run Ruby (Expectation "*" "Declares:include") `shouldBe` (Expectation "*" "Includes")

    it "corrects ruby def declaration" $ do
      run Ruby (Expectation "*" "Declares:def") `shouldBe` (Expectation "*" "DeclaresComputation")

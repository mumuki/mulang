module AutocorrectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Analyzer.Autocorrector
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import qualified Language.Mulang.Analyzer.Analysis as A
import           Data.Maybe (fromJust)
import qualified Data.Map as Map

transform = head . fromJust . expectations . A.spec . autocorrect

run language = transform . expectationsAnalysis (CodeSample language "foo") . (:[])
runWithCustomRules language code rules e = transform (Analysis
                                          code
                                          (emptyAnalysisSpec { originalLanguage = language, expectations = Just [e], autocorrectionRules = Just $ Map.fromList rules }))
spec :: Spec
spec = do
  describe "correct custom usages" $ do
    it "corrects given rules when using MulangSample and originalLanguage" $ do
      let setting = runWithCustomRules (Just Ruby) (MulangSample (Just None)) [("Uses:foo", "UsesPlus")]

      setting (Expectation "*" "UsesPlus")  `shouldBe` (Expectation "*" "UsesPlus")
      setting (Expectation "*" "Uses:foo")  `shouldBe` (Expectation "*" "UsesPlus")

    it "corrects given rules when using MulangSample and no originalLanguage" $ do
      let setting = runWithCustomRules Nothing (MulangSample (Just None)) [("Uses:foo", "UsesPlus")]

      setting (Expectation "*" "UsesPlus")  `shouldBe` (Expectation "*" "UsesPlus")
      setting (Expectation "*" "Uses:foo")  `shouldBe` (Expectation "*" "UsesPlus")

    it "corrects given rules when using CodeSample" $ do
      let setting = runWithCustomRules Nothing (CodeSample JavaScript "x") [("Uses:foo", "UsesPlus")]

      setting (Expectation "*" "UsesPlus")  `shouldBe` (Expectation "*" "UsesPlus")
      setting (Expectation "*" "Uses:foo")  `shouldBe` (Expectation "*" "UsesPlus")

  describe "correct primitive usages" $ do
    it "never corrects plain *" $ do
      let uses = Expectation "*" "Uses:*"

      run Mulang uses `shouldBe` uses
      run Ruby uses `shouldBe` uses
      run JavaScript uses `shouldBe` uses
      run Prolog uses `shouldBe` uses
      run Php uses `shouldBe` uses

    it "always corrects plain * with strict match " $ do
      let strictUses = Expectation "*" "Uses:=*"
      let usesMultiply = Expectation "*" "UsesMultiply"

      run Mulang strictUses `shouldBe` strictUses

      run Ruby strictUses `shouldBe` usesMultiply
      run JavaScript strictUses `shouldBe` usesMultiply
      run Prolog strictUses `shouldBe` usesMultiply
      run Php strictUses `shouldBe` usesMultiply

    it "corrects operators when strict match" $ do
      run Java (Expectation "*" "Uses:=+") `shouldBe` (Expectation "*" "UsesPlus")
      run Haskell (Expectation "*" "Uses:=-") `shouldBe` (Expectation "*" "UsesMinus")
      run JavaScript (Expectation "*" "Uses:====") `shouldBe` (Expectation "*" "UsesEqual")
      run JavaScript (Expectation "*" "Uses:===") `shouldBe` (Expectation "*" "UsesSimilar")

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
      run Java (Expectation "*" "Uses:%") `shouldBe` (Expectation "*" "UsesModulo")
      run Java (Expectation "*" "Uses:&") `shouldBe` (Expectation "*" "UsesBitwiseAnd")
      run Java (Expectation "*" "Uses:|") `shouldBe` (Expectation "*" "UsesBitwiseOr")
      run Java (Expectation "*" "Uses:>>") `shouldBe` (Expectation "*" "UsesBitwiseRightShift")
      run Java (Expectation "*" "Uses:<<") `shouldBe` (Expectation "*" "UsesBitwiseLeftShift")

    it "corrects python and" $ do
      run Python (Expectation "*" "Uses:and") `shouldBe` (Expectation "*" "UsesAnd")
      run Python (Expectation "*"  "Uses:&&") `shouldBe` (Expectation "*" "Uses:&&")

    it "corrects ruby and" $ do
      run Ruby (Expectation "*" "Uses:and") `shouldBe` (Expectation "*" "UsesAnd")
      run Ruby (Expectation "*"  "Uses:&&") `shouldBe` (Expectation "*" "UsesAnd")

    it "corrects ruby size" $ do
      run Ruby (Expectation "*" "Uses:size") `shouldBe` (Expectation "*" "UsesSize")
      run Ruby (Expectation "*"  "Uses:length") `shouldBe` (Expectation "*" "UsesSize")

    it "corrects JS operators" $ do
      run JavaScript (Expectation "*" "Uses:+") `shouldBe` (Expectation "*" "UsesPlus")
      run JavaScript (Expectation "*" "Uses:&&") `shouldBe` (Expectation "*" "UsesAnd")
      run JavaScript (Expectation "*" "Uses:||") `shouldBe` (Expectation "*" "UsesOr")
      run JavaScript (Expectation "*" "Uses:%") `shouldBe` (Expectation "*" "UsesModulo")
      run JavaScript (Expectation "*" "Uses:&") `shouldBe` (Expectation "*" "UsesBitwiseAnd")
      run JavaScript (Expectation "*" "Uses:|") `shouldBe` (Expectation "*" "UsesBitwiseOr")
      run JavaScript (Expectation "*" "Uses:>>") `shouldBe` (Expectation "*" "UsesBitwiseRightShift")
      run JavaScript (Expectation "*" "Uses:<<") `shouldBe` (Expectation "*" "UsesBitwiseLeftShift")
      run JavaScript (Expectation "*" "Uses:length") `shouldBe` (Expectation "*" "UsesSize")

    it "corrects C operators" $ do
      run C (Expectation "*" "Uses:+") `shouldBe` (Expectation "*" "UsesPlus")
      run C (Expectation "*" "Uses:&&") `shouldBe` (Expectation "*" "UsesAnd")
      run C (Expectation "*" "Uses:||") `shouldBe` (Expectation "*" "UsesOr")
      run C (Expectation "*" "Uses:%") `shouldBe` (Expectation "*" "UsesModulo")
      run C (Expectation "*" "Uses:&") `shouldBe` (Expectation "*" "UsesBitwiseAnd")
      run C (Expectation "*" "Uses:|") `shouldBe` (Expectation "*" "UsesBitwiseOr")
      run C (Expectation "*" "Uses:>>") `shouldBe` (Expectation "*" "UsesBitwiseRightShift")
      run C (Expectation "*" "Uses:<<") `shouldBe` (Expectation "*" "UsesBitwiseLeftShift")
      run C (Expectation "*" "Uses:^") `shouldBe` (Expectation "*" "UsesBitwiseXor")

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

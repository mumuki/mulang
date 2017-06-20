module DomainLanguageSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Unfold (mainExpressions)
import           Language.Mulang.DomainLanguage (DomainLanguage(..), hasMisspelledBindings, hasTooShortBindings)
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)
import           Text.Dictionary (toDictionary)
import           Text.Inflections.Tokenizer (camelCase)

spec :: Spec
spec = do
  let english = toDictionary ["a","day","great","is","today"]
  let language = DomainLanguage english camelCase mainExpressions 3

  describe "hasTooShortBindings" $ do
    let run = hasTooShortBindings language

    it "is True when it is a one-char binding" $ do
      run (hs "x = True") `shouldBe` True

    it "is True when it is a two-chars binding" $ do
      run (hs "xy = True") `shouldBe` True

    it "is False when it is a three-chars binding" $ do
      run (hs "zip = []") `shouldBe` False

    it "is False when it contains a short parameter name"  $ do
      run (hs "aFunction a = a") `shouldBe` False

    it "is False when it contains a short local variable name" $ do
      run (js "function foo() { var x = 1; return x }") `shouldBe` False

    it "is False when it uses a short named function" $ do
      run (hs "aFunction aNumber = f aNumber") `shouldBe` False

    it "is False when it contains a short local variable name in a method" $ do
      run (js "var pepita = {come:function(){var x = 1; }, vola:function(){}};") `shouldBe` False

    it "is True when it contains a short named method" $ do
      run (js "var pepita = {x:function(){}, vola:function(){}};") `shouldBe` True

    it "is True when it contains a short named attribute" $ do
      run (js "var pepita = {x: 2, vola:function(){}};") `shouldBe` True

    it "is True when it contains a short variable name" $ do
      run (js "var x = 3;") `shouldBe` True


  describe "hasMisspelledBindings" $ do
    let run = hasMisspelledBindings language

    it "is True when it is a single, well written token" $ do
      run (hs "today = True") `shouldBe` False

    it "is True when all tokens are well-written" $ do
      run (hs "todayIsAGreatDay = True") `shouldBe` False

    it "is True when it is a single, bad written token" $ do
      run (hs "tuday = True") `shouldBe` True

    it "is False when there are typos" $ do
      run (hs "tudayIsAGreatDay = True") `shouldBe` True
      run (hs "todayIsAGraetDay = True") `shouldBe` True

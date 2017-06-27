module UnfoldSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Unfold (mainExpressions)
import           Language.Mulang.Ast
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "mainExpressions" $ do
    it "answers main functions" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2\n\
                     \w k = p\n\
                     \     where z = 2"
      (length $ mainExpressions code) `shouldBe` 2

    it "answers main variables" $ do
      let code = hs "f =  1\ng = 2"
      (length $ mainExpressions code) `shouldBe` 2

    it "answers main objects and methods" $ do
      let code = js "var pepita = {comer: function(x){ this.energia = this.energia + 1; }, volar: function(){console.log('volando')}}"
      (length $ mainExpressions code) `shouldBe` 3

    it "answers main attributes" $ do
      let code = js "var pepita = {energia: 4, posicion: 4,\n\
                    \              comer: function(x){ this.energia = this.energia + 1; },\n\
                    \              volar: function(){console.log('volando')}}"
      (length $ mainExpressions code) `shouldBe` 5

    it "programs" $ do
      let es = [SimpleFunction "main" [] MuNull,
                             EntryPoint (Application (Reference "main") [])]
      let code = Sequence es
      (mainExpressions code) `shouldBe` es


module UnfoldSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Generator (declarations, declaredIdentifiers)
import           Language.Mulang.Ast
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "declarations" $ do
    it "answers functions" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2\n\
                     \w k = p\n\
                     \     where z = 2"
      (length $ declarations code) `shouldBe` 3

    it "answers variables" $ do
      let code = hs "f =  1\ng = 2"
      (length $ declarations code) `shouldBe` 2

    it "answers objects and methods" $ do
      let code = js "var pepita = {comer: function(x){ this.energia = this.energia + 1; }, volar: function(){console.log('volando')}}"
      (length $ declarations code) `shouldBe` 3

    it "answers attributes" $ do
      let code = js "var pepita = {energia: 4, posicion: 4,\n\
                    \              comer: function(x){ this.energia = this.energia + 1; },\n\
                    \              volar: function(){console.log('volando')}}"
      (length $ declarations code) `shouldBe` 5

    it "programs" $ do
      let es = [SimpleFunction "foo" [] None, EntryPoint "main" (Application (Reference "foo") [])]
      let code = Sequence es
      (declarations code) `shouldBe` es
      (declaredIdentifiers code) `shouldBe` ["foo", "main"]

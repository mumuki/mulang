module ExplorerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Explorer
import           Language.Mulang.Ast
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "mainExpressionsOf" $ do
    it "answers main functions" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2\n\
                     \w k = p\n\
                     \     where z = 2"
      (length $ mainExpressionsOf code) `shouldBe` 2

    it "answers main variables" $ do
      let code = hs "f =  1\ng = 2"
      (length $ mainExpressionsOf code) `shouldBe` 2

    it "answers main objects and methods" $ do
      let code = js "var pepita = {comer: function(x){ this.energia = this.energia + 1; }, volar: function(){console.log('volando')}}"
      (length $ mainExpressionsOf code) `shouldBe` 3

    it "answers main attributes" $ do
      let code = js "var pepita = {energia: 4, posicion: 4,\n\
                    \              comer: function(x){ this.energia = this.energia + 1; },\n\
                    \              volar: function(){console.log('volando')}}"
      (length $ mainExpressionsOf code) `shouldBe` 5

    it "programs" $ do
      let mainExpressions = [Function "main" [Equation [] (UnguardedBody MuNull)],
                             EntryPoint (Application (Reference "main") [])]
      let code = Sequence mainExpressions
      (mainExpressionsOf code) `shouldBe` mainExpressions


  describe "declaredBindingsOf" $ do
    it "answers bindings for binding" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2\n\
                     \w k = p\n\
                     \     where z = 2"
      (declaredBindingsOf code) `shouldBe` ["f", "w", "z"]

  describe "referencedBindingsOf" $ do
    it "answers bindings for binding" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2"
      (referencedBindingsOf code) `shouldBe` [".","m","x","y", "g","h"]

  describe "transitiveReferencedBindingsOf" $ do
    it "answers transitive bindings for binding" $ do
      let code = hs "f x = m x\n\
                 \m 0 = p 0\n\
                 \p x = g x"
      (transitiveReferencedBindingsOf "f" code) `shouldBe` ["f","m","x","p","g"]

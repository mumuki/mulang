module ExplorerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Explorer
import           Language.Mulang.Parsers.Haskell

spec :: Spec
spec = do
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
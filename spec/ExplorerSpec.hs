module ExplorerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Unfold (mainExpressions, allExpressions)
import           Language.Mulang.Explorer
import           Language.Mulang.Parsers.Haskell (hs)

spec :: Spec
spec = do
  describe "declaredBindingsOf" $ do
    let code = hs "f x =  (:[]) . m x y . g h 2\n\
                   \w k = p\n\
                   \     where z = 2"
    context "when using all expressions" $ do
      it "answers bindings for binding" $ do
        (declaredBindingsOf allExpressions code) `shouldBe` ["f", "w", "z"]

    context "when using main expressions" $ do
      it "answers bindings for binding" $ do
        (declaredBindingsOf mainExpressions code) `shouldBe` ["f", "w"]


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

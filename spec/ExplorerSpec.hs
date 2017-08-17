module ExplorerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Generator (transitiveReferencedIdentifiersOf, declaredIdentifiersOf, referencedIdentifiersOf)
import           Language.Mulang.Parsers.Haskell (hs)

spec :: Spec
spec = do
  describe "declaredIdentifiersOf" $ do
    let code = hs "f x =  (:[]) . m x y . g h 2\n\
                   \w k = p\n\
                   \     where z = 2"
    it "answers declared identifiers" $ do
      (declaredIdentifiersOf code) `shouldBe` ["f", "w", "z"]

  describe "referencedIdentifiersOf" $ do
    it "answers referenced identifiers" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2"
      (referencedIdentifiersOf code) `shouldBe` [".","m","x","y", "g","h"]

  describe "transitiveReferencedIdentifiersOf" $ do
    it "answers transitive referenced identifiers" $ do
      let code = hs "f x = m x\n\
                 \m 0 = p 0\n\
                 \p x = g x"
      (transitiveReferencedIdentifiersOf "f" code) `shouldBe` ["f","m","x","p","g"]

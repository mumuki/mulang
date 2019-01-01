module GeneratorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Generator (transitiveReferencedIdentifiers, declaredIdentifiers, referencedIdentifiers)
import           Language.Mulang.Parsers.Haskell (hs)

spec :: Spec
spec = do
  describe "declaredIdentifiers" $ do
    let code = hs "f x =  (:[]) . m x y . g h 2\n\
                   \w k = p\n\
                   \     where z = 2"
    it "answers declared identifiers" $ do
      (declaredIdentifiers code) `shouldBe` ["f", "w", "z"]

  describe "referencedIdentifiers" $ do
    it "answers referenced identifiers" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2"
      (referencedIdentifiers code) `shouldBe` [".", "flip", ":", "m","x","y", "g","h"]

  describe "transitiveReferencedIdentifiers" $ do
    it "answers transitive referenced identifiers" $ do
      let code = hs "f x = m x\n\
                 \m 0 = p 0\n\
                 \p x = g x"
      (transitiveReferencedIdentifiers "f" code) `shouldBe` ["f","m","x","p","g"]

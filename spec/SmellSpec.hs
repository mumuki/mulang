module SmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.Haskell (hs)

spec :: Spec
spec = do
  describe "hasRedundantLambda" $ do
    it "is True whn η-conversion applies" $ do
      hasRedundantLambda "x" (hs "x = \\m -> f m") `shouldBe` True

    it "is True whn η-conversion applies, whith composition " $ do
      hasRedundantLambda "x" (hs "x = \\m -> (f.g) m") `shouldBe` True

    it "is False when it is an unavoidable lambda" $ do
      hasRedundantLambda "x" (hs "x = \\m -> m f") `shouldBe` False

  describe "hasRedundantParameter" $ do
    it "is True on trivial application expressions" $ do
      hasRedundantParameter "foo" (hs "foo x = bar x") `shouldBe` True

    it "is True on complex application expressions" $ do
      hasRedundantParameter "foo" (hs "foo x = bar y x") `shouldBe` True

    it "is True on complex application expressions with multiple parameters" $ do
      hasRedundantParameter "foo" (hs "foo z x = bar y x") `shouldBe` True

    it "is False when parameter is not avoidable" $ do
      hasRedundantParameter "foo" (hs "foo x y = foo y x") `shouldBe` False


  describe "hasRedundantIf" $ do
    it "is True when if present and both branches are boolean literals" $ do
      hasRedundantIf "x" (hs "x = if m then True else False") `shouldBe` True
      hasRedundantIf "x" (hs "x = if m then False else True") `shouldBe` True

    it "is False when there is no if" $ do
      hasRedundantIf "x" (hs "x = False") `shouldBe` False

  describe "hasRedundantGuards" $ do
    it "is True when present and both branches are boolean literals" $ do
      hasRedundantGuards "f" (hs "f x | c x = True\n\
                            \    | otherwise = False") `shouldBe` True

    it "is False when present but branches do not answers booleans" $ do
      hasRedundantGuards "f" (hs "f x | c x = 2\n\
                             \    | otherwise = 3") `shouldBe` False

    it "is False when there is no guard" $ do
      hasRedundantGuards "x" (hs "x = False") `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in an if" $ do
      hasRedundantBooleanComparison "x" (hs "x = if m == True then 1 else 2") `shouldBe` True

    it "is True when comparing a literal in an unguarded expression" $ do
      hasRedundantBooleanComparison "f" (hs "f x = x == True") `shouldBe` True

    it "is False on normal comparison" $ do
      hasRedundantBooleanComparison "f" (hs "f x = x == 2") `shouldBe` False

    it "is False when no comparison" $ do
      hasRedundantBooleanComparison "f" (hs "f x = True") `shouldBe` False
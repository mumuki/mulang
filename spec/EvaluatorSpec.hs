module EvaluatorSpec(spec) where

import           Language.Mulang.Cli.Interpreter
import           Language.Mulang.Cli.Code
import           Language.Mulang.Cli.Compiler
import           Test.Hspec

spec = describe "Evaluator" $ do
  describe "Advanced expectations" $ do
    it "evaluates empty expectations" $ do
      evaluate (Input (Code Haskell "x = 2") []) `shouldBe` (Output [] [])

    it "detects smells" $ do
      evaluate (Input (Code Haskell "x = \\y -> f y") []) `shouldBe` (Output [] [(Advanced ["x"] "HasRedundantParameter" Anyone False True)])

    it "evaluates present named expectations" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let xdeclares = (Advanced [] "declares" (Named "x") False False)
      evaluate (Input (Code Haskell "x = 2") [ydeclares, xdeclares]) `shouldBe` (Output [
                                                                          ExpectationResult ydeclares False,
                                                                          ExpectationResult xdeclares True] [])

    it "evaluates present expectations" $ do
      let declaresF = (Advanced [] "declaresFunction" Anyone False False)
      let declaresT = (Advanced [] "declaresTypeAlias" Anyone False False)
      evaluate (Input (Code Haskell "f x = 2") [declaresF, declaresT]) `shouldBe` (Output [
                                                                          ExpectationResult declaresF True,
                                                                          ExpectationResult declaresT False] [])
    it "works with plain mulang format" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let code = Code Mulang "MuNull"
      evaluate (Input code [ydeclares]) `shouldBe` (Output [ExpectationResult ydeclares False] [])

  describe "Basic expectations" $ do
    it "can be negated" $ do
      let notDeclaresx = (Basic "x" "Not:HasBinding")
      let notDeclaresy = (Basic "y" "Not:HasBinding")
      evaluate (Input (Code Haskell "x = 2") [notDeclaresy, notDeclaresx]) `shouldBe` (Output [
                                                                          ExpectationResult notDeclaresy True,
                                                                          ExpectationResult notDeclaresx False] [])

    it "works with HasBinding" $ do
      let xdeclares = (Basic "x" "HasBinding")
      let ydeclares = (Basic "y" "HasBinding")
      evaluate (Input (Code Haskell "x = 2") [ydeclares, xdeclares]) `shouldBe` (Output [
                                                                          ExpectationResult ydeclares False,
                                                                          ExpectationResult xdeclares True] [])
    
    it "works with HasUsage" $ do
      let usesy = (Basic "x" "HasUsage:y")
      let usesz = (Basic "x" "HasUsage:z")
      evaluate (Input (Code Haskell "x = y * 10") [usesy, usesz]) `shouldBe` (Output [
                                                                          ExpectationResult usesy True,
                                                                          ExpectationResult usesz False] [])

    it "works with HasArity" $ do
      let hasArity2 = (Basic "f" "HasArity:2")
      let hasArity3 = (Basic "f" "HasArity:3")
      evaluate (Input (Code Haskell "f x y = y + x") [hasArity2, hasArity3]) `shouldBe` (Output [
                                                                          ExpectationResult hasArity2 True,
                                                                          ExpectationResult hasArity3 False] [])

    it "works with HasTypeSignature" $ do
      let hasTypeSignature = (Basic "f" "HasTypeSignature")
      evaluate (Input (Code Haskell "f :: Int -> Int -> Int \nf x y = y + x") [hasTypeSignature]) `shouldBe` (Output [
                                                                          ExpectationResult hasTypeSignature True] [])

    it "works with HasTypeDeclaration" $ do
      let hasTypeDeclaration = (Basic "Words" "HasTypeDeclaration")
      evaluate (Input (Code Haskell "type Words = [String]") [hasTypeDeclaration]) `shouldBe` (Output [
                                                                          ExpectationResult hasTypeDeclaration True] [])

    it "works with HasIf" $ do
      let hasIf = (Basic "min" "HasIf")
      evaluate (Input (Code Haskell "min x y = if x < y then x else y") [hasIf]) `shouldBe` (Output [ExpectationResult hasIf True] [])

    it "works with HasGuards" $ do
      let hasGuards = (Basic "min" "HasGuards")
      evaluate (Input (Code Haskell "min x y | x < y = x | otherwise = y") [hasGuards]) `shouldBe` (Output [ExpectationResult hasGuards True] [])

    it "works with HasAnonymousVariable" $ do
      let hasAnonymousVariable = (Basic "c" "HasAnonymousVariable")
      evaluate (Input (Code Haskell "c _ = 14") [hasAnonymousVariable]) `shouldBe` (Output [ExpectationResult hasAnonymousVariable True] [])

    it "works with HasRepeat" $ do
      let hasRepeat = (Basic "c" "HasRepeat")
      evaluate (Input (Code Mulang "Repeat (MuNumber 4) (Sequence [])") [hasRepeat]) `shouldBe` (Output [ExpectationResult hasRepeat True] [])
          
    it "works with HasComposition" $ do
      let hasComposition = (Basic "h" "HasComposition")
      evaluate (Input (Code Haskell "h = f . g") [hasComposition]) `shouldBe` (Output [ExpectationResult hasComposition True] [])

    it "works with HasComprehension" $ do
      let hasComprehension = (Basic "x" "HasComprehension")
      evaluate (Input (Code Haskell "x = [m | m <- t]") [hasComprehension]) `shouldBe` (Output [ExpectationResult hasComprehension True] [])
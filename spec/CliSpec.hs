module CliSpec(spec) where

import           Language.Mulang.Cli.Interpreter
import           Language.Mulang.Cli.Code
import           Language.Mulang.Cli.Compiler
import           Test.Hspec

expectationsAnalysysResult expectationsResults smellResults = Output expectationsResults smellResults []

spec = describe "Evaluator" $ do
  describe "Advanced expectations" $ do
    it "evaluates unknown basic expectations" $ do
      let hasTurtle = Basic "x" "HasTurtle"
      evaluate (expectationsSample (Code Haskell "x = 2") [hasTurtle]) `shouldBe` (expectationsAnalysysResult [
                                                                        ExpectationResult hasTurtle True] [])

    it "evaluates unknown basic negated expectations" $ do
      let notHasTurtle = Basic "x" "Not:HasTurtle"
      evaluate (expectationsSample (Code Haskell "x = 2") [notHasTurtle]) `shouldBe` (expectationsAnalysysResult [
                                                                        ExpectationResult notHasTurtle True] [])

    it "evaluates empty expectations" $ do
      evaluate (expectationsSample (Code Haskell "x = 2") []) `shouldBe` (expectationsAnalysysResult [] [])

    it "detects smells" $ do
      evaluate (expectationsSample (Code Haskell "f x = if x then True else False") []) `shouldBe` (expectationsAnalysysResult [] [Basic "f" "HasRedundantIf"])

    it "evaluates present named expectations" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let xdeclares = (Advanced [] "declares" (Named "x") False False)
      evaluate (expectationsSample (Code Haskell "x = 2") [ydeclares, xdeclares]) `shouldBe` (expectationsAnalysysResult [
                                                                          ExpectationResult ydeclares False,
                                                                          ExpectationResult xdeclares True] [])

    it "evaluates present expectations" $ do
      let declaresF = (Advanced [] "declaresFunction" Anyone False False)
      let declaresT = (Advanced [] "declaresTypeAlias" Anyone False False)
      evaluate (expectationsSample (Code Haskell "f x = 2") [declaresF, declaresT]) `shouldBe` (expectationsAnalysysResult [
                                                                          ExpectationResult declaresF True,
                                                                          ExpectationResult declaresT False] [])
    it "works with plain mulang format" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let code = Code Mulang "MuNull"
      evaluate (expectationsSample code [ydeclares]) `shouldBe` (expectationsAnalysysResult [ExpectationResult ydeclares False] [])

  describe "Basic expectations" $ do
    it "can be negated" $ do
      let notDeclaresx = (Basic "x" "Not:HasBinding")
      let notDeclaresy = (Basic "y" "Not:HasBinding")
      evaluate (expectationsSample (Code Haskell "x = \"¡\"") [notDeclaresy, notDeclaresx]) `shouldBe` (expectationsAnalysysResult [
                                                                          ExpectationResult notDeclaresy True,
                                                                          ExpectationResult notDeclaresx False] [])

    it "works with HasBinding" $ do
      let xdeclares = (Basic "x" "HasBinding")
      let ydeclares = (Basic "y" "HasBinding")
      evaluate (expectationsSample (Code Haskell "x = 2") [ydeclares, xdeclares]) `shouldBe` (expectationsAnalysysResult [
                                                                          ExpectationResult ydeclares False,
                                                                          ExpectationResult xdeclares True] [])

    it "works with HasUsage" $ do
      let usesy = (Basic "x" "HasUsage:y")
      let usesz = (Basic "x" "HasUsage:z")
      evaluate (expectationsSample (Code Haskell "x = y * 10") [usesy, usesz]) `shouldBe` (expectationsAnalysysResult [
                                                                          ExpectationResult usesy True,
                                                                          ExpectationResult usesz False] [])

    it "works with HasArity" $ do
      let hasArity2 = (Basic "foo" "HasArity:2")
      let hasArity3 = (Basic "foo" "HasArity:3")
      evaluate (expectationsSample (Code Prolog "foo(x, y).") [hasArity2, hasArity3]) `shouldBe` (expectationsAnalysysResult [
                                                                          ExpectationResult hasArity2 True,
                                                                          ExpectationResult hasArity3 False] [])

    it "works with HasTypeSignature" $ do
      let hasTypeSignature = (Basic "f" "HasTypeSignature")
      evaluate (expectationsSample (Code Haskell "f x y = y + x") [hasTypeSignature]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasTypeSignature False] [])
      evaluate (expectationsSample (Code Haskell "f :: Int -> Int -> Int \nf x y = y + x") [hasTypeSignature]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasTypeSignature True] [])

    it "works with HasTypeDeclaration" $ do
      let hasTypeDeclaration = (Basic "Words" "HasTypeDeclaration")
      evaluate (expectationsSample (Code Haskell "type Works = [String]") [hasTypeDeclaration]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasTypeDeclaration False] [])
      evaluate (expectationsSample (Code Haskell "data Words = Words") [hasTypeDeclaration]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasTypeDeclaration False] [])
      evaluate (expectationsSample (Code Haskell "type Words = [String]") [hasTypeDeclaration]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasTypeDeclaration True] [])

    it "works with HasIf" $ do
      let hasIf = (Basic "min" "HasIf")
      evaluate (expectationsSample (Code Haskell "min x y = True") [hasIf]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasIf False] [])
      evaluate (expectationsSample (Code Haskell "min x y = if x < y then x else y") [hasIf]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasIf True] [])

    it "works with HasGuards" $ do
      let hasGuards = (Basic "min" "HasGuards")
      evaluate (expectationsSample (Code Haskell "min x y = x") [hasGuards]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasGuards False] [])
      evaluate (expectationsSample (Code Haskell "min x y | x < y = x | otherwise = y") [hasGuards]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasGuards True] [])

    it "works with HasAnonymousVariable" $ do
      let hasAnonymousVariable = (Basic "c" "HasAnonymousVariable")
      evaluate (expectationsSample (Code Haskell "c x = 14") [hasAnonymousVariable]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasAnonymousVariable False] [])
      evaluate (expectationsSample (Code Haskell "c _ = 14") [hasAnonymousVariable]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasAnonymousVariable True] [])

    it "works with HasRepeat" $ do
      pendingWith "Should be implemented when Gobstones support is ready"

    it "works with HasComposition" $ do
      let hasComposition = (Basic "h" "HasComposition")
      evaluate (expectationsSample (Code Haskell "h = f") [hasComposition]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasComposition False] [])
      evaluate (expectationsSample (Code Haskell "h = f . g") [hasComposition]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasComposition True] [])

    it "works with HasComprehension" $ do
      let hasComprehension = (Basic "x" "HasComprehension")
      evaluate (expectationsSample (Code Haskell "x = [m | m <- t]") [hasComprehension]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasComprehension True] [])

    it "works with HasConditional" $ do
      let hasConditional = (Basic "min" "HasConditional")
      evaluate (expectationsSample (Code JavaScript "function min(x, y) { if (x < y) { return x } else { return y } }") [hasConditional]) `shouldBe` (expectationsAnalysysResult [
                                                                                                            ExpectationResult hasConditional True] [])

    it "works with HasWhile" $ do
      let hasWhile = (Basic "f" "HasWhile")
      evaluate (expectationsSample (Code JavaScript "function f() { var x = 5; while (x < 10) { x++ } }") [hasWhile]) `shouldBe` (expectationsAnalysysResult [
                                                                                                            ExpectationResult hasWhile True] [])

    it "works with HasForall" $ do
      let hasForall = (Basic "f" "HasForall")
      evaluate (expectationsSample (Code Prolog "f(X) :- isElement(Usuario), forall(isRelated(X, Y), complies(Y)).") [hasForall]) `shouldBe` (expectationsAnalysysResult [
                                                                                                            ExpectationResult hasForall True] [])

    it "works with HasFindall" $ do
      let hasFindall = (Basic "baz" "HasFindall")
      evaluate (expectationsSample (Code Prolog "baz(X):- bar(X, Y).") [hasFindall]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasFindall False] [])
      evaluate (expectationsSample (Code Prolog "baz(X):- findall(Y, bar(X, Y), Z).") [hasFindall]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasFindall True] [])

    it "works with HasLambda" $ do
      let hasLambda = (Basic "f" "HasLambda")
      evaluate (expectationsSample (Code Haskell "f = map id") [hasLambda]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasLambda False] [])
      evaluate (expectationsSample (Code Haskell "f = map $ \\x -> x + 1") [hasLambda]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasLambda True] [])

    it "works with HasDirectRecursion" $ do
      let hasDirectRecursion = (Basic "f" "HasDirectRecursion")
      evaluate (expectationsSample (Code Haskell "f x = if x < 5 then g (x - 1) else 2") [hasDirectRecursion]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasDirectRecursion False] [])
      evaluate (expectationsSample (Code Haskell "f x = if x < 5 then f (x - 1) else 2") [hasDirectRecursion]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasDirectRecursion True] [])

    it "works with HasNot" $ do
      let hasNot = (Basic "foo" "HasNot")
      evaluate (expectationsSample (Code Prolog "foo(X) :- bar(X).") [hasNot]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasNot False] [])
      evaluate (expectationsSample (Code Prolog "foo(X) :- not(bar(X)).") [hasNot]) `shouldBe` (expectationsAnalysysResult [ExpectationResult hasNot True] [])
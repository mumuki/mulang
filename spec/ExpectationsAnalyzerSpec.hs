module ExpectationsAnalyzerSpec(spec) where

import           Language.Mulang
import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result expectationsResults smellResults = AnalysisCompleted expectationsResults smellResults []

run language content expectations = analyse (expectationsAnalysis (CodeSample language content) expectations)
runAst ast expectations = analyse (expectationsAnalysis (MulangSample ast) expectations)

passed e = ExpectationResult e True
failed e = ExpectationResult e False

spec = describe "ExpectationsAnalyzer" $ do
  it "works with Mulang input" $ do
    let ydeclares = Expectation "" "HasBinding:y"
    (runAst MuNull [ydeclares]) `shouldReturn` (result [failed ydeclares] [])

  describe "Advanced expectations" $ do
    it "evaluates unknown basic expectations" $ do
      let hasTurtle = Expectation "x" "HasTurtle"
      (run Haskell "x = 2" [hasTurtle]) `shouldReturn` (result [passed hasTurtle] [])

    it "evaluates unknown basic negated expectations" $ do
      let notHasTurtle = Expectation "x" "Not:HasTurtle"
      (run Haskell "x = 2" [notHasTurtle]) `shouldReturn` (result [passed notHasTurtle] [])

    it "evaluates empty expectations" $ do
      (run Haskell "x = 2" []) `shouldReturn` (result [] [])

    it "evaluates present named expectations" $ do
      let ydeclares = Expectation "" "HasBinding:y"
      let xdeclares = Expectation "" "HasBinding:x"
      (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

    it "evaluates present expectations" $ do
      let declaresF = Expectation "" "HasFunction"
      let declaresT = Expectation "" "HasTypeAlias"
      (run Haskell "f x = 2" [declaresF, declaresT]) `shouldReturn` (result [passed declaresF, failed declaresT] [])

  describe "Basic expectations" $ do
    it "can be negated" $ do
      let notDeclaresX = Expectation "x" "Not:HasBinding"
      let notDeclaresY = Expectation "y" "Not:HasBinding"
      (run Haskell "x = \"¡\"" [notDeclaresY, notDeclaresX]) `shouldReturn` (result [
                                                                          passed notDeclaresY, failed notDeclaresX] [])

    it "works with HasBinding" $ do
      let xdeclares = Expectation "x" "HasBinding"
      let ydeclares = Expectation "y" "HasBinding"
      (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

    it "works with HasUsage" $ do
      let usesy = Expectation "x" "HasUsage:y"
      let usesz = Expectation "x" "HasUsage:z"
      (run Haskell "x = y * 10" [usesy, usesz]) `shouldReturn` (result [passed usesy, failed usesz] [])

    it "works with HasArity" $ do
      let hasArity2 = Expectation "foo" "HasArity2"
      let hasArity3 = Expectation "foo" "HasArity3"
      (run Prolog "foo(x, y)." [hasArity2, hasArity3]) `shouldReturn` (result [passed hasArity2, failed hasArity3] [])

    it "works with HasTypeSignature" $ do
      let hasTypeSignature = Expectation "f" "HasTypeSignature"
      (run Haskell "f x y = y + x" [hasTypeSignature]) `shouldReturn` (result [failed hasTypeSignature] [])
      (run Haskell "f :: Int -> Int -> Int \nf x y = y + x" [hasTypeSignature]) `shouldReturn` (result [passed hasTypeSignature] [])

    it "works with HasTypeAlias" $ do
      let hasTypeAlias = Expectation "Words" "HasTypeAlias"
      (run Haskell "type Works = [String]" [hasTypeAlias]) `shouldReturn` (result [failed hasTypeAlias] [])
      (run Haskell "data Words = Words" [hasTypeAlias]) `shouldReturn` (result [failed hasTypeAlias] [])
      (run Haskell "type Words = [String]" [hasTypeAlias]) `shouldReturn` (result [passed hasTypeAlias] [])

    it "works with HasIf" $ do
      let hasIf = Expectation "min" "HasIf"
      (run Haskell "min x y = True" [hasIf]) `shouldReturn` (result [failed hasIf] [])
      (run Haskell "min x y = if x < y then x else y" [hasIf]) `shouldReturn` (result [passed hasIf] [])

    it "works with HasGuards" $ do
      let hasGuards = Expectation "min" "HasGuards"
      (run Haskell "min x y = x" [hasGuards]) `shouldReturn` (result [failed hasGuards] [])
      (run Haskell "min x y | x < y = x | otherwise = y" [hasGuards]) `shouldReturn` (result [passed hasGuards] [])

    it "works with HasAnonymousVariable" $ do
      let hasAnonymousVariable = Expectation "c" "HasAnonymousVariable"
      (run Haskell "c x = 14" [hasAnonymousVariable]) `shouldReturn` (result [failed hasAnonymousVariable] [])
      (run Haskell "c _ = 14" [hasAnonymousVariable]) `shouldReturn` (result [passed hasAnonymousVariable] [])

    it "works with HasRepeat" $ do
      pendingWith "Should be implemented when Gobstones support is ready"

    it "works with HasComposition" $ do
      let hasComposition = Expectation "h" "HasComposition"
      (run Haskell "h = f" [hasComposition]) `shouldReturn` (result [failed hasComposition] [])
      (run Haskell "h = f . g" [hasComposition]) `shouldReturn` (result [passed hasComposition] [])

    it "works with HasComprehension" $ do
      let hasComprehension = Expectation "x" "HasComprehension"
      (run Haskell "x = [m | m <- t]" [hasComprehension]) `shouldReturn` (result [passed hasComprehension] [])

    it "works with HasConditional" $ do
      let hasConditional = Expectation "min" "HasConditional"
      (run JavaScript "function min(x, y) { if (x < y) { return x } else { return y } }" [hasConditional]) `shouldReturn` (result [
                                                                                                            passed hasConditional] [])

    it "works with HasWhile" $ do
      let hasWhile = Expectation "f" "HasWhile"
      (run JavaScript "function f() { var x = 5; while (x < 10) { x++ } }" [hasWhile]) `shouldReturn` (result [passed hasWhile] [])

    it "works with HasForall" $ do
      let hasForall = Expectation "f" "HasForall"
      (run Prolog "f(X) :- isElement(Usuario), forall(isRelated(X, Y), complies(Y))." [hasForall]) `shouldReturn` (result [
                                                                                                            passed hasForall] [])

    it "works with HasFindall" $ do
      let hasFindall = Expectation "baz" "HasFindall"
      (run Prolog "baz(X):- bar(X, Y)." [hasFindall]) `shouldReturn` (result [failed hasFindall] [])
      (run Prolog "baz(X):- findall(Y, bar(X, Y), Z)." [hasFindall]) `shouldReturn` (result [passed hasFindall] [])

    it "works with HasLambda" $ do
      let hasLambda = Expectation "f" "HasLambda"
      (run Haskell "f = map id" [hasLambda]) `shouldReturn` (result [failed hasLambda] [])
      (run Haskell "f = map $ \\x -> x + 1" [hasLambda]) `shouldReturn` (result [passed hasLambda] [])

    it "works with HasDirectRecursion" $ do
      let hasDirectRecursion = Expectation "f" "HasDirectRecursion"
      (run Haskell "f x = if x < 5 then g (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [failed hasDirectRecursion] [])
      (run Haskell "f x = if x < 5 then f (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [passed hasDirectRecursion] [])

    it "works with HasNot" $ do
      let hasNot = Expectation "foo" "HasNot"
      (run Prolog "foo(X) :- bar(X)." [hasNot]) `shouldReturn` (result [failed hasNot] [])
      (run Prolog "foo(X) :- not(bar(X))." [hasNot]) `shouldReturn` (result [passed hasNot] [])

    it "proerly reports parsing errors" $ do
      let hasNot = Expectation "foo" "HasNot"
      (run Haskell " foo " [hasNot]) `shouldReturn` (AnalysisFailed "Sample code parsing error")

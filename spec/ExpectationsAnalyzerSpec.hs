module ExpectationsAnalyzerSpec(spec) where

import           Language.Mulang
import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result expectationsResults smellResults = AnalysisCompleted expectationsResults smellResults [] noDomainLanguageViolations

run language content expectations = analyse (expectationsAnalysis (CodeSample language content) expectations)
runAst ast expectations = analyse (expectationsAnalysis (MulangSample ast) expectations)

passed e = ExpectationResult e True
failed e = ExpectationResult e False

spec = describe "ExpectationsAnalyzer" $ do
  it "works with Mulang input" $ do
    let ydeclares = (Advanced [] "declares" (Named "y") False False)
    (runAst MuNull [ydeclares]) `shouldReturn` (result [failed ydeclares] [])

  describe "Advanced expectations" $ do
    it "evaluates unknown basic expectations" $ do
      let hasTurtle = Basic "x" "HasTurtle"
      (run Haskell "x = 2" [hasTurtle]) `shouldReturn` (result [passed hasTurtle] [])

    it "evaluates unknown basic negated expectations" $ do
      let notHasTurtle = Basic "x" "Not:HasTurtle"
      (run Haskell "x = 2" [notHasTurtle]) `shouldReturn` (result [passed notHasTurtle] [])

    it "evaluates empty expectations" $ do
      (run Haskell "x = 2" []) `shouldReturn` (result [] [])

    it "evaluates present named expectations" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let xdeclares = (Advanced [] "declares" (Named "x") False False)
      (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

    it "evaluates present expectations" $ do
      let declaresF = (Advanced [] "declaresFunction" Anyone False False)
      let declaresT = (Advanced [] "declaresTypeAlias" Anyone False False)
      (run Haskell "f x = 2" [declaresF, declaresT]) `shouldReturn` (result [passed declaresF, failed declaresT] [])

  describe "Basic expectations" $ do
    it "can be negated" $ do
      let notDeclaresx = (Basic "x" "Not:HasBinding")
      let notDeclaresy = (Basic "y" "Not:HasBinding")
      (run Haskell "x = \"¡\"" [notDeclaresy, notDeclaresx]) `shouldReturn` (result [
                                                                          passed notDeclaresy, failed notDeclaresx] [])

    it "works with HasBinding" $ do
      let xdeclares = (Basic "x" "HasBinding")
      let ydeclares = (Basic "y" "HasBinding")
      (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

    it "works with HasUsage" $ do
      let usesy = (Basic "x" "HasUsage:y")
      let usesz = (Basic "x" "HasUsage:z")
      (run Haskell "x = y * 10" [usesy, usesz]) `shouldReturn` (result [passed usesy, failed usesz] [])

    it "works with HasArity" $ do
      let hasArity2 = (Basic "foo" "HasArity:2")
      let hasArity3 = (Basic "foo" "HasArity:3")
      (run Prolog "foo(x, y)." [hasArity2, hasArity3]) `shouldReturn` (result [passed hasArity2, failed hasArity3] [])

    it "works with HasTypeSignature" $ do
      let hasTypeSignature = (Basic "f" "HasTypeSignature")
      (run Haskell "f x y = y + x" [hasTypeSignature]) `shouldReturn` (result [failed hasTypeSignature] [])
      (run Haskell "f :: Int -> Int -> Int \nf x y = y + x" [hasTypeSignature]) `shouldReturn` (result [passed hasTypeSignature] [])

    it "works with HasTypeDeclaration" $ do
      let hasTypeDeclaration = (Basic "Words" "HasTypeDeclaration")
      (run Haskell "type Works = [String]" [hasTypeDeclaration]) `shouldReturn` (result [failed hasTypeDeclaration] [])
      (run Haskell "data Words = Words" [hasTypeDeclaration]) `shouldReturn` (result [failed hasTypeDeclaration] [])
      (run Haskell "type Words = [String]" [hasTypeDeclaration]) `shouldReturn` (result [passed hasTypeDeclaration] [])

    it "works with HasIf" $ do
      let hasIf = (Basic "min" "HasIf")
      (run Haskell "min x y = True" [hasIf]) `shouldReturn` (result [failed hasIf] [])
      (run Haskell "min x y = if x < y then x else y" [hasIf]) `shouldReturn` (result [passed hasIf] [])

    it "works with HasGuards" $ do
      let hasGuards = (Basic "min" "HasGuards")
      (run Haskell "min x y = x" [hasGuards]) `shouldReturn` (result [failed hasGuards] [])
      (run Haskell "min x y | x < y = x | otherwise = y" [hasGuards]) `shouldReturn` (result [passed hasGuards] [])

    it "works with HasAnonymousVariable" $ do
      let hasAnonymousVariable = (Basic "c" "HasAnonymousVariable")
      (run Haskell "c x = 14" [hasAnonymousVariable]) `shouldReturn` (result [failed hasAnonymousVariable] [])
      (run Haskell "c _ = 14" [hasAnonymousVariable]) `shouldReturn` (result [passed hasAnonymousVariable] [])

    it "works with HasRepeat" $ do
      pendingWith "Should be implemented when Gobstones support is ready"

    it "works with HasComposition" $ do
      let hasComposition = (Basic "h" "HasComposition")
      (run Haskell "h = f" [hasComposition]) `shouldReturn` (result [failed hasComposition] [])
      (run Haskell "h = f . g" [hasComposition]) `shouldReturn` (result [passed hasComposition] [])

    it "works with HasComprehension" $ do
      let hasComprehension = (Basic "x" "HasComprehension")
      (run Haskell "x = [m | m <- t]" [hasComprehension]) `shouldReturn` (result [passed hasComprehension] [])

    it "works with HasConditional" $ do
      let hasConditional = (Basic "min" "HasConditional")
      (run JavaScript "function min(x, y) { if (x < y) { return x } else { return y } }" [hasConditional]) `shouldReturn` (result [
                                                                                                            passed hasConditional] [])

    it "works with HasWhile" $ do
      let hasWhile = (Basic "f" "HasWhile")
      (run JavaScript "function f() { var x = 5; while (x < 10) { x++ } }" [hasWhile]) `shouldReturn` (result [passed hasWhile] [])

    it "works with HasForall" $ do
      let hasForall = (Basic "f" "HasForall")
      (run Prolog "f(X) :- isElement(Usuario), forall(isRelated(X, Y), complies(Y))." [hasForall]) `shouldReturn` (result [
                                                                                                            passed hasForall] [])

    it "works with HasFindall" $ do
      let hasFindall = (Basic "baz" "HasFindall")
      (run Prolog "baz(X):- bar(X, Y)." [hasFindall]) `shouldReturn` (result [failed hasFindall] [])
      (run Prolog "baz(X):- findall(Y, bar(X, Y), Z)." [hasFindall]) `shouldReturn` (result [passed hasFindall] [])

    it "works with HasLambda" $ do
      let hasLambda = (Basic "f" "HasLambda")
      (run Haskell "f = map id" [hasLambda]) `shouldReturn` (result [failed hasLambda] [])
      (run Haskell "f = map $ \\x -> x + 1" [hasLambda]) `shouldReturn` (result [passed hasLambda] [])

    it "works with HasDirectRecursion" $ do
      let hasDirectRecursion = (Basic "f" "HasDirectRecursion")
      (run Haskell "f x = if x < 5 then g (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [failed hasDirectRecursion] [])
      (run Haskell "f x = if x < 5 then f (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [passed hasDirectRecursion] [])

    it "works with HasNot" $ do
      let hasNot = (Basic "foo" "HasNot")
      (run Prolog "foo(X) :- bar(X)." [hasNot]) `shouldReturn` (result [failed hasNot] [])
      (run Prolog "foo(X) :- not(bar(X))." [hasNot]) `shouldReturn` (result [passed hasNot] [])

    it "proerly reports parsing errors" $ do
      let hasNot = (Basic "foo" "HasNot")
      (run Haskell " foo " [hasNot]) `shouldReturn` (AnalysisFailed "Sample code parsing error")

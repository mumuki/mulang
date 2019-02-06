module ExpectationsAnalyzerSpec(spec) where

import           Language.Mulang
import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result expectationResults smells
  = emptyCompletedAnalysisResult { expectationResults = expectationResults, smells = smells }

run language content expectations = analyse (expectationsAnalysis (CodeSample language content) expectations)
runAst ast expectations = analyse (expectationsAnalysis (MulangSample ast Nothing) expectations)

passed e = ExpectationResult e True
failed e = ExpectationResult e False

spec = describe "ExpectationsAnalyzer" $ do
  it "works with Mulang input" $ do
    let ydeclares = Ringed "*" "Declares:y"
    (runAst None [ydeclares]) `shouldReturn` (result [failed ydeclares] [])

  describe "Advanced expectations" $ do
    it "evaluates unknown basic expectations" $ do
      let hasTurtle = Ringed "x" "HasTurtle"
      (run Haskell "x = 2" [hasTurtle]) `shouldReturn` (result [passed hasTurtle] [])

    it "evaluates unknown basic negated expectations" $ do
      let notHasTurtle = Ringed "x" "Not:HasTurtle"
      (run Haskell "x = 2" [notHasTurtle]) `shouldReturn` (result [passed notHasTurtle] [])

    it "evaluates empty expectations" $ do
      (run Haskell "x = 2" []) `shouldReturn` (result [] [])

    it "evaluates present named expectations" $ do
      let ydeclares = Ringed "*" "Declares:y"
      let xdeclares = Ringed "*" "Declares:x"
      (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

    it "evaluates present expectations" $ do
      let declaresF = Ringed "*" "DeclaresFunction"
      let declaresT = Ringed "*" "DeclaresTypeAlias"
      (run Haskell "f x = 2" [declaresF, declaresT]) `shouldReturn` (result [passed declaresF, failed declaresT] [])

  describe "Basic expectations" $ do
    it "can be negated" $ do
      let notDeclaresX = Ringed "*" "Not:Declares:x"
      let notDeclaresY = Ringed "*" "Not:Declares:y"
      (run Haskell "x = \"¡\"" [notDeclaresY, notDeclaresX]) `shouldReturn` (result [
                                                                          passed notDeclaresY, failed notDeclaresX] [])
    it "works with Declares" $ do
      let xdeclares = Ringed "*" "Declares:x"
      let ydeclares = Ringed "*" "Declares:y"
      (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

    it "works with Uses" $ do
      let usesy = Ringed "x" "Uses:y"
      let usesz = Ringed "x" "Uses:z"
      (run Haskell "x = y * 10" [usesy, usesz]) `shouldReturn` (result [passed usesy, failed usesz] [])

    it "works with DeclaresComputationWithArity" $ do
      let hasArity2 = Ringed "*" "DeclaresComputationWithArity2:foo"
      let hasArity3 = Ringed "*" "DeclaresComputationWithArity3:foo"
      (run Prolog "foo(x, y)." [hasArity2, hasArity3]) `shouldReturn` (result [passed hasArity2, failed hasArity3] [])

    it "works with DeclaresTypeSignature" $ do
      let declaresTypeSignature = Ringed "*" "DeclaresTypeSignature:f"
      (run Haskell "f x y = y + x" [declaresTypeSignature]) `shouldReturn` (result [failed declaresTypeSignature] [])
      (run Haskell "f :: Int -> Int -> Int \nf x y = y + x" [declaresTypeSignature]) `shouldReturn` (result [passed declaresTypeSignature] [])

    it "works with DeclaresTypeAlias" $ do
      let hasTypeAlias = Ringed "*" "DeclaresTypeAlias:Words"
      (run Haskell "type Works = [String]" [hasTypeAlias]) `shouldReturn` (result [failed hasTypeAlias] [])
      (run Haskell "data Words = Words" [hasTypeAlias]) `shouldReturn` (result [failed hasTypeAlias] [])
      (run Haskell "type Words = [String]" [hasTypeAlias]) `shouldReturn` (result [passed hasTypeAlias] [])

    it "works with UsesIf" $ do
      let hasIf = Ringed "min" "UsesIf"
      (run Haskell "min x y = True" [hasIf]) `shouldReturn` (result [failed hasIf] [])
      (run Haskell "min x y = if x < y then x else y" [hasIf]) `shouldReturn` (result [passed hasIf] [])

    it "works with UsesGuards" $ do
      let hasGuards = Ringed "min" "UsesGuards"
      (run Haskell "min x y = x" [hasGuards]) `shouldReturn` (result [failed hasGuards] [])
      (run Haskell "min x y | x < y = x | otherwise = y" [hasGuards]) `shouldReturn` (result [passed hasGuards] [])

    it "works with UsesAnonymousVariable" $ do
      let hasAnonymousVariable = Ringed "c" "UsesAnonymousVariable"
      (run Haskell "c x = 14" [hasAnonymousVariable]) `shouldReturn` (result [failed hasAnonymousVariable] [])
      (run Haskell "c _ = 14" [hasAnonymousVariable]) `shouldReturn` (result [passed hasAnonymousVariable] [])

    it "works with UsesComposition" $ do
      let hasComposition = Ringed "h" "UsesComposition"
      (run Haskell "h = f" [hasComposition]) `shouldReturn` (result [failed hasComposition] [])
      (run Haskell "h = f . g" [hasComposition]) `shouldReturn` (result [passed hasComposition] [])

    it "works with UsesForComprehension" $ do
      let hasComprehension = Ringed "x" "UsesForComprehension"
      (run Haskell "x = [m | m <- t]" [hasComprehension]) `shouldReturn` (result [passed hasComprehension] [])

    it "works with UsesForLoop" $ do
      let hasForLoop = Ringed "f" "UsesForLoop"
      (run JavaScript "function f() { var x; for (x = 0; x < 10; x++) { x; } }" [hasForLoop]) `shouldReturn` (result [passed hasForLoop] [])

    it "works with UsesConditional" $ do
      let hasConditional = Ringed "min" "UsesConditional"
      (run JavaScript "function min(x, y) { if (x < y) { return x } else { return y } }" [hasConditional]) `shouldReturn` (result [
                                                                                                            passed hasConditional] [])

    it "works with UsesWhile" $ do
      let hasWhile = Ringed "f" "UsesWhile"
      (run JavaScript "function f() { var x = 5; while (x < 10) { x++ } }" [hasWhile]) `shouldReturn` (result [passed hasWhile] [])

    it "works with HasForall" $ do
      let hasForall = Ringed "f" "HasForall"
      (run Prolog "f(X) :- isElement(Usuario), forall(isRelated(X, Y), complies(Y))." [hasForall]) `shouldReturn` (result [
                                                                                                            passed hasForall] [])

    it "works with UsesFindall" $ do
      let hasFindall = Ringed "baz" "UsesFindall"
      (run Prolog "baz(X):- bar(X, Y)." [hasFindall]) `shouldReturn` (result [failed hasFindall] [])
      (run Prolog "baz(X):- findall(Y, bar(X, Y), Z)." [hasFindall]) `shouldReturn` (result [passed hasFindall] [])

    it "works with UsesLambda" $ do
      let hasLambda = Ringed "f" "UsesLambda"
      (run Haskell "f = map id" [hasLambda]) `shouldReturn` (result [failed hasLambda] [])
      (run Haskell "f = map $ \\x -> x + 1" [hasLambda]) `shouldReturn` (result [passed hasLambda] [])

    it "works with DeclaresRecursively" $ do
      let hasDirectRecursion = Ringed "*" "DeclaresRecursively:f"
      (run Haskell "f x = if x < 5 then g (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [failed hasDirectRecursion] [])
      (run Haskell "f x = if x < 5 then f (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [passed hasDirectRecursion] [])

    it "works with UsesNot" $ do
      let hasNot = Ringed "foo" "UsesNot"
      (run Prolog "foo(X) :- bar(X)." [hasNot]) `shouldReturn` (result [failed hasNot] [])
      (run Prolog "foo(X) :- not(bar(X))." [hasNot]) `shouldReturn` (result [passed hasNot] [])

    it "properly reports parsing errors" $ do
      let hasNot = Ringed "foo" "UsesNot"
      (run Haskell " foo " [hasNot]) `shouldReturn` (AnalysisFailed "Parse error")

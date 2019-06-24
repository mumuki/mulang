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
    let ydeclares = Expectation "*" "Declares:y"
    (runAst None [ydeclares]) `shouldReturn` (result [failed ydeclares] [])

  it "evaluates unknown basic expectations" $ do
    let hasTurtle = Expectation "x" "HasTurtle"
    (run Haskell "x = 2" [hasTurtle]) `shouldReturn` (result [passed hasTurtle] [])

  it "evaluates unknown basic negated expectations" $ do
    let notHasTurtle = Expectation "x" "Not:HasTurtle"
    (run Haskell "x = 2" [notHasTurtle]) `shouldReturn` (result [passed notHasTurtle] [])

  it "evaluates empty expectations" $ do
    (run Haskell "x = 2" []) `shouldReturn` (result [] [])

  it "evaluates present named expectations" $ do
    let ydeclares = Expectation "*" "Declares:y"
    let xdeclares = Expectation "*" "Declares:x"
    (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

  it "evaluates present expectations" $ do
    let declaresF = Expectation "*" "DeclaresFunction"
    let declaresT = Expectation "*" "DeclaresTypeAlias"
    (run Haskell "f x = 2" [declaresF, declaresT]) `shouldReturn` (result [passed declaresF, failed declaresT] [])

  it "can be negated" $ do
    let notDeclaresX = Expectation "*" "Not:Declares:x"
    let notDeclaresY = Expectation "*" "Not:Declares:y"
    (run Haskell "x = \"¡\"" [notDeclaresY, notDeclaresX]) `shouldReturn` (result [
                                                                        passed notDeclaresY, failed notDeclaresX] [])
  it "works with Declares" $ do
    let xdeclares = Expectation "*" "Declares:x"
    let ydeclares = Expectation "*" "Declares:y"
    (run Haskell "x = 2" [ydeclares, xdeclares]) `shouldReturn` (result [failed ydeclares, passed xdeclares] [])

  it "works with Uses" $ do
    let usesy = Expectation "x" "Uses:y"
    let usesz = Expectation "x" "Uses:z"
    (run Haskell "x = y * 10" [usesy, usesz]) `shouldReturn` (result [passed usesy, failed usesz] [])

  it "works with DeclaresComputationWithArity" $ do
    let hasArity2 = Expectation "*" "DeclaresComputationWithArity2:foo"
    let hasArity3 = Expectation "*" "DeclaresComputationWithArity3:foo"
    (run Prolog "foo(x, y)." [hasArity2, hasArity3]) `shouldReturn` (result [passed hasArity2, failed hasArity3] [])

  it "works with DeclaresTypeSignature" $ do
    let declaresTypeSignature = Expectation "*" "DeclaresTypeSignature:f"
    (run Haskell "f x y = y + x" [declaresTypeSignature]) `shouldReturn` (result [failed declaresTypeSignature] [])
    (run Haskell "f :: Int -> Int -> Int \nf x y = y + x" [declaresTypeSignature]) `shouldReturn` (result [passed declaresTypeSignature] [])

  it "works with DeclaresTypeAlias" $ do
    let hasTypeAlias = Expectation "*" "DeclaresTypeAlias:Words"
    (run Haskell "type Works = [String]" [hasTypeAlias]) `shouldReturn` (result [failed hasTypeAlias] [])
    (run Haskell "data Words = Words" [hasTypeAlias]) `shouldReturn` (result [failed hasTypeAlias] [])
    (run Haskell "type Words = [String]" [hasTypeAlias]) `shouldReturn` (result [passed hasTypeAlias] [])

  it "works with UsesIf" $ do
    let hasIf = Expectation "min" "UsesIf"
    (run Haskell "min x y = True" [hasIf]) `shouldReturn` (result [failed hasIf] [])
    (run Haskell "min x y = if x < y then x else y" [hasIf]) `shouldReturn` (result [passed hasIf] [])

  it "works with UsesGuards" $ do
    let hasGuards = Expectation "min" "UsesGuards"
    (run Haskell "min x y = x" [hasGuards]) `shouldReturn` (result [failed hasGuards] [])
    (run Haskell "min x y | x < y = x | otherwise = y" [hasGuards]) `shouldReturn` (result [passed hasGuards] [])

  it "works with UsesAnonymousVariable" $ do
    let hasAnonymousVariable = Expectation "c" "UsesAnonymousVariable"
    (run Haskell "c x = 14" [hasAnonymousVariable]) `shouldReturn` (result [failed hasAnonymousVariable] [])
    (run Haskell "c _ = 14" [hasAnonymousVariable]) `shouldReturn` (result [passed hasAnonymousVariable] [])

  it "works with UsesComposition" $ do
    let hasComposition = Expectation "h" "UsesComposition"
    (run Haskell "h = f" [hasComposition]) `shouldReturn` (result [failed hasComposition] [])
    (run Haskell "h = f . g" [hasComposition]) `shouldReturn` (result [passed hasComposition] [])

  it "works with UsesForComprehension" $ do
    let hasComprehension = Expectation "x" "UsesForComprehension"
    (run Haskell "x = [m | m <- t]" [hasComprehension]) `shouldReturn` (result [passed hasComprehension] [])

  it "works with UsesForLoop" $ do
    let hasForLoop = Expectation "f" "UsesForLoop"
    (run JavaScript "function f() { var x; for (x = 0; x < 10; x++) { x; } }" [hasForLoop]) `shouldReturn` (result [passed hasForLoop] [])

  it "works with UsesConditional" $ do
    let hasConditional = Expectation "min" "UsesConditional"
    (run JavaScript "function min(x, y) { if (x < y) { return x } else { return y } }" [hasConditional]) `shouldReturn` (result [
                                                                                                          passed hasConditional] [])

  it "works with UsesWhile" $ do
    let hasWhile = Expectation "f" "UsesWhile"
    (run JavaScript "function f() { var x = 5; while (x < 10) { x++ } }" [hasWhile]) `shouldReturn` (result [passed hasWhile] [])

  it "works with UsesForall" $ do
    let hasForall = Expectation "f" "UsesForall"
    (run Prolog "f(X) :- isElement(Usuario), forall(isRelated(X, Y), complies(Y))." [hasForall]) `shouldReturn` (result [
                                                                                                          passed hasForall] [])

  it "works with UsesFindall" $ do
    let hasFindall = Expectation "baz" "UsesFindall"
    (run Prolog "baz(X):- bar(X, Y)." [hasFindall]) `shouldReturn` (result [failed hasFindall] [])
    (run Prolog "baz(X):- findall(Y, bar(X, Y), Z)." [hasFindall]) `shouldReturn` (result [passed hasFindall] [])

  it "works with UsesLambda" $ do
    let hasLambda = Expectation "f" "UsesLambda"
    (run Haskell "f = map id" [hasLambda]) `shouldReturn` (result [failed hasLambda] [])
    (run Haskell "f = map $ \\x -> x + 1" [hasLambda]) `shouldReturn` (result [passed hasLambda] [])

  it "works with DeclaresRecursively" $ do
    let hasDirectRecursion = Expectation "*" "DeclaresRecursively:f"
    (run Haskell "f x = if x < 5 then g (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [failed hasDirectRecursion] [])
    (run Haskell "f x = if x < 5 then f (x - 1) else 2" [hasDirectRecursion]) `shouldReturn` (result [passed hasDirectRecursion] [])

  it "works with UsesNot" $ do
    let hasNot = Expectation "foo" "UsesNot"
    (run Prolog "foo(X) :- bar(X)." [hasNot]) `shouldReturn` (result [failed hasNot] [])
    (run Prolog "foo(X) :- not(bar(X))." [hasNot]) `shouldReturn` (result [passed hasNot] [])

  it "properly reports parsing errors" $ do
    let hasNot = Expectation "foo" "UsesNot"
    (run Haskell " foo " [hasNot]) `shouldReturn` (AnalysisFailed "Parse error")

  it "works with keyword-based expectation synthesis of declares" $ do
    let usesType = Expectation "*" "Uses:type"
    let declaresTypeAlias = Expectation "*" "DeclaresTypeAlias"
    run Haskell "type X = Int" [usesType] `shouldReturn` (result [passed declaresTypeAlias] [])

  it "works with keyword-based expectation synthesis of uses" $ do
    let usesType = Expectation "*" "Uses:type"
    let declaresTypeAlias = Expectation "*" "DeclaresTypeAlias"
    run Haskell "type X = Int" [usesType] `shouldReturn` (result [passed declaresTypeAlias] [])

  it "works with operator-based expectation synthesis of declares" $ do
    let declaresNot = Expectation "*" "Declares:not"
    let usesNegation = Expectation "*" "DeclaresNegation"
    run Haskell "x = not True" [declaresNot] `shouldReturn` (result [failed usesNegation] [])

  it "works with operator-based expectation synthesis of uses" $ do
    let usesNot = Expectation "*" "Uses:not"
    let usesNegation = Expectation "*" "UsesNegation"
    run Haskell "x = not True" [usesNot] `shouldReturn` (result [passed usesNegation] [])

  it "works with operators" $ do
    let usesNegation = Expectation "*" "UsesNegation"
    run Haskell "x = not True" [usesNegation] `shouldReturn` (result [passed usesNegation] [])

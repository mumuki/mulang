module ExpectationsCompilerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  let run code binding inspection = compileExpectation (Expectation binding inspection) code

  it "works with HasEntryPoint" $ do
    run (hs "f x = 2") "" "HasEntryPoint" `shouldBe` False

  it "works with HasProcedure" $ do
    run (hs "f x = 2") "" "HasProcedure" `shouldBe` False

  it "works with HasMethod on empty code" $ do
    run (js "") "pepita" "HasMethod:*" `shouldBe` False

  it "works with HasMethod and explicitly anyone object" $ do
    run (js "var pepita = {volar:function(){}}") "pepita" "HasMethod:*" `shouldBe` True
    run (js "var pepita = {}") "pepita" "HasMethod:*" `shouldBe` False

  it "works with HasMethod and named object" $ do
    run (js "var pepita = {volar:function(){}}") "pepita" "HasMethod:volar" `shouldBe` True
    run (js "var pepita = {volar:function(){}}") "pepita" "HasMethod:cantar" `shouldBe` False

  it "works with HasMethod with scopde binding" $ do
    run (js "var pepita = {volar:function(){}}") "Intransitive:pepita.volar" "HasMethod" `shouldBe` True
    run (js "var pepita = {volar:function(){}}") "pepita.cantar" "HasMethod" `shouldBe` False

  it "works with HasFunction with empty binding and empty object" $ do
    run (hs "f x = 2") "" "HasFunction" `shouldBe` True

  it "works with HasFunction with empty binding and explicitly anyone object" $ do
    run (hs "f x = 2") "" "HasFunction:*" `shouldBe` True

  it "works with HasFunction with non-empty binding and empty object" $ do
    run (hs "f x = 2") "f" "HasFunction" `shouldBe` True
    run (hs "f x = 2") "g" "HasFunction" `shouldBe` False

  it "works with HasFunction with empty binding and non-empty object" $ do
    run (hs "f x = 2") "" "HasFunction:f" `shouldBe` True
    run (hs "f x = 2") "" "HasFunction:g" `shouldBe` False

  it "works with HasFunction with empty binding and explicitly equal object" $ do
    run (hs "f x = 2") "" "HasFunction:=f" `shouldBe` True
    run (hs "f x = 2") "" "HasFunction:=g" `shouldBe` False

  it "works with HasFunction with empty binding and explicitly like object" $ do
    run (hs "foo x = 2") "" "HasFunction:~fo" `shouldBe` True
    run (hs "foo x = 2") "" "HasFunction:~go" `shouldBe` False

  it "works with HasWhile" $ do
    run (hs "f x = 2") "" "HasWhile" `shouldBe` False

  it "works with Not" $ do
    run (hs "f x = 2") "" "Not:HasWhile" `shouldBe` True
    run (hs "f x = 2") "f" "Not:HasWhile" `shouldBe` True
    run (hs "f x = 2") "g" "Not:HasWhile" `shouldBe` True

  it "works with Not:HasBinding" $ do
    run (hs "f x = 2") "g" "Not:HasBinding" `shouldBe` True
    run (hs "f x = 2") "f" "Not:HasBinding" `shouldBe` False

  it "works with Not:HasFunction" $ do
    run (hs "f x = 2") ""  "Not:HasFunction" `shouldBe` False
    run (hs "f x = 2") "f" "Not:HasFunction" `shouldBe` False
    run (hs "f x = 2") "g" "Not:HasFunction" `shouldBe` True

  it "works with Not:HasFunction:f" $ do
    run (hs "f x = 2") ""  "Not:HasFunction:f" `shouldBe` False
    run (hs "f x = 2") ""  "Not:HasFunction:g" `shouldBe` True

  it "works with HasSwitch" $ do
    run (hs "f x = 2") "" "HasSwitch" `shouldBe` False

  it "works with HasRepeat" $ do
    run (hs "f x = 2") "" "HasRepeat" `shouldBe` False

  it "works with HasPatternMatching" $ do
    run (hs "f x = 2") "" "HasPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "" "HasPatternMatching" `shouldBe` True

  it "works with HasPatternMatching, with binding" $ do
    run (hs "f x = 2") "f" "HasPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "f" "HasPatternMatching" `shouldBe` True

  it "works with HasVariable" $ do
    run (js "var f = 2") "g" "HasVariable" `shouldBe` False
    run (js "var f = 2") "f" "HasVariable" `shouldBe` True

  it "works with HasArity1" $ do
    run (hs "f a b = 2") "f" "HasArity1" `shouldBe` False
    run (hs "f a = 2") "f" "HasArity1" `shouldBe` True

  it "works with HasArity" $ do
    run (hs "f a b = 2") "f" "HasArity:1" `shouldBe` False
    run (hs "f a = 2") "f" "HasArity:1" `shouldBe` True
    run (hs "f a b = 2") "f" "HasArity:2" `shouldBe` True
    run (hs "f a = 2") "f" "HasArity:2" `shouldBe` False

  it "works with HasObject" $ do
    run (hs "f a b = 2") "f" "HasObject" `shouldBe` False

  it "works with HasClass" $ do
    run (hs "f a b = 2") "f" "HasClass" `shouldBe` False

  it "works with HasUsage" $ do
    run (hs "f a b = g") "m" "HasUsage:g" `shouldBe` False
    run (hs "f a b = h") "f" "HasUsage:g" `shouldBe` False
    run (hs "f a b = g") "f" "HasUsage:g" `shouldBe` True

  it "works with transitive inspections" $ do
    run (hs "f a b = g\ng = m") "f" "HasUsage:h" `shouldBe` False
    run (hs "f a b = g\ng = h") "f" "HasUsage:h" `shouldBe` True

  it "works with transitive inspections" $ do
    run (hs "f a b = g\ng = h") "Intransitive:f" "HasUsage:h" `shouldBe` False
    run (hs "f a b = h") "Intransitive:f" "HasUsage:h" `shouldBe` True

  it "works with HasBinding" $ do
    run (hs "f a b = g") "g" "HasBinding" `shouldBe` False
    run (hs "f a b = g") "f" "HasBinding" `shouldBe` True
    run (hs "f a b = g") ""  "HasBinding:f" `shouldBe` True

  it "works with HasBinding with object" $ do
    run (hs "f a b = g where g = 2") "f" "HasBinding:h" `shouldBe` False
    run (hs "f a b = g where g = 2") "f" "HasBinding:g" `shouldBe` True

  it "works with HasComputation" $ do
    run (hs "f a b = g") "g" "HasComputation" `shouldBe` False
    run (hs "f a b = g") "f" "HasComputation" `shouldBe` True

  it "works with HasTypeSignature" $ do
    run (hs "f x = 1")  "f" "HasTypeSignature" `shouldBe` False
    run (hs "f :: Int") "f" "HasTypeSignature" `shouldBe` True

  it "works with HasComprehension" $ do
    run (hs "f :: Int") "f" "HasComprehension" `shouldBe` False

  it "works with HasDirectRecursion" $ do
    run (hs "f :: Int") "f" "HasDirectRecursion" `shouldBe` False

  it "works with HasComposition" $ do
    run (hs "f :: Int") "f" "HasComposition" `shouldBe` False

  it "works with HasGuards" $ do
    run (hs "f 3 = 3") "f" "HasGuards" `shouldBe` False

  it "works with HasIf" $ do
    run (hs "f 3 = 3") "f" "HasIf" `shouldBe` False

  it "works with HasConditional" $ do
    run (hs "f 3 = 3") "f" "HasConditional" `shouldBe` False

  it "works with HasLambda" $ do
    run (hs "f 3 = 3") "f" "HasLambda" `shouldBe` False


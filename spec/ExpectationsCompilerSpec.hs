module ExpectationsCompilerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  let run code binding inspection = compileExpectation (Expectation binding inspection) code

  it "works with DeclaresEntryPoint" $ do
    run (hs "f x = 2") "" "DeclaresEntryPoint" `shouldBe` False

  it "works with DeclaresProcedure" $ do
    run (hs "f x = 2") "" "DeclaresProcedure" `shouldBe` False

  it "works with DeclaresMethod on empty code" $ do
    run (js "") "pepita" "DeclaresMethod:*" `shouldBe` False

  it "works with DeclaresMethod and explicitly anyone object" $ do
    run (js "var pepita = {volar:function(){}}") "pepita" "DeclaresMethod:*" `shouldBe` True
    run (js "var pepita = {}") "pepita" "DeclaresMethod:*" `shouldBe` False

  it "works with DeclaresMethod and named object" $ do
    run (js "var pepita = {volar:function(){}}") "pepita" "DeclaresMethod:volar" `shouldBe` True
    run (js "var pepita = {volar:function(){}}") "pepita" "DeclaresMethod:cantar" `shouldBe` False

  it "works with DeclaresMethod with scopde binding" $ do
    run (js "var pepita = {volar:function(){}}") "pepita.volar" "DeclaresMethod" `shouldBe` True
    run (js "var pepita = {volar:function(){}}") "pepita.cantar" "DeclaresMethod" `shouldBe` False

  it "works with DeclaresFunction with empty binding and empty object" $ do
    run (hs "f x = 2") "" "DeclaresFunction" `shouldBe` True

  it "works with DeclaresFunction with empty binding and explicitly anyone object" $ do
    run (hs "f x = 2") "" "DeclaresFunction:*" `shouldBe` True

  it "works with DeclaresFunction with non-empty binding and empty object" $ do
    run (hs "f x = 2") "f" "DeclaresFunction" `shouldBe` True
    run (hs "f x = 2") "g" "DeclaresFunction" `shouldBe` False

  it "works with DeclaresFunction with empty binding and non-empty object" $ do
    run (hs "f x = 2") "" "DeclaresFunction:f" `shouldBe` True
    run (hs "f x = 2") "" "DeclaresFunction:g" `shouldBe` False

  it "works with DeclaresFunction with empty binding and explicitly equal object" $ do
    run (hs "f x = 2") "" "DeclaresFunction:=f" `shouldBe` True
    run (hs "f x = 2") "" "DeclaresFunction:=g" `shouldBe` False

  it "works with DeclaresFunction with empty binding and explicitly like object" $ do
    run (hs "foo x = 2") "" "DeclaresFunction:~fo" `shouldBe` True
    run (hs "foo x = 2") "" "DeclaresFunction:~go" `shouldBe` False

  it "works with UsesWhile" $ do
    run (hs "f x = 2") "" "UsesWhile" `shouldBe` False

  it "works with Not" $ do
    run (hs "f x = 2") "" "Not:UsesWhile" `shouldBe` True

  it "works with UsesSwitch" $ do
    run (hs "f x = 2") "" "UsesSwitch" `shouldBe` False

  it "works with HasRepeat" $ do
    run (hs "f x = 2") "" "HasRepeat" `shouldBe` False

  it "works with UsesPatternMatching" $ do
    run (hs "f x = 2") "" "UsesPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "" "UsesPatternMatching" `shouldBe` True

  it "works with UsesPatternMatching, with binding" $ do
    run (hs "f x = 2") "f" "UsesPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "f" "UsesPatternMatching" `shouldBe` True

  it "works with DeclaresVariable" $ do
    run (js "var f = 2") "g" "DeclaresVariable" `shouldBe` False
    run (js "var f = 2") "f" "DeclaresVariable" `shouldBe` True

  it "works with HasArity1" $ do
    run (hs "f a b = 2") "f" "HasArity1" `shouldBe` False
    run (hs "f a = 2") "f" "HasArity1" `shouldBe` True

  it "works with DeclaresObject" $ do
    run (hs "f a b = 2") "f" "DeclaresObject" `shouldBe` False

  it "works with DeclaresClass" $ do
    run (hs "f a b = 2") "f" "DeclaresClass" `shouldBe` False

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

  it "works with Declares" $ do
    run (hs "f a b = g") "g" "Declares" `shouldBe` False
    run (hs "f a b = g") "f" "Declares" `shouldBe` True

  it "works with Declares with object" $ do
    run (hs "f a b = g where g = 2") "f" "Declares:h" `shouldBe` False
    run (hs "f a b = g where g = 2") "f" "Declares:g" `shouldBe` True

  it "works with DeclaresComputation" $ do
    run (hs "f a b = g") "g" "DeclaresComputation" `shouldBe` False
    run (hs "f a b = g") "f" "DeclaresComputation" `shouldBe` True

  it "works with DeclaresTypeSignature" $ do
    run (hs "f x = 1")  "f" "DeclaresTypeSignature" `shouldBe` False
    run (hs "f :: Int") "f" "DeclaresTypeSignature" `shouldBe` True

  it "works with UsesComprehension" $ do
    run (hs "f :: Int") "f" "UsesComprehension" `shouldBe` False

  it "works with DeclaresRecursively" $ do
    run (hs "f :: Int") "f" "DeclaresRecursively" `shouldBe` False

  it "works with UsesComposition" $ do
    run (hs "f :: Int") "f" "UsesComposition" `shouldBe` False

  it "works with UsesGuards" $ do
    run (hs "f 3 = 3") "f" "UsesGuards" `shouldBe` False

  it "works with UsesIf" $ do
    run (hs "f 3 = 3") "f" "UsesIf" `shouldBe` False

  it "works with HasConditional" $ do
    run (hs "f 3 = 3") "f" "HasConditional" `shouldBe` False

  it "works with HasLambda" $ do
    run (hs "f 3 = 3") "f" "HasLambda" `shouldBe` False


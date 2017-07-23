module ExpectationsCompilerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Java

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
    run (js "var pepita = {volar:function(){}}") "pepita.volar" "DeclaresMethod" `shouldBe` False
    run (js "var pepita = {volar:function(){}}") "Intransitive:pepita.volar" "DeclaresMethod:^volar" `shouldBe` False
    run (js "var pepita = {volar:function(){}}") "pepita.cantar" "DeclaresMethod" `shouldBe` False

  it "works with DeclaresFunction with empty binding and empty object" $ do
    run (hs "f x = 2") "" "DeclaresFunction" `shouldBe` True

  it "works with DeclaresFunction with empty binding and explicitly anyone object" $ do
    run (hs "f x = 2") "" "DeclaresFunction:*" `shouldBe` True

  it "works with DeclaresFunction with non-empty binding and empty object" $ do
    run (hs "f x = 2") "f" "DeclaresFunction:^f" `shouldBe` False
    run (hs "f x = 2") "g" "DeclaresFunction:^f" `shouldBe` False

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
    run (hs "f x = 2") "f" "Not:UsesWhile" `shouldBe` True
    run (hs "f x = 2") "g" "Not:UsesWhile" `shouldBe` True

  it "works with Not:Declares" $ do
    run (hs "f x = 2") "g" "Not:Declares" `shouldBe` True
    run (hs "f x = 2") "f" "Not:Declares:^f" `shouldBe` True
    run (hs "f x = m where m = 2") "f" "Not:Declares" `shouldBe` False

  it "works with Not:DeclaresFunction" $ do
    run (hs "f x = 2") ""  "Not:DeclaresFunction" `shouldBe` False
    run (hs "f x = 2") "f" "Not:DeclaresFunction" `shouldBe` True
    run (hs "f x = 2") "g" "Not:DeclaresFunction" `shouldBe` True

  it "works with Not:DeclaresFunction:f" $ do
    run (hs "f x = 2") ""  "Not:DeclaresFunction:f" `shouldBe` False
    run (hs "f x = 2") ""  "Not:DeclaresFunction:g" `shouldBe` True

  it "works with UsesSwitch" $ do
    run (hs "f x = 2") "" "UsesSwitch" `shouldBe` False

  it "works with UsesRepeat" $ do
    run (hs "f x = 2") "" "UsesRepeat" `shouldBe` False

  it "works with UsesPatternMatching" $ do
    run (hs "f x = 2") "" "UsesPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "" "UsesPatternMatching" `shouldBe` True

  it "works with UsesPatternMatching, with binding" $ do
    run (hs "f x = 2") "f" "UsesPatternMatching" `shouldBe` False
    run (hs "f [] = 2\nf _ = 3") "f" "UsesPatternMatching" `shouldBe` True

  it "works with DeclaresVariable" $ do
    run (js "var f = 2") "g" "DeclaresVariable" `shouldBe` False
    run (js "var f = 2") "f" "DeclaresVariable:^f" `shouldBe` False
    run (js "function f() { var m = 2; }") "f" "DeclaresVariable" `shouldBe` True
    run (js "function f() { var m = 2; }") "f" "DeclaresVariable:m" `shouldBe` True

  it "works with DeclaresComputationWithArity1" $ do
    run (hs "f a b = 2") "" "DeclaresComputationWithArity1:f" `shouldBe` False
    run (hs "f a = 2")   "" "DeclaresComputationWithArity1:f" `shouldBe` True

  it "works with DeclaresComputationWithArity" $ do
    run (hs "f a b = 2") "" "DeclaresComputationWithArity1:f" `shouldBe` False
    run (hs "f a = 2")   "" "DeclaresComputationWithArity1:f" `shouldBe` True
    run (hs "f a b = 2") "" "DeclaresComputationWithArity2:f" `shouldBe` True
    run (hs "f a = 2")   "" "DeclaresComputationWithArity2:f" `shouldBe` False

  it "works with DeclaresObject" $ do
    run (hs "f a b = 2") "f" "DeclaresObject" `shouldBe` False

  it "works with DeclaresClass" $ do
    run (hs "f a b = 2") "f" "DeclaresClass" `shouldBe` False

  it "works with Uses" $ do
    run (hs "f a b = g") "m" "Uses:g" `shouldBe` False
    run (hs "f a b = h") "f" "Uses:g" `shouldBe` False
    run (hs "f a b = g") "f" "Uses:g" `shouldBe` True

  it "works with transitive inspections" $ do
    run (hs "f a b = g\ng = m") "f" "Uses:h" `shouldBe` False
    run (hs "f a b = g\ng = h") "f" "Uses:h" `shouldBe` True

  it "works with transitive inspections" $ do
    run (hs "f a b = g\ng = h") "Intransitive:f" "Uses:h" `shouldBe` False
    run (hs "f a b = h") "Intransitive:f" "Uses:h" `shouldBe` True

  it "works with Declares" $ do
    run (hs "f a b = g") "g" "Declares" `shouldBe` False
    run (hs "f a b = g") "f" "Declares:^f" `shouldBe` False
    run (hs "f a b = g") "" "Declares" `shouldBe` True

  it "works with Declares with object" $ do
    run (hs "f a b = g") ""  "Declares:f" `shouldBe` True
    run (hs "f a b = g") ""  "Declares:g" `shouldBe` False
    run (hs "f a b = g where g = 2") "f" "Declares:h" `shouldBe` False
    run (hs "f a b = g where g = 2") "f" "Declares:g" `shouldBe` True

  it "works with DeclaresComputation" $ do
    run (hs "f a b = g") "g" "DeclaresComputation" `shouldBe` False
    run (hs "f a b = g") "f" "DeclaresComputation" `shouldBe` False
    run (hs "f a b = g") ""  "DeclaresComputation:f" `shouldBe` True

  it "works with DeclaresTypeSignature" $ do
    run (hs "f x = 1")  "" "DeclaresTypeSignature:f" `shouldBe` False
    run (hs "f :: Int") "" "DeclaresTypeSignature:f" `shouldBe` True

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

  it "works with UsesConditional" $ do
    run (hs "f 3 = 3") "f" "UsesConditional" `shouldBe` False

  it "works with UsesLambda" $ do
    run (hs "f 3 = 3") "f" "UsesLambda" `shouldBe` False

  it "works with Implements" $ do
    run (java "class Foo implements Bar {}") "Foo" "Implements:Bar" `shouldBe` True
    run (java "class Foo implements Bar {}") "Foo" "Implements:Baz" `shouldBe` False

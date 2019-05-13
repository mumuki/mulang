module ExplangAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result explangTestResults smells
  = emptyCompletedAnalysisResult { explangTestResults = explangTestResults, smells = smells }

run language content test = analyse (explangTestAnalysis (CodeSample language content) test)

passed message = ExplangTestResult message True
failed message = ExplangTestResult message False

nok = result [failed "E0"] []
ok = result [passed "E0"] []

spec = describe "ExpectationsAnalyzer" $ do
  it "evaluates usesIf" $ do
    (run JavaScript "" "test \"must use if\": UsesIf") `shouldReturn` (result [failed "must use if"] [])
    (run JavaScript "if (true) {}" "test \"must use if\": UsesIf") `shouldReturn` (result [passed "must use if"] [])

  it "evaluates count (uses for)" $ do
    (run JavaScript "for (let x in []) {}" "test: count (uses for) = 0") `shouldReturn` nok
    (run JavaScript "for (let x in []) {}" "test: count (uses for) = 1") `shouldReturn` ok
    (run JavaScript "for (let x in []) {}" "test: count (uses for) = 2") `shouldReturn` nok

  it "evaluates count (declares variable)" $ do
    (run JavaScript "var x = 1" "test: count (declares variable) = 0") `shouldReturn` nok
    (run JavaScript "var x = 1" "test: count (declares variable) = 1") `shouldReturn` ok
    (run JavaScript "var x = 1" "test: count (declares variable) = 2") `shouldReturn` nok
    (run JavaScript "var x = 1;\n\
                    \let y = 2" "test: count (declares variable) = 2") `shouldReturn` ok

  it "evaluates declares procedure that (assigns `finished` with true)" $ do
    let test = "test: declares procedure that (assigns `finished` with true)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function finish(){}" test) `shouldReturn` nok
    (run JavaScript "function finish() { finished = false }" test) `shouldReturn` nok
    (run JavaScript "function finish() { finished = true }" test) `shouldReturn` ok

  it "evaluates declares function like `total` that (returns with math)" $ do
    let test = "test: declares function like `total` that (returns with math)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` nok

  it "evaluates declares function like `total` with math" $ do
    let test = "declares function like `total` with math"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` nok

  it "evaluates declares function like `total` that (uses math)" $ do
    let test = "declares function like `total` that (uses math)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` ok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { return magicNumber }" test) `shouldReturn` ok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { var z = magicNumber; return 0 }" test) `shouldReturn` ok

  it "evaluates declares function like `total` that (returns something that (uses math))" $ do
    let test = "declares function like `total` that (returns something that (uses math))"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` ok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { return magicNumber }" test) `shouldReturn` ok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { var z = magicNumber; return 0 }" test) `shouldReturn` nok

  it "evaluates count (declares variable like `client`)" $ do
    (run JavaScript "var clientName = 'jon'" "test: count (declares variable like `client`) = 0") `shouldReturn` nok
    (run JavaScript "var clientName = 'jon'" "test: count (declares variable like `client`) = 1") `shouldReturn` ok
    (run JavaScript "var clientName = 'jon'" "test: count (declares variable like `client`) = 2") `shouldReturn` nok
    (run JavaScript "var clientName = 'jon';\n\
                    \let clientSurname = 'doe'" "test: count (declares variable like `client`) = 2") `shouldReturn` ok
    (run JavaScript "var clientName = 'jon';\n\
                    \let clientSurname = 'doe';\n\
                    \let username = 'jondoe4'" "test: count (declares variable like `client`) = 3") `shouldReturn` nok

  it "evaluates count (declares variable with \"jon\")" $ do
      (run JavaScript "var clientName = 'jon'" "test: count (declares variable with \"jon\") = 0") `shouldReturn` nok
      (run JavaScript "var clientName = 'jon'" "test: count (declares variable with \"jon\") = 1") `shouldReturn` ok
      (run JavaScript "var username = 'jondoe'" "test: count (declares variable with \"jon\") = 1") `shouldReturn` nok
      (run JavaScript "var clientName = 'jon'" "test: count (declares variable with \"jon\") = 2") `shouldReturn` nok
      (run JavaScript "var clientName = 'jon';\n\
                      \let clientSurname = 'doe'" "test: count (declares variable with \"jon\") = 2") `shouldReturn` nok

  it "evaluates count (uses if)" $ do
    (run JavaScript "" "test: count (UsesIf) = 0") `shouldReturn` ok
    (run JavaScript "" "test: count (UsesIf) = 1") `shouldReturn` nok

    (run JavaScript "" "test: count (UsesIf) >= 0") `shouldReturn` ok
    (run JavaScript "" "test: count (UsesIf) >= 1") `shouldReturn` nok
    (run JavaScript "" "test: count (UsesIf) >= 2") `shouldReturn` nok

    (run JavaScript "if (true) {}" "test: count (UsesIf) >= 0") `shouldReturn` ok
    (run JavaScript "if (true) {}" "test: count (UsesIf) >= 1") `shouldReturn` ok
    (run JavaScript "if (true) {}" "test: count (UsesIf) >= 2") `shouldReturn` nok

    (run JavaScript "if (true) {}; if (false) {}" "test: count (UsesIf) >= 0") `shouldReturn` ok
    (run JavaScript "if (true) {}; if (false) {}" "test: count (UsesIf) >= 1") `shouldReturn` ok
    (run JavaScript "if (true) {}; if (false) {}" "test: count (UsesIf) >= 2") `shouldReturn` ok

    (run JavaScript "if (true) {}; if(true) {}; if (false) {}" "test: count (UsesIf) >= 3") `shouldReturn` ok

  it "is case sensitive in standard syntax" $ do
    (run JavaScript "" "test: usesif") `shouldReturn` ok

  it "is white-space insensitive in extended syntax" $ do
    (run JavaScript "" "test: Uses If") `shouldReturn` nok
    (run JavaScript "if (true) {}" "test: Uses If") `shouldReturn` ok

  it "is case insensitive in extended syntax" $ do
    (run JavaScript "" "test: uses if") `shouldReturn` nok
    (run JavaScript "if (true) {}" "test: uses if") `shouldReturn` ok


      -- f "DeclaresAttribute"   m            = boundMatching countAttributes m
      -- f "DeclaresClass"       m            = boundMatching countClasses m
      -- f "DeclaresFunction"    m            = boundMatching countFunctions m
      -- f "DeclaresInterface"   m            = boundMatching countInterfaces m
      -- f "DeclaresMethod"      m            = boundMatching countMethods m
      -- f "DeclaresObject"      m            = boundMatching countObjects m
      -- f "DeclaresProcedure"   m            = boundMatching countProcedures m

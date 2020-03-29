module CustomExpectationsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result customExpectationResults smells
  = emptyCompletedAnalysisResult { expectationResults = customExpectationResults, smells = smells }

run language content test = analyse (customExpectationsAnalysis (CodeSample language content) test)

passed message = customExpectationResult message True
failed message = customExpectationResult message False

nok = result [failed "E0"] []
ok = result [passed "E0"] []

spec = describe "ExpectationsAnalyzer" $ do
  it "evaluates usesIf" $ do
    (run JavaScript "" "expectation \"must use if\": UsesIf") `shouldReturn` (result [failed "must use if"] [])
    (run JavaScript "if (true) {}" "expectation \"must use if\": UsesIf") `shouldReturn` (result [passed "must use if"] [])

  it "evaluates count (calls `esMultiploDe`)" $ do
    let test = "expectation: count(calls `esMultiploDe`) = 2;\n"
    (run JavaScript "function esNumeroDeLaSuerte(x) {\r\n  return x > 0 && x != 15 ; \r\n} " test) `shouldReturn`  nok;
    (run JavaScript "function esNumeroDeLaSuerte(x) {\r\n  return x > 0 && esMultiploDe(2, x) && x != 15 ; \r\n} " test) `shouldReturn`  nok;
    (run JavaScript "function esNumeroDeLaSuerte(x) {\r\n  return x > 0 && (esMultiploDe(2, x) || esMultiploDe(3, x)) && x != 15 ; \r\n} " test) `shouldReturn`  ok;

  it "evaluates count (uses for)" $ do
    (run JavaScript "for (let x in []) {}" "expectation: count (uses for) = 0") `shouldReturn` nok
    (run JavaScript "for (let x in []) {}" "expectation: count (uses for) = 1") `shouldReturn` ok
    (run JavaScript "for (let x in []) {}" "expectation: count (uses for) = 2") `shouldReturn` nok

  it "evaluates count (declares variable)" $ do
    (run JavaScript "var x = 1" "expectation: count (declares variable) = 0") `shouldReturn` nok
    (run JavaScript "var x = 1" "expectation: count (declares variable) = 1") `shouldReturn` ok
    (run JavaScript "var x = 1" "expectation: count (declares variable) = 2") `shouldReturn` nok
    (run JavaScript "var x = 1;\n\
                    \let y = 2" "expectation: count (declares variable) = 2") `shouldReturn` ok

  it "evaluates count (declares procedure)" $ do
    (run JavaScript "" "expectation: count (declares procedure) = 0") `shouldReturn` ok
    (run JavaScript "function f(){}" "expectation: count (declares procedure) = 0") `shouldReturn` nok
    (run JavaScript "function f(){}" "expectation: count (declares procedure) = 1") `shouldReturn` ok
    (run JavaScript "function f1(){} function f2(){}" "expectation: count (declares procedure) = 1") `shouldReturn` nok
    (run JavaScript "function f1(){} function f2(){}" "expectation: count (declares procedure) = 2") `shouldReturn` ok

  it "evaluates declares procedure that (assigns)" $ do
    let test = "expectation: declares procedure that (assigns)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function finish(){}" test) `shouldReturn` nok
    (run JavaScript "function finish() { pending = false }" test) `shouldReturn` ok
    (run JavaScript "function finish() { finished = false }" test) `shouldReturn` ok
    (run JavaScript "function finish() { finished = true }" test) `shouldReturn` ok

  it "evaluates declares procedure that (assigns `finished` with true)" $ do
    let test = "expectation: declares procedure that (assigns `finished` with true)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function finish(){}" test) `shouldReturn` nok
    (run JavaScript "function finish() { pending = false }" test) `shouldReturn` nok
    (run JavaScript "function finish() { finished = false }" test) `shouldReturn` nok
    (run JavaScript "function finish() { finished = true }" test) `shouldReturn` ok

  it "evaluates declares function like `total` that (returns with nil)" $ do
    let test = "expectation: declares function like `total` that (returns with nil)"

    (run JavaScript "function totalAmount() { return 2 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return null }" test) `shouldReturn` ok

  it "evaluates returns with nil" $ do
    let test = "expectation: returns with nil"

    (run JavaScript "function totalAmount() { return 2 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return null }" test) `shouldReturn` ok

  it "evaluates returns with math" $ do
    let test = "expectation: returns with math"

    (run JavaScript "function totalAmount() { return 2 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return null }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 4 + 4 }" test) `shouldReturn` ok


  it "evaluates declares function like `total` that (returns with math)" $ do
    let test = "expectation: declares function like `total` that (returns with math)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` nok

  it "evaluates declares function like `total` with math" $ do
    let test = "expectation: declares function like `total` with math"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` ok

  it "evaluates declares function like `total` that (uses math)" $ do
    let test = "expectation: declares function like `total` that (uses math)"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` ok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { return magicNumber }" test) `shouldReturn` nok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { var z = magicNumber; return 0 }" test) `shouldReturn` nok

  it "evaluates declares function like `total` that (returns something that (uses math))" $ do
    let test = "expectation: declares function like `total` that (returns something that (uses math))"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() {}" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return 0 }" test) `shouldReturn` nok
    (run JavaScript "function totalAmount() { return x + y }" test) `shouldReturn` ok
    (run JavaScript "function totalAmount() { var z = x + y; return z }" test) `shouldReturn` nok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { return magicNumber }" test) `shouldReturn` nok
    (run JavaScript "var magicNumber = 10 + 1; function totalAmount() { var z = magicNumber; return 0 }" test) `shouldReturn` nok

  it "evaluates count (declares variable like `client`)" $ do
    (run JavaScript "var clientName = 'jon'" "expectation: count (declares variable like `client`) = 0") `shouldReturn` nok
    (run JavaScript "var clientName = 'jon'" "expectation: count (declares variable like `client`) = 1") `shouldReturn` ok
    (run JavaScript "var clientName = 'jon'" "expectation: count (declares variable like `client`) = 2") `shouldReturn` nok
    (run JavaScript "var clientName = 'jon';\n\
                    \let clientSurname = 'doe'" "expectation: count (declares variable like `client`) = 2") `shouldReturn` ok
    (run JavaScript "var clientName = 'jon';\n\
                    \let clientSurname = 'doe';\n\
                    \let username = 'jondoe4'" "expectation: count (declares variable like `client`) = 3") `shouldReturn` nok

  it "evaluates count (declares variable with \"jon\")" $ do
      (run JavaScript "var clientName = 'jon'" "expectation: count (declares variable with \"jon\") = 0") `shouldReturn` nok
      (run JavaScript "var clientName = 'jon'" "expectation: count (declares variable with \"jon\") = 1") `shouldReturn` ok
      (run JavaScript "var username = 'jondoe'" "expectation: count (declares variable with \"jon\") = 1") `shouldReturn` nok
      (run JavaScript "var clientName = 'jon'" "expectation: count (declares variable with \"jon\") = 2") `shouldReturn` nok
      (run JavaScript "var clientName = 'jon';\n\
                      \let clientSurname = 'doe'" "expectation: count (declares variable with \"jon\") = 2") `shouldReturn` nok

  it "evaluates count (uses if)" $ do
    (run JavaScript "" "expectation: count (UsesIf) = 0") `shouldReturn` ok
    (run JavaScript "" "expectation: count (UsesIf) = 1") `shouldReturn` nok

    (run JavaScript "" "expectation: count (UsesIf) >= 0") `shouldReturn` ok
    (run JavaScript "" "expectation: count (UsesIf) >= 1") `shouldReturn` nok
    (run JavaScript "" "expectation: count (UsesIf) >= 2") `shouldReturn` nok

    (run JavaScript "if (true) {}" "expectation: count (UsesIf) >= 0") `shouldReturn` ok
    (run JavaScript "if (true) {}" "expectation: count (UsesIf) >= 1") `shouldReturn` ok
    (run JavaScript "if (true) {}" "expectation: count (UsesIf) >= 2") `shouldReturn` nok

    (run JavaScript "if (true) {}; if (false) {}" "expectation: count (UsesIf) >= 0") `shouldReturn` ok
    (run JavaScript "if (true) {}; if (false) {}" "expectation: count (UsesIf) >= 1") `shouldReturn` ok
    (run JavaScript "if (true) {}; if (false) {}" "expectation: count (UsesIf) >= 2") `shouldReturn` ok

    (run JavaScript "if (true) {}; if(true) {}; if (false) {}" "expectation: count (UsesIf) >= 3") `shouldReturn` ok

  it "rejects unsupported counters" $ do
    (run JavaScript "if (true) {}; " "expectation: count (declares) >= 3") `shouldReturn` AnalysisFailed "Can not count over Declares with matcher Unmatching"

  it "is case sensitive in standard syntax" $ do
    (run JavaScript "" "expectation: UsesIf") `shouldReturn` nok
    (run JavaScript "" "expectation: usesif") `shouldReturn` AnalysisFailed "Unknown inspection Usesif with matcher Unmatching"

  it "accepts titlecase in standard syntax" $ do
    (run JavaScript "" "expectation: UsesIf") `shouldReturn` nok

  it "accepts camelcase in standard syntax" $ do
    (run JavaScript "" "expectation: usesIf") `shouldReturn` nok

  it "is white-space insensitive in extended syntax" $ do
    (run JavaScript "" "expectation: Uses If") `shouldReturn` nok
    (run JavaScript "if (true) {}" "expectation: Uses If") `shouldReturn` ok

  it "is case insensitive in extended syntax" $ do
    (run JavaScript "" "expectation: uses if") `shouldReturn` nok
    (run JavaScript "if (true) {}" "expectation: uses if") `shouldReturn` ok

  it "evaluates usesNot" $ do
    let test = "expectation: usesNot"

    (run Prolog "" test) `shouldReturn` nok
    (run Prolog "foo(X):-bar(X)." test) `shouldReturn` nok
    (run Prolog "foo(X):-not(bar(X))." test) `shouldReturn` ok

  it "evaluates uses not" $ do
    let test = "expectation: uses not"

    (run Prolog "" test) `shouldReturn` nok
    (run Prolog "foo(X):-bar(X)." test) `shouldReturn` nok
    (run Prolog "foo(X):-not(bar(X))." test) `shouldReturn` ok

  it "evaluates uses `not`" $ do
    -- (run Haskell "x = not y" "expectation: uses `not`") `shouldReturn` nok
    pendingWith "autocorrector does not work with EDL"

  it "evaluates uses `!`" $ do
    -- (run JavaScript "var x = ! y" "expectation: uses `!`") `shouldReturn` nok
    pendingWith "autocorrector does not work with EDL"

  it "evaluates uses negation" $ do
    let test = "expectation: uses negation"

    (run Haskell "" test) `shouldReturn` nok
    (run Haskell "x = not y" test) `shouldReturn` ok
    (run Haskell "x = y" test) `shouldReturn` nok

  it "evaluates UsesNegation" $ do
    let test = "expectation: UsesNegation"

    (run Haskell "" test) `shouldReturn` nok
    (run Haskell "x = not y" test) `shouldReturn` ok
    (run Haskell "x = y" test) `shouldReturn` nok

  it "evaluates uses and" $ do
    let test = "expectation: uses and"

    (run Haskell "" test) `shouldReturn` nok
    (run Haskell "x = y && z" test) `shouldReturn` ok
    (run Haskell "x = y" test) `shouldReturn` nok

  it "evaluates if with anything" $ do
    let test = "expectation: uses if with (anything, literal, nonliteral)"

    (run Haskell "" test) `shouldReturn` nok
    (run Haskell "x y = if y then True else y" test) `shouldReturn` ok
    (run Haskell "x y = if y then True else False" test) `shouldReturn` nok
    (run Haskell "x y = if y then y else y" test) `shouldReturn` nok

  it "evaluates if with nested matchers" $ do
    let test = "expectation: uses if with (anything, something that (!uses while), something that (!uses while));"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "if (window) { console.log('ok') }" test) `shouldReturn` ok
    (run JavaScript "if (window) { console.log('ok') } else { console.log('nok') }" test) `shouldReturn` ok
    (run JavaScript "if (window) { while(true) { } } else { console.log('nok') }" test) `shouldReturn` nok
    (run JavaScript "if (window) { console.log('ok') } else { while(true){} }" test) `shouldReturn` nok
    (run JavaScript "if (window) { while(true) { } }" test) `shouldReturn` nok

  it "evaluates foor-loop with nested matchers" $ do
    let test = "expectation: uses for loop with (something that (declares variable `x`), anything, anything, anything);"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "for (var x = 0; x < 10; x++) { x; }" test) `shouldReturn` ok
    (run JavaScript "for (let x = 0; x < 10; x++) { x; }" test) `shouldReturn` ok
    (run JavaScript "for (let y = 0; y < 10; y++) { y; }" test) `shouldReturn` nok
    (run JavaScript "for (x = 0; x < 10; x++) { x; }" test) `shouldReturn` nok

  it "evaluates foor-loop with incomplete matchers tuple" $ do
    let test = "expectation: uses for loop with (something that (declares variable `x`));"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "for (var x = 0; x < 10; x++) { x; }" test) `shouldReturn` ok
    (run JavaScript "for (let x = 0; x < 10; x++) { x; }" test) `shouldReturn` ok
    (run JavaScript "for (let y = 0; y < 10; y++) { y; }" test) `shouldReturn` nok
    (run JavaScript "for (x = 0; x < 10; x++) { x; }" test) `shouldReturn` nok

  it "evaluates foor-loop with one-arg simplified syntax " $ do
    let test = "expectation: uses for loop that (declares variable `x`);"

    (run JavaScript "" test) `shouldReturn` nok
    (run JavaScript "for (var x = 0; x < 10; x++) { x; }" test) `shouldReturn` ok
    (run JavaScript "for (let x = 0; x < 10; x++) { x; }" test) `shouldReturn` ok
    (run JavaScript "for (let y = 0; y < 10; y++) { y; }" test) `shouldReturn` nok
    (run JavaScript "for (x = 0; x < 10; x++) { x; }" test) `shouldReturn` nok

  it "evaluates uses `&&`" $ do
    -- (run Haskell "x = y && x" "expectation: uses `&&`") `shouldReturn` ok
    pendingWith "autocorrector does not work with EDL"

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module RunnerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Python
import           Language.Mulang.Interpreter.Runner
import           Language.Mulang.Interpreter.Tests

import           Data.Text (unpack, append)
import           NeatInterpolation (text)

parse language = language . unpack
run language code suite = runTests ast (getTests ast)
  where ast = parse language (append code suite)
runjs = run js
runpy = run py

spec :: Spec
spec = do
  describe "runTests" $ do
    context "javascript" $ do
      it "runs tests" $ do
        let code = [text||]
        let suite = [text|
          it("is true", function() {
            assert(true)
          })
        |]
        runjs code suite `shouldReturn` [TestResult ["is true"] Success]

      context "assert.equals" $ do
        it "passes if values are equal" $ do
          let code = [text||]
          let suite = [text|
            it("passes", function() {
              assert.equals(1, 1)
            })
          |]
          runjs code suite `shouldReturn` [TestResult ["passes"] Success]

        it "fails if values are not equal" $ do
          let code = [text||]
          let suite = [text|
            it("fails", function() {
              assert.equals(1, 2)
            })
          |]
          runjs code suite `shouldReturn` [TestResult ["fails"] (Failure "MuString \"Expected MuNumber 1.0 but got: MuNumber 2.0\"")]

      it "can handle failed tests" $ do
        let code = [text||]
        let suite = [text|
          it("fails", function() {
            assert(false)
          })
        |]
        runjs code suite `shouldReturn` [TestResult ["fails"] (Failure "MuString \"Expected True but got: False\"")]

      it "can handle errored tests" $ do
        let code = [text||]
        let suite = [text|
          it("errors", function() {
            assert.equals(succ(3), 4)
          })
        |]
        runjs code suite `shouldReturn` [TestResult ["errors"] (Failure "MuString \"Reference not found for name 'succ'\"")]

      it "can reference functions defined in code" $ do
        let code = [text|
          function succ(n) {
            return n + 1;
          }
        |]
        let suite = [text|
          it("succ increments a given number by 1", function() {
            assert.equals(succ(3), 4)
          })
        |]
        runjs code suite `shouldReturn` [TestResult ["succ increments a given number by 1"] Success]

      it "accepts describes" $ do
        let code = [text|
          function succ(n) {
            return n + 1;
          }
        |]
        let suite = [text|
          describe("succ", function () {
            it("increments a given number by 1", function() {
              assert.equals(succ(3), 4)
            })
          })
        |]
        runjs code suite `shouldReturn` [TestResult ["succ", "increments a given number by 1"] Success]

      it "accepts multiple test cases" $ do
        let code = [text|
          function succ(n) {
            return n + 1;
          }
        |]
        let suite = [text|
          it("if I pass a 3 to succ it returns 4", function() {
            assert.equals(succ(3), 4)
          })

          it("if I pass a 10 to succ it returns 11", function() {
            assert.equals(succ(10), 11)
          })
        |]
        runjs code suite `shouldReturn` [(TestResult ["if I pass a 3 to succ it returns 4"] Success),
                                         (TestResult ["if I pass a 10 to succ it returns 11"] Success)]


    context "python" $ do
      it "runs tests" $ do
        let code = [text||]
        let suite = [text|
          class TestPython(unittest.TestCase):
            def test_is_true():
              self.assertTrue(True)
        |]
        runpy code suite `shouldReturn` [TestResult ["TestPython", "test_is_true"] Success]

      context "assert.equals" $ do
        it "passes if values are equal" $ do
          let code = [text||]
          let suite = [text|
            class TestPython(unittest.TestCase):
              def test_passes():
                self.assertEqual(1, 1)
          |]
          runpy code suite `shouldReturn` [(TestResult ["TestPython", "test_passes"] Success)]

        it "fails if values are not equal" $ do
          let code = [text||]
          let suite = [text|
            class TestPython(unittest.TestCase):
              def test_fails():
                self.assertEqual(1, 2)
          |]
          runpy code suite `shouldReturn` [TestResult ["TestPython", "test_fails"] (Failure "MuString \"Expected MuNumber 1.0 but got: MuNumber 2.0\"")]

      it "can handle failed tests" $ do
        let code = [text||]
        let suite = [text|
          class TestPython(unittest.TestCase):
            def test_fails():
              self.assertTrue(False)
        |]
        runpy code suite `shouldReturn` [TestResult ["TestPython", "test_fails"] (Failure "MuString \"Expected True but got: False\"")]

      it "can handle errored tests" $ do
        let code = [text||]
        let suite = [text|
          class TestPython(unittest.TestCase):
            def test_errors():
              self.assertEqual(succ(3), 4)
        |]
        runpy code suite `shouldReturn` [TestResult ["TestPython", "test_errors"] (Failure "MuString \"Reference not found for name 'succ'\"")]

      it "can reference functions defined in code" $ do
        let code = [text|
          def succ(n):
            return n + 1
        |]
        let suite = [text|
          class TestPython(unittest.TestCase):
            def test_succ_increments_a_given_numer_by_1():
              self.assertEqual(succ(3), 4)
        |]
        runpy code suite `shouldReturn` [TestResult ["TestPython", "test_succ_increments_a_given_numer_by_1"] Success]

      it "accepts multiple test cases" $ do
        let code = [text|
          def succ(n):
            return n + 1
        |]
        let suite = [text|
          class TestSucc(unittest.TestCase):
            def test_if_I_pass_a_3_to_succ_it_returns_4():
              self.assertEqual(succ(3), 4)

            def test_if_I_pass_a_10_to_succ_it_returns_11():
              self.assertEqual(succ(10), 11)
        |]
        runpy code suite `shouldReturn` [(TestResult ["TestSucc", "test_if_I_pass_a_3_to_succ_it_returns_4"] Success),
                                         (TestResult ["TestSucc", "test_if_I_pass_a_10_to_succ_it_returns_11"] Success)]

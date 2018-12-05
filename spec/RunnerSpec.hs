{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module RunnerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Parsers.JavaScript
import           Interpreter.Runner
import           Interpreter.Mulang.Tests

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

parse = js . unpack
run code testSuite = runTests (parse code) (getTests $ parse testSuite)

spec :: Spec
spec = do
  describe "runTests" $ do
    it "runs tests" $ do
      let code = [text||]
      let suite = [text|
        it("is true", function() {
          assert(true)
        })
      |]
      run code suite `shouldReturn` [(["is true"], Success)]

    it "can handle failed tests" $ do
      let code = [text||]
      let suite = [text|
        it("fails", function() {
          assert(false)
        })
      |]
      run code suite `shouldReturn` [(["fails"], Failure "MuString \"Expected true but got: MuBool False\"")]

    it "can handle errored tests" $ do
      let code = [text||]
      let suite = [text|
        it("errors", function() {
          assert.equals(succ(3), 4)
        })
      |]
      run code suite `shouldReturn` [(["errors"], Failure "MuString \"Reference not found for name 'succ'\"")]

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
      run code suite `shouldReturn` [(["succ increments a given number by 1"], Success)]

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
      run code suite `shouldReturn` [(["succ", "increments a given number by 1"], Success)]

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
      run code suite `shouldReturn` [(["if I pass a 3 to succ it returns 4"], Success),
                                     (["if I pass a 10 to succ it returns 11"], Success)]


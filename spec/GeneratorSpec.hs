module GeneratorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Generator
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)

spec :: Spec
spec = do
  describe "declaredIdentifiers" $ do
    context "standard code" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2\n\
                    \w k = p\n\
                    \     where z = 2"
      it "answers declared identifiers" $ do
        (declaredIdentifiers code) `shouldBe` ["f", "w", "z"]

    context "generic elements" $ do
      it "answers declared nested positional identifiers" $ do
        let code = (Element "a" [] [Element "b" [] [Element "c" [] [], Element "d" [] []]])
        (declaredIdentifiers code) `shouldBe` ["a", "b", "c", "d"]

      it "answers declared nested named identifiers" $ do
        let code = (Element "a" [] [Element "b" [("x", (Element "c" [] [])), ("y", (Element "d" [] []))] []])
        (declaredIdentifiers code) `shouldBe` ["a", "b", "c", "d"]

  describe "referencedIdentifiers" $ do
    it "answers referenced identifiers" $ do
      let code = hs "f x =  (:[]) . m x y . g h 2"
      (referencedIdentifiers code) `shouldBe` ["flip", ":", "m","x","y", "g","h"]

  describe "transitiveReferencedIdentifiers" $ do
    it "answers transitive referenced identifiers" $ do
      let code = hs "f x = m x\n\
                 \m 0 = p 0\n\
                 \p x = g x"
      (transitiveReferencedIdentifiers "f" code) `shouldBe` ["f","m","x","p","g"]

  describe "equationsExpandedExpressions" $ do
    describe "simple procedures" $ do
      let run e = let (Procedure _ equations) = js e in equationsExpandedExpressions equations

      it "answers the expressions in a blank procedure" $ do
        run "function f() { }" `shouldBe` [None]

      it "answers the expressions in a one-liner procedure" $ do
        run "function f() { this; }" `shouldBe` [Self]

      it "answers the expressions in a multi-liner procedure" $ do
        run "function f() { x(); y(); }" `shouldBe` [Application (Reference "x") [], Application (Reference "y") []]

    describe "multi-equation functions" $ do
      let run e = let (Function _ equations) = hs e in equationsExpandedExpressions equations

      it "answers the expressions in a unguarded function" $ do
        run "g 0 = y\ng _ = z" `shouldBe` [Return (Reference "y"), Return (Reference "z")]

      it "answers the expressions in a guarded function" $ do
        (length . run) "g x | x > 0 = y\ng _ = 3" `shouldBe` 3

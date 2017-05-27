module SignatureSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Signature
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Prolog
import           Language.Mulang.Explorer

import           Data.List (transpose, nub)
import           Control.Monad (msum)

signaturesOf :: Expression -> [Signature]
signaturesOf = nub . map (signatureOf.snd) . declarationsOf

signatureOf :: Expression -> Signature
signatureOf (FunctionDeclaration name equations)  = NamedSignature name (parameterNamesOf equations)
signatureOf (ProcedureDeclaration name equations) = NamedSignature name (parameterNamesOf equations)
signatureOf (MethodDeclaration name equations)    = NamedSignature name (parameterNamesOf equations)
signatureOf (RuleDeclaration name args _)         = AritySignature name (length args)
signatureOf (FactDeclaration name args)           = AritySignature name (length args)

parameterNamesOf :: [Equation] -> [Maybe Binding]
parameterNamesOf = map msum . transpose . map (map parameterNameOf . equationParams)

parameterNameOf :: Pattern -> Maybe Binding
parameterNameOf (VariablePattern v) = Just v
parameterNameOf _                   = Nothing

spec :: Spec
spec = do
  describe "NamedSignature" $ do
    it "empty expression" $ do
      signaturesOf (js "") `shouldBe` []

    it "nullary function" $ do
      signaturesOf (js "function foo() {}") `shouldBe` [NamedSignature "foo" []]

    it "one-arg function" $ do
      signaturesOf (js "function foo(x) {}") `shouldBe` [NamedSignature "foo" [Just "x"]]

    it "binary function" $ do
      signaturesOf (js "function foo(x, y) {}") `shouldBe` [NamedSignature "foo" [Just "x", Just "y"]]

    it "binary function with non variable pattern" $ do
      signaturesOf (hs "foo x _ = x") `shouldBe` [NamedSignature "foo" [Just "x", Nothing]]

    it "binary function with multiple complmentary equations" $ do
      signaturesOf (hs "foo x 1 = x\nfoo _ y = y") `shouldBe` [
                                                      NamedSignature "foo" [Just "x", Just "y"]]

    it "binary function with multiple non-complementary equations" $ do
      signaturesOf (hs "foo x 1 _ = x\nfoo p 2 z = z\nfoo _ 3 _= 3") `shouldBe` [
                                                      NamedSignature "foo" [Just "x", Nothing, Just "z"]]

  describe "AritySignature" $ do
    it "empty expression" $ do
      signaturesOf (pl "") `shouldBe` []

    it "just a fact" $ do
      signaturesOf (pl "good(dog).") `shouldBe` [AritySignature "good" 1]

    it "two facts" $ do
      signaturesOf (pl "good(dog).good(cat).") `shouldBe` [AritySignature "good" 1]

    it "a fact of arity 2" $ do
      signaturesOf (pl "legs(dog, 4).") `shouldBe` [AritySignature "legs" 2]

    it "a fact of arity 2 and another of arity 3" $ do
      signaturesOf (pl "legs(dog, 4).wings(dragon).wings(bird).") `shouldBe` [
                                                                      AritySignature "legs" 2,
                                                                      AritySignature "wings" 1]
    it "two facts of same name, different arity" $ do
      signaturesOf (pl "legs(dog, 4).legs(dragon).") `shouldBe` [ AritySignature "legs" 2,
                                                                  AritySignature "legs" 1]


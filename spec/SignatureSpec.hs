module SignatureSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Signature
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Prolog
import           Language.Mulang.Explorer

import           Data.List (transpose, nub)
import           Data.Maybe (mapMaybe)
import           Control.Monad (msum)

signaturesOf :: Expression -> [Signature]
signaturesOf = nub . mapMaybe (signatureOf.snd) . declarationsOf

signatureOf :: Expression -> Maybe Signature
signatureOf (FunctionDeclaration name equations)  = Just $ NamedSignature name (parameterNamesOf equations)
signatureOf (ProcedureDeclaration name equations) = Just $ NamedSignature name (parameterNamesOf equations)
signatureOf (RuleDeclaration name args _)         = Just $ AritySignature name (length args)
signatureOf (FactDeclaration name args)           = Just $ AritySignature name (length args)
signatureOf (TypeSignature name args)             = Just $ TypedSignature name args
signatureOf (VariableDeclaration name _)          = Just $ AritySignature name 0
signatureOf _                                     = Nothing

parameterNamesOf :: [Equation] -> [Maybe Binding]
parameterNamesOf = map msum . transpose . map (map parameterNameOf . equationParams)

parameterNameOf :: Pattern -> Maybe Binding
parameterNameOf (VariablePattern v) = Just v
parameterNameOf _                   = Nothing

spec :: Spec
spec = do
  describe "unhandled declaration" $ do
    it "object declaration" $ do
      signaturesOf (js "var x = {}") `shouldBe` []

  describe "TypedSignature" $ do
    it "simple variable type declaration" $ do
      signaturesOf (hs "foo :: Int") `shouldBe` [TypedSignature "foo" ["Int"]]

    it "simple function type declaration" $ do
      signaturesOf (hs "foo :: Int -> Int") `shouldBe` [TypedSignature "foo" ["Int", "Int"]]

    it "simple function tuple declaration" $ do
      signaturesOf (hs "foo :: b -> (Int, [a])") `shouldBe` [TypedSignature "foo" ["b", "(Int, [a])"]]

  describe "NamedSignature" $ do
    it "empty expression" $ do
      signaturesOf (js "") `shouldBe` []

    it "variable" $ do
      signaturesOf (js "var x = 3; var z = 2;") `shouldBe` [
                                                    AritySignature "x" 0,
                                                    AritySignature "z" 0]

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


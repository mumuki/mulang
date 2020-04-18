module HaskellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast hiding (Equal, NotEqual)
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Operators.Haskell
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "tokens table" $ do

    it "excludes some c-like operators" $ do
      let operators = Map.keys haskellTokensTable

      elem Modulo operators `shouldBe` True
      elem BitwiseOr operators `shouldBe` False
      elem BitwiseAnd operators `shouldBe` False
      elem BitwiseLeftShift operators `shouldBe` False
      elem BitwiseRightShift operators `shouldBe` False

  describe "parse" $ do
    it "parses literal character patterns" $ do
      hs "f 'a' = 1" `shouldBe` (Function "f" [Equation [LiteralPattern "'a'"] (UnguardedBody (Return (MuNumber 1.0)))])

    it "parses literal string patterns" $ do
      hs "f \"hello world\" = 1" `shouldBe` (Function "f" [Equation [LiteralPattern "\"hello world\""] (UnguardedBody (Return (MuNumber 1.0)))])

    it "parses literal number patterns" $ do
      hs "f 1 = 1" `shouldBe` (Function "f" [Equation [LiteralPattern "1"] (UnguardedBody (Return (MuNumber 1.0)))])

    it "parses left infix partial application" $ do
      hs "f = (1+)" `shouldBe` Variable "f" (Application (Primitive Plus) [MuNumber 1.0])

    it "parses right infix partial application" $ do
      hs "f = (+1)" `shouldBe` Variable "f" (Application (Reference "flip") [Primitive Plus, MuNumber 1.0])

    it "parses type restrictions" $ do
      hs "f :: Num a => [a] -> [a]" `shouldBe` SubroutineSignature "f" ["[a]"] "[a]" ["Num a"]

    it "parses multiple type restrictions" $ do
      hs "f :: (Num a, Eq b) => [a] -> [b]" `shouldBe` SubroutineSignature "f" ["[a]"] "[b]" ["Num a", "Eq b"]

    it "parses signatures without type restrictions" $ do
      hs "f :: [a] -> [a]" `shouldBe` SubroutineSignature "f" ["[a]"] "[a]" []

    it "parses type alias" $ do
      hs "type String = [Char]" `shouldBe` TypeAlias "String" "[Char]"

    it "parses type alias with arguments" $ do
      hs "type List a = [a]" `shouldBe` TypeAlias "List a" "[a]"

    it "parses inline type annotations" $ do
      hs "x = 1 :: Int" `shouldBe` Variable "x" (TypeCast (MuNumber 1) (SimpleType "Int" []))

    it "parses inline type annotations with restrictions" $ do
      hs "x = 1 :: (Num a, Foldable t) => t a" `shouldBe` Variable "x" (TypeCast (MuNumber 1) (SimpleType "t a" ["Num a", "Foldable t"]))

    it "parses chars and single char strings differently" $ do
      hs "x = \"a\"" `shouldNotBe` hs "x = 'a'"

    it "parses chars as MuChars" $ do
      hs "x = 'a'" `shouldBe` Variable "x" (MuChar 'a')

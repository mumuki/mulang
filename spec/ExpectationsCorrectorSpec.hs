{-# LANGUAGE TupleSections #-}


module ExpectationsCorrectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer hiding (spec)
import           Language.Mulang.Ast (PrimitiveOperator (..))

import Data.Tuple (swap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
type Target = String
type Inspection = String
type Token = String

type TokensTable = Map PrimitiveOperator [Token]
type OperatorsTable = Map Token PrimitiveOperator

tokensTable :: Language -> TokensTable
tokensTable Haskell = Map.fromList [
    (Equal, ["=="]),
    (NotEqual, ["/="]),
    (Negation, ["not"]),
    (And, ["&&"]),
    (Or, ["||"]),
    (Hash, []),
    (GreatherOrEqualThan, [">="]),
    (GreatherThan, [">"]),
    (LessOrEqualThan, ["<="]),
    (LessThan, ["<"]),
    (Otherwise, ["otherwise"]),
    (ForwardComposition, []),
    (BackwardComposition, ["."])
  ]
tokensTable Java = Map.fromList [
    (Equal, ["=="]),
    (NotEqual, ["!="]),
    (Negation, ["!"]),
    (And, ["&&"]),
    (Or, ["||"]),
    (Hash, ["hashCode"]),
    (GreatherOrEqualThan, [">="]),
    (GreatherThan, [">"]),
    (LessOrEqualThan, ["<="]),
    (LessThan, ["<"]),
    (Otherwise, []),
    (ForwardComposition, []),
    (BackwardComposition, [])
  ]
tokensTable Ruby = Map.fromList [
    (Equal, ["=="]),
    (NotEqual, ["!="]),
    (Negation, ["!"]),
    (And, ["&&", "and"]),
    (Or, ["||", "or"]),
    (Hash, ["hash"]),
    (GreatherOrEqualThan, [">="]),
    (GreatherThan, [">"]),
    (LessOrEqualThan, ["<="]),
    (LessThan, ["<"]),
    (Otherwise, []),
    (ForwardComposition, [">>"]),
    (BackwardComposition, ["<<"])
  ]
tokensTable Python = Map.fromList [
    (Equal, ["=="]),
    (NotEqual, ["!=", "<>"]),
    (Negation, ["not"]),
    (And, ["and"]),
    (Or, ["or"]),
    (Hash, ["hash"]),
    (GreatherOrEqualThan, [">="]),
    (GreatherThan, [">"]),
    (LessOrEqualThan, ["<="]),
    (LessThan, ["<"]),
    (Otherwise, []),
    (ForwardComposition, [">>"]),
    (BackwardComposition, ["<<"])
  ]

operatorsTable :: Language -> OperatorsTable
operatorsTable =  Map.fromList . concatMap (fill . swap) . Map.toList . tokensTable

fill (xs, t) = map (,t) xs

run :: Language -> Target -> Maybe Inspection

run Haskell "type"      = Just "DeclaresTypeAlias"
run Java    "if"        = Just "UsesIf"
run Java    "class"     = Just "DeclaresClass"
run Java    "interface" = Just "DeclaresInterface"
run Java    "for"       = Just "UsesForLoop"
run Python  "def"       = Just "DeclaresComputation"
run Ruby    "class"     = Just "DeclaresClass"
run Ruby    "include"   = Just  "Includes"
run Ruby    "def"       = Just "DeclaresComputation"
run language  target    = fmap (("Uses" ++) . show) . (Map.lookup target) . operatorsTable $ language

spec :: Spec
spec = do
  describe "correct primitive usages" $ do
    it "corrects haskell and" $ do
      run Haskell "and" `shouldBe` Nothing
      run Haskell "&&"  `shouldBe` (Just "UsesAnd")

    it "corrects haskell or" $ do
      run Haskell "or" `shouldBe` Nothing
      run Haskell "||" `shouldBe` (Just "UsesOr")

    it "corrects haskell not" $ do
      run Haskell "not" `shouldBe` (Just "UsesNegation")
      run Haskell "!"   `shouldBe`  Nothing

    it "corrects java and" $ do
      run Java    "and" `shouldBe` Nothing
      run Java    "&&"  `shouldBe` (Just "UsesAnd")

    it "corrects python and" $ do
      run Python  "and" `shouldBe` (Just "UsesAnd")
      run Python  "&&"  `shouldBe` Nothing

    it "corrects ruby and" $ do
      run Ruby    "and" `shouldBe` (Just "UsesAnd")
      run Ruby    "&&"  `shouldBe` (Just "UsesAnd")

  describe "corrects keyword usages" $ do
    it "corrects haskell type" $ do
      run Haskell "type" `shouldBe` (Just "DeclaresTypeAlias")

    it "corrects java if" $ do
      run Java    "if"     `shouldBe` (Just  "UsesIf")

    it "corrects java class" $ do
      run Java    "class"  `shouldBe` (Just "DeclaresClass")

    it "corrects java interface" $ do
      run Java    "interface"  `shouldBe` (Just "DeclaresInterface")

    it "corrects java for" $ do
      run Java    "for"  `shouldBe` (Just "UsesForLoop")

    it "corrects python def" $ do
      run Python  "def" `shouldBe` (Just "DeclaresComputation")

    it "corrects ruby class" $ do
      run Ruby    "class"  `shouldBe` (Just "DeclaresClass")

    it "corrects ruby include" $ do
      run Ruby    "include"  `shouldBe` (Just "Includes")

    it "corrects ruby def" $ do
      run Ruby  "def" `shouldBe` (Just "DeclaresComputation")

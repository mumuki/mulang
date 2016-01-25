{-# LANGUAGE OverloadedStrings #-}

module InspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Parsers.Haskell
import           Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "hasTypeSignature" $ do
    it "is True whn type signature is present" $ do
      hasTypeSignature "x" "x :: Int\n\
                           \x = 3" `shouldBe` True

    it "is False whn type signature is absent " $ do
      hasTypeSignature "x" "x = 2" `shouldBe` False

  describe "hasFunctionDeclaration" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        hasFunctionDeclaration "f"  "f x = 1" `shouldBe` True

      it "is False when functions is not declared" $ do
        hasFunctionDeclaration "g"  "f x = 1" `shouldBe` False

    describe "with constants" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        hasFunctionDeclaration "f"  "f = 2" `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        hasFunctionDeclaration "f"  "f = \\x -> x + 1" `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        hasFunctionDeclaration "f"  "f = 3" `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        hasFunctionDeclaration "f"  "f = []" `shouldBe` False

      it "is True when constant is declared with a variable literal" $ do
        hasFunctionDeclaration "f"  "f = snd" `shouldBe` True

  describe "hasArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        hasArity 1 "f" "f x = x + 1" `shouldBe` True

      it "is False when function is declared with another arity" $ do
        hasArity 2 "f" "f x = x + 1" `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        hasArity 2 "f" "f = \\x y -> x + y" `shouldBe` True

      it "is False when constant is declared with lambda of given arity" $ do
        hasArity 3 "f" "f = \\x y -> x + y" `shouldBe` False

      it "is False if it is a variable" $ do
        hasArity 1 "f" "f = snd" `shouldBe` False


  describe "hasBinding" $ do
    describe "with constants" $ do
      it "is True when binding exists" $ do
        hasBinding "x"  "x = 1" `shouldBe` True

      it "is False when binding doesnt exists" $ do
        hasBinding "y"  "x = 1" `shouldBe` False

    describe "with functions" $ do
      it "is True when binding exists" $ do
        hasBinding "x"  "x m = 1" `shouldBe` True

      it "is False when binding doesnt exists" $ do
        hasBinding "y"  "x m = 1" `shouldBe` False

  describe "hasComprehension" $ do
    it "is True when comprehension exists" $ do
      hasComprehension "x"  "x = [m|m<-t]" `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      hasComprehension "y"  "x = []" `shouldBe` False

  describe "hasUsage" $ do
    it "is True when required function is used on application" $ do
      hasUsage "m" "y"  "y x = m x" `shouldBe` True

    it "is True when required function is used as argument" $ do
      hasUsage "m" "y"  "y x = x m" `shouldBe` True

    it "is True when required function is used as operator" $ do
      hasUsage "&&" "y"  "y x = x && z" `shouldBe` True

    it "is False when required function is not used in constant" $ do
      hasUsage "m" "y"  "y = 3" `shouldBe` False

    it "is False when required function is not used in function" $ do
      hasUsage "m" "y"  "y = x 3" `shouldBe` False

    it "is False when binding is not present" $ do
      hasUsage "m" "y"  "z = m 3" `shouldBe` False

    it "is False when required function is blank" $ do
      hasUsage "" "y"  "y = m 3" `shouldBe` False

    it "is False when not present in enum" $ do
      hasUsage "h" "y"  "y = [a..b]" `shouldBe` False

    it "is True when is present in enum" $ do
      hasUsage "h" "y"  "y = [a..h]" `shouldBe` True

    it "is True when required constructor is used on application" $ do
      hasUsage "Foo" "y" "y x = Foo x" `shouldBe` True

    it "is False when required constructor is not used on application" $ do
      hasUsage "Foo" "y" "y x = Bar x" `shouldBe` False

    it "is True when required function is used on list comprehension" $ do
      hasUsage "f" "y" "y x = [ f m | m <- ms  ]" `shouldBe` True

    it "is False when required function is not used on list comprehension" $ do
      hasUsage "f" "y" "y x = [ g m | m <- ms  ]" `shouldBe` False

    it "is False when there is variable hiding in list comprehension" $ do
      --hasUsage "m" "y" "y x = [ g m | m <- ms  ]" `shouldBe` False
      pending

    it "is False when there is variable hiding in list comprehension generator" $ do
      hasUsage "m" "y" "y x = [ g x | m <- ms, x <- f m]" `shouldBe` False


  describe "hasDirectRecursion" $ do
    it "is True when has direct recursion in unguarded expresion" $ do
      hasDirectRecursion "y"  "y x = y x" `shouldBe` True

    it "is True when has direct recursion in guarded expresion" $ do
      hasDirectRecursion "y"  "y x | c x = y m\n\
                              \    | otherwise = 0" `shouldBe` True

    it "is False when there is no recursion" $ do
      hasDirectRecursion "y" "y = 3" `shouldBe` False

  describe "hasComposition" $ do
    describe "when constant assignment" $ do
      it "is True when composition is present on top level" $ do
        hasComposition "x" "x = y . z" `shouldBe` True

      it "is True when composition is present inside lambda" $ do
        hasComposition "x" "x = \\m -> y . z" `shouldBe` True

      it "is True when composition is present inside application" $ do
        hasComposition "x" "x = f (g.h) x" `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition "x" "x = 1" `shouldBe` False

    describe "when unguarded function" $ do
      it "is True when composition is present on top level" $ do
        hasComposition "f" "f x = (g . f) x" `shouldBe` True

      it "is True when composition is present within if" $ do
        hasComposition "f" "f x = if True then (g . f) x else 5" `shouldBe` True

      it "is True when composition is present within list" $ do
        hasComposition "f" "f x = [(g.h x), m]" `shouldBe` True

      it "is True when composition is present within comprehension" $ do
        hasComposition "f" "f x = [ (g.h x) m | m <- [1..20]]" `shouldBe` True

      it "is True when composition is present within where" $ do
        hasComposition "f" "f x = m\n\
                           \      where m = (f.g) " `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition "f" "f x = g x" `shouldBe` False

    describe "when guarded function " $ do
      it "is True when composition is present on top level" $ do
        hasComposition "f" "f x | c x = g . f $ x\n\
                           \    | otherwise = 4" `shouldBe` True

      it "is True when composition is present on guard" $ do
        hasComposition "f" "f x | (c . g) x = g x\n\
                           \    | otherwise = 4" `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition "f" "f x | c x = f x\n\
                           \    | otherwise = 4" `shouldBe` False

  describe "hasGuards" $ do
    describe "detects guards when" $ do
      it "is present" $ do
        hasGuards "f" "f x | c x = 2\n\
                      \    | otherwise = 4"  `shouldBe` True

      it "is present" $ do
        hasGuards "f" "f x = c x == 2"  `shouldBe` False

  describe "hasIf" $ do
    it "is True when present" $ do
      hasIf "f" "f x = if c x then 2 else 3"  `shouldBe` True

    it "is False when not present" $ do
      hasIf "f" "f x = x"  `shouldBe` False


  describe "lambda analyzer" $ do
    describe "detects lambdas when" $ do
      it "is present" $ do
        hasLambda "f" "f x = \\y -> 4" `shouldBe` True

      it "is present" $ do
        hasLambda "f" "f x = 4" `shouldBe` False


  describe "hasAnonymousVariable" $ do
    it "is True if _ is present in paramenters" $ do
      hasAnonymousVariable "foo" "foo _ = 1" `shouldBe` True

    it "is False if _ is not present in parameters" $ do
      hasAnonymousVariable "foo" "foo x = 1" `shouldBe` False

    it "is False if _ is present only in seccond equation" $ do
      let code = fromJust . parseHaskell . unlines $ ["foo False bool = bool", "foo True _ = True"]
      hasAnonymousVariable "foo" code `shouldBe` True


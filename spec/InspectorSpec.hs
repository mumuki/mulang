module InspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.Haskell
import           Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "hasTypeSignature" $ do
    it "is True whn type signature is present" $ do
      hasTypeSignature (hs "x :: Int\n\
                           \x = 3") `shouldBe` True

    it "is False whn type signature is absent " $ do
      hasTypeSignature (hs "x = 2") `shouldBe` False

  describe "hasFunctionDeclaration" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        hasFunctionDeclaration (hs "f x = 1") `shouldBe` True

    describe "with constants" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        hasFunctionDeclaration (hs "f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        hasFunctionDeclaration (hs "f = \\x -> x + 1") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        hasFunctionDeclaration (hs "f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        hasFunctionDeclaration (hs "f = []") `shouldBe` False

      it "is False when constant is declared with a variable literal" $ do
        hasFunctionDeclaration (hs "f = snd") `shouldBe` False

  describe "hasArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        scoped (hasArity 1) "f" (hs "f x = x + 1") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        scoped (hasArity 2) "f" (hs "f x = x + 1") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        scoped (hasArity 2) "f" (hs "f = \\x y -> x + y") `shouldBe` True

      it "is False when constant is declared with lambda of given arity" $ do
        scoped (hasArity 3) "f" (hs "f = \\x y -> x + y") `shouldBe` False

      it "is False if it is a variable" $ do
        scoped (hasArity 1) "f" (hs "f = snd") `shouldBe` False


  describe "hasBinding" $ do
    describe "with constants" $ do
      it "is True when binding exists" $ do
        hasBinding "x" (hs "x = 1") `shouldBe` True

      it "is False when binding doesnt exists" $ do
        hasBinding "y" (hs "x = 1") `shouldBe` False

    describe "with functions" $ do
      it "is True when binding exists" $ do
        hasBinding "x" (hs "x m = 1") `shouldBe` True

      it "is False when binding doesnt exists" $ do
        hasBinding "y" (hs "x m = 1") `shouldBe` False

  describe "hasComprehension" $ do
    it "is True when list comprehension exists" $ do
      hasComprehension (hs "x = [m|m<-t]") `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      hasComprehension (hs "x = []") `shouldBe` False

    it "is True when do syntax is used" $ do
      hasComprehension (hs "y = do { x <- xs; return x }") `shouldBe` True

  describe "hasUsage" $ do
    it "is True when required function is used on application" $ do
      hasUsage "m" (hs "y x = m x") `shouldBe` True

    it "is True when required function is used as argument" $ do
      hasUsage "m" (hs "y x = x m") `shouldBe` True

    it "is True when required function is used as operator" $ do
      hasUsage "&&" (hs "y x = x && z") `shouldBe` True

    it "is False when required function is not used in constant" $ do
      hasUsage "m" (hs "y = 3") `shouldBe` False

    it "is False when required function is not used in function" $ do
      hasUsage "m" (hs "y = x 3") `shouldBe` False

    it "is False when binding is not present, scoped" $ do
      scoped (hasUsage "m") "p" (hs "z = m 3") `shouldBe` False

    it "is False when required function is blank" $ do
      hasUsage "" (hs "y = m 3") `shouldBe` False

    it "is False when not present in enum" $ do
      hasUsage "h" (hs "y = [a..b]") `shouldBe` False

    it "is True when is present in enum" $ do
      hasUsage "h" (hs "y = [a..h]") `shouldBe` True

    it "is True when required constructor is used on application" $ do
      hasUsage "Foo" (hs "y x = Foo x") `shouldBe` True

    it "is False when required constructor is not used on application" $ do
      hasUsage "Foo" (hs "y x = Bar x") `shouldBe` False

    it "is True when required function is used on list comprehension" $ do
      hasUsage "f" (hs "y x = [ f m | m <- ms  ]") `shouldBe` True

    it "is False when required function is not used on list comprehension" $ do
      hasUsage "f" (hs "y x = [ g m | m <- ms  ]") `shouldBe` False

    it "is False when there is variable hiding in list comprehension" $ do
      --hasUsage "m" "y x = [ g m | m <- ms  ]") `shouldBe` False
      pending

    it "is False when there is variable hiding in list comprehension generator" $ do
      hasUsage "m" (hs "y x = [ g x | m <- ms, x <- f m]") `shouldBe` False


  describe "hasDirectRecursion" $ do
    it "is True when has direct recursion in unguarded expresion" $ do
      hasDirectRecursion (hs "y x = y x") `shouldBe` True

    it "is True when has direct recursion in guarded expresion" $ do
      hasDirectRecursion (hs "y x | c x = y m\n\
                              \    | otherwise = 0") `shouldBe` True

    it "is False when there is no recursion" $ do
      hasDirectRecursion (hs "y = 3") `shouldBe` False

    it "is False when there is no recursion, scoped" $ do
      scoped hasDirectRecursion "y" (hs "y = 3\nf x = f 4") `shouldBe` False

    it "is False when there is no recursion, scoped" $ do
      scoped hasDirectRecursion "y" (hs "y x = y 3") `shouldBe` True

  describe "hasComposition" $ do
    describe "when constant assignment" $ do
      it "is True when composition is present on top level" $ do
        hasComposition (hs "x = y . z") `shouldBe` True

      it "is True when composition is present inside lambda" $ do
        hasComposition (hs "x = \\m -> y . z") `shouldBe` True

      it "is True when composition is present inside application" $ do
        hasComposition (hs "x = f (g.h) x") `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition (hs "x = 1") `shouldBe` False

    describe "when unguarded function" $ do
      it "is True when composition is present on top level" $ do
        hasComposition (hs "f x = (g . f) x") `shouldBe` True

      it "is True when composition is present within if" $ do
        hasComposition (hs "f x = if True then (g . f) x else 5") `shouldBe` True

      it "is True when composition is present within list" $ do
        hasComposition (hs "f x = [(g.h x), m]") `shouldBe` True

      it "is True when composition is present within comprehension" $ do
        hasComposition (hs "f x = [ (g.h x) m | m <- [1..20]]") `shouldBe` True

      it "is True when composition is present within where" $ do
        hasComposition (hs "f x = m\n\
                           \      where m = (f.g) ") `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition (hs "f x = g x") `shouldBe` False

    describe "when guarded function " $ do
      it "is True when composition is present on top level" $ do
        hasComposition (hs "f x | c x = g . f $ x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is True when composition is present on guard" $ do
        hasComposition (hs "f x | (c . g) x = g x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition (hs "f x | c x = f x\n\
                           \    | otherwise = 4") `shouldBe` False

  describe "hasGuards" $ do
    describe "detects guards when" $ do
      it "is present" $ do
        hasGuards (hs "f x | c x = 2\n\
                      \    | otherwise = 4") `shouldBe` True

      it "is present" $ do
        hasGuards (hs "f x = c x == 2") `shouldBe` False

  describe "hasIf" $ do
    it "is True when present" $ do
      hasIf (hs "f x = if c x then 2 else 3") `shouldBe` True

    it "is False when not present" $ do
      hasIf (hs "f x = x") `shouldBe` False


  describe "lambda analyzer" $ do
    describe "detects lambdas when" $ do
      it "is present" $ do
        hasLambda (hs "f x = \\y -> 4") `shouldBe` True

      it "is present" $ do
        hasLambda (hs "f x = 4") `shouldBe` False


  describe "hasAnonymousVariable" $ do
    it "is True if _ is present in paramenters" $ do
      hasAnonymousVariable (hs "foo _ = 1") `shouldBe` True

    it "is False if _ is not present in parameters" $ do
      hasAnonymousVariable (hs "foo x = 1") `shouldBe` False

    it "is False if _ is present only in seccond equation" $ do
      let code = fromJust . parseHaskell . unlines $ ["foo False bool = bool", "foo True _ = True"]
      hasAnonymousVariable code `shouldBe` True


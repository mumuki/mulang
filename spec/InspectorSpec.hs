module InspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.Haskell
import           Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "declaresTypeSignature" $ do
    it "is True whn type signature is present" $ do
      declaresTypeSignature (named "x") (hs "x :: Int\n\
                           \x = 3") `shouldBe` True

    it "is False whn type signature is absent " $ do
      declaresTypeSignature (named "x") (hs "x = 2") `shouldBe` False

  describe "declaresFunction" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        declaresFunction (named "f") (hs "f x = 1") `shouldBe` True

    describe "with constants" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction (named "f") (hs "f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        declaresFunction (named "f") (hs "f = \\x -> x + 1") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        declaresFunction (named "f") (hs "f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        declaresFunction (named "f") (hs "f = []") `shouldBe` False

      it "is False when constant is declared with a variable literal" $ do
        declaresFunction (named "f") (hs "f = snd") `shouldBe` False

  describe "declaresWithArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresWithArity 1) (named "f") (hs "f x = x + 1") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresWithArity 2) (named "f") (hs "f x = x + 1") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresWithArity 2) (named "f") (hs "f = \\x y -> x + y") `shouldBe` True

      it "is False when constant is declared with lambda of given arity" $ do
        (declaresWithArity 3) (named "f") (hs "f = \\x y -> x + y") `shouldBe` False

      it "is False if it is a variable" $ do
        (declaresWithArity 1) (named "f") (hs "f = snd") `shouldBe` False


  describe "declares" $ do
    describe "with constants" $ do
      it "is True when binding exists" $ do
        declares (named "x") (hs "x = 1") `shouldBe` True

      it "is False when binding doesnt exists" $ do
        declares (named "y") (hs "x = 1") `shouldBe` False

    describe "with functions" $ do
      it "is True when binding exists" $ do
        declares (named "x") (hs "x m = 1") `shouldBe` True

      it "is False when binding doesnt exists" $ do
        declares (named "y") (hs "x m = 1") `shouldBe` False

  describe "usesComprehension" $ do
    it "is True when list comprehension exists" $ do
      usesComprehension (hs "x = [m|m<-t]") `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      usesComprehension (hs "x = []") `shouldBe` False

    it "is True when do syntax is used" $ do
      usesComprehension (hs "y = do { x <- xs; return x }") `shouldBe` True

  describe "parses" $ do
    it "is True when similar" $ do
      parses hs "x = map f . map g" (hs "x = map f.map g") `shouldBe` True

    it "is False when differ" $ do
      parses hs "x = map g . map f" (hs "x = map f . map g") `shouldBe` False

  describe "uses" $ do
    it "is True when required function is used on application" $ do
      uses (named "m") (hs "y x = m x") `shouldBe` True

    it "is True when required function is used as argument" $ do
      uses (named "m") (hs "y x = x m") `shouldBe` True

    it "is True when required function is used as operator" $ do
      uses (named "&&" )(hs "y x = x && z") `shouldBe` True

    it "is False when required function is not used in constant" $ do
      uses (named "m") (hs "y = 3") `shouldBe` False

    it "is False when required function is not used in function" $ do
      uses (named "m") (hs "y = x 3") `shouldBe` False

    it "is False when binding is not present, scoped" $ do
      scoped (uses (named "m")) "p" (hs "z = m 3") `shouldBe` False

    it "is False when required function is blank" $ do
      uses (named "" )(hs "y = m 3") `shouldBe` False

    it "is False when not present in enum" $ do
      uses (named "h") (hs "y = [a..b]") `shouldBe` False

    it "is True when is present in enum" $ do
      uses (named "h") (hs "y = [a..h]") `shouldBe` True

    it "is True when required constructor is used on application" $ do
      uses (named "Foo") (hs "y x = Foo x") `shouldBe` True

    it "is False when required constructor is not used on application" $ do
      uses (named "Foo") (hs "y x = Bar x") `shouldBe` False

    it "is True when required function is used on list comprehension" $ do
      uses (named "f") (hs "y x = [ f m | m <- ms  ]") `shouldBe` True

    it "is False when required function is not used on list comprehension" $ do
      uses (named "f") (hs "y x = [ g m | m <- ms  ]") `shouldBe` False

    it "is False when there is variable hiding in list comprehension" $ do
      --uses (named "m") "y x = [ g m | m <- ms  ]") `shouldBe` False
      pending

    it "is False when there is variable hiding in list comprehension generator" $ do
      uses (named "m") (hs "y x = [ g x | m <- ms, x <- f m]") `shouldBe` False


  describe "declaresRecursively" $ do
    it "is True when has direct recursion in unguarded expresion" $ do
      declaresRecursively (named "y") (hs "y x = y x") `shouldBe` True

    it "is True when has direct recursion in guarded expresion" $ do
      declaresRecursively (named "y") (hs "y x | c x = y m\n\
                              \    | otherwise = 0") `shouldBe` True

    it "is False when there is no named recursion" $ do
      declaresRecursively (named "y") (hs "y = 3") `shouldBe` False

    it "is False when there is no named recursion, scoped" $ do
      declaresRecursively (named "y") (hs "y = 3\nf x = f 4") `shouldBe` False

    it "is True when there is any recursion" $ do
      declaresRecursively anyone (hs "y x = y 3") `shouldBe` True

    it "is False when there is no recursion" $ do
      declaresRecursively anyone (hs "y x = 3") `shouldBe` False

  describe "usesComposition" $ do
    describe "when constant assignment" $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "x = y . z") `shouldBe` True

      it "is True when composition is present inside lambda" $ do
        usesComposition (hs "x = \\m -> y . z") `shouldBe` True

      it "is True when composition is present inside application" $ do
        usesComposition (hs "x = f (g.h) x") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "x = 1") `shouldBe` False

    describe "when unguarded function" $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "f x = (g . f) x") `shouldBe` True

      it "is True when composition is present within if" $ do
        usesComposition (hs "f x = if True then (g . f) x else 5") `shouldBe` True

      it "is True when composition is present within list" $ do
        usesComposition (hs "f x = [(g.h x), m]") `shouldBe` True

      it "is True when composition is present within comprehension" $ do
        usesComposition (hs "f x = [ (g.h x) m | m <- [1..20]]") `shouldBe` True

      it "is True when composition is present within where" $ do
        usesComposition (hs "f x = m\n\
                           \      where m = (f.g) ") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "f x = g x") `shouldBe` False

    describe "when guarded function " $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "f x | c x = g . f $ x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is True when composition is present on guard" $ do
        usesComposition (hs "f x | (c . g) x = g x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "f x | c x = f x\n\
                           \    | otherwise = 4") `shouldBe` False

  describe "usesGuards" $ do
    describe "detects guards when" $ do
      it "is present" $ do
        usesGuards (hs "f x | c x = 2\n\
                      \    | otherwise = 4") `shouldBe` True

      it "is present" $ do
        usesGuards (hs "f x = c x == 2") `shouldBe` False

  describe "usesIf" $ do
    it "is True when present" $ do
      usesIf (hs "f x = if c x then 2 else 3") `shouldBe` True

    it "is False when not present" $ do
      usesIf (hs "f x = x") `shouldBe` False


  describe "lambda analyzer" $ do
    describe "detects lambdas when" $ do
      it "is present" $ do
        usesLambda (hs "f x = \\y -> 4") `shouldBe` True

      it "is present" $ do
        usesLambda (hs "f x = 4") `shouldBe` False


  describe "usesAnnonymousVariable" $ do
    it "is True if _ is present in paramenters" $ do
      usesAnnonymousVariable (hs "foo _ = 1") `shouldBe` True

    it "is False if _ is not present in parameters" $ do
      usesAnnonymousVariable (hs "foo x = 1") `shouldBe` False

    it "is False if _ is present only in seccond equation" $ do
      let code = fromJust . parseHaskell . unlines $ ["foo False bool = bool", "foo True _ = True"]
      usesAnnonymousVariable code `shouldBe` True


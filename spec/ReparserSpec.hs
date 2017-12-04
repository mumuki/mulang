{-# LANGUAGE RankNTypes #-}

module ReparserSpec (spec) where

import           Test.Hspec
import           Text.Reparser

spec :: Spec
spec = do
  describe "reparse" $ do
    it "`return[bar]`" $ do
      reparse "return[bar]" `shouldBe` Nothing

    it "``" $ do
      reparse "" `shouldBe` Nothing

    it "`[]`" $ do
      reparse "[]" `shouldBe` Nothing

    it "`[][]`" $ do
      reparse "[][]" `shouldBe` Nothing

    it "works with letters between brackets" $ do
      reparse "[hello]" `shouldBe` Nothing

    it "works with uncloded bracket" $ do
      reparse "[" `shouldBe` Just (SyntaxError 1 1 UnclosedBracket)

    it "works with unclosed bracket and streams" $ do
      reparse "[hello" `shouldBe` Just (SyntaxError 1 1 UnclosedBracket)

    it "`return[bar`" $ do
      reparse "return[bar" `shouldBe` Just (SyntaxError 1 7 UnclosedBracket)

    it "`[]]`" $ do
      reparse "[]]" `shouldBe` Just (SyntaxError 1 3 UnopenBracket)

    it "works with many trivial unbalanced brackets, on first pair" $ do
      reparse "[[]" `shouldBe` Just (SyntaxError 1 1 UnclosedBracket)

    it "works with mixed streams and brackets" $ do
      reparse "hello[world[hello]world" `shouldBe` Just (SyntaxError 1 6 UnclosedBracket)

    it "works with nested streams and brackets" $ do
      reparse "hello[world[hello]world]" `shouldBe` Nothing

    it "`hello[world[hello[world]]`" $ do
      reparse "hello[world[hello[world]]" `shouldBe` Just (SyntaxError 1 6 UnclosedBracket)

    it "`hello[world]hello[world]`" $ do
      reparse "hello[world]hello[world]" `shouldBe` Nothing

    it "`return(bar)`" $ do
      reparse "return(bar)" `shouldBe` Nothing

    it "`()`" $ do
      reparse "()" `shouldBe` Nothing

    it "`()()`" $ do
      reparse "()()" `shouldBe` Nothing

    it "works with letters between brackets" $ do
      reparse "(hello)" `shouldBe` Nothing

    it "(" $ do
      reparse "(" `shouldBe` Just (SyntaxError 1 1 UnclosedParen)

    it "works with unclosed bracket and streams" $ do
      reparse "(hello" `shouldBe` Just (SyntaxError 1 1 UnclosedParen)

    it "`return(bar`" $ do
      reparse "return(bar" `shouldBe` Just (SyntaxError 1 7 UnclosedParen)

    it "`)`" $ do
      reparse ")" `shouldBe` Just (SyntaxError 1 1 UnopenParen)

    it "works with many trivial unbalanced brackets, on first pair" $ do
      reparse "(()" `shouldBe` Just (SyntaxError 1 1 UnclosedParen)

    it "works with mixed streams and brackets" $ do
      reparse "hello(world(hello)world" `shouldBe` Just (SyntaxError 1 6 UnclosedParen)

    it "works with nested streams and brackets" $ do
      reparse "hello(world(hello)world)" `shouldBe` Nothing

    it "`hello(world(hello(world)`" $ do
      reparse "hello(world(hello(world)" `shouldBe` Just (SyntaxError 1 12 UnclosedParen)

    it "`hello(world(hello(world`" $ do
      reparse "hello(world(hello(world" `shouldBe` Just (SyntaxError 1 18 UnclosedParen)

    it "`hello(world)hello(world)`" $ do
      reparse "hello(world)hello(world)" `shouldBe` Nothing

    it "`([])`" $ do
      reparse "([])" `shouldBe` Nothing

    it "`{[(hello)]}`" $ do
      reparse "{[(hello)]}" `shouldBe` Nothing

    it "`(]`" $ do
      reparse "(]" `shouldBe` Just (SyntaxError 1 1 UnclosedParen)

    it "`{ ( { ( []{} ) } }`" $ do
      reparse "{ ( { ( []{} ) } }" `shouldBe` Just (SyntaxError 1 3 UnclosedParen)

    it "`{ ( { ( ) } }`" $ do
      reparse "{ ( { ( ) } }" `shouldBe` Just (SyntaxError 1 3 UnclosedParen)

    it "`{ ( { } }`" $ do
      reparse "{ ( { } }" `shouldBe` Just (SyntaxError 1 3 UnclosedParen)

    it "`{(}`" $ do
      reparse "{(}" `shouldBe` Just (SyntaxError 1 2 UnclosedParen)

    it "`{[}`" $ do
      reparse "{[}" `shouldBe` Just (SyntaxError 1 2 UnclosedBracket)

    it "`[{]`" $ do
      reparse "[{]" `shouldBe` Just (SyntaxError 1 2 UnclosedBrace)

    it "`[{`" $ do
      reparse "[{" `shouldBe` Just (SyntaxError 1 2 UnclosedBrace)

    it "`[}]`" $ do
      reparse "[}]" `shouldBe` Just (SyntaxError 1 1 UnclosedBracket)

    it "`( ( )`" $ do
      reparse "( ( )" `shouldBe` Just (SyntaxError 1 1 UnclosedParen)

    it "`'{'`" $ do
      reparse "'{'" `shouldBe` Nothing

    it "`('{')`" $ do
      reparse "('{')" `shouldBe` Nothing

    it "`\"{\"`" $ do
      reparse "\"{\"" `shouldBe` Nothing

    it "`(\"{\")`" $ do
      reparse "(\"{\")" `shouldBe` Nothing

    it "`public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}\n}`" $ do
      reparse "public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}\n}" `shouldBe` Nothing

    it "`public class Main {\npublic static void main(String[] args) \nSystem.out.println(new int[]{1, 2});\n}\n}`" $ do
      reparse "public class Main {\npublic static void main(String[] args) \nSystem.out.println(new int[]{1, 2});\n}\n}" `shouldBe` Just (SyntaxError 5 1 UnopenBrace)

    it "`public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}`" $ do
      reparse "public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}" `shouldBe` Just (SyntaxError 1 19 UnclosedBrace)

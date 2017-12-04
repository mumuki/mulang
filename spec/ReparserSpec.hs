{-# LANGUAGE RankNTypes #-}

module ReparserSpec (spec) where

import           Test.Hspec

import Text.Parsec
import Control.Fallible (orFail)
import Control.Monad (msum, join)
import Data.Maybe (isJust)

type ParsecParser a = forall x . Parsec String x a

data SyntaxError
  = SyntaxError { line :: Int, column :: Int, kind :: SyntaxErrorKind } deriving (Show, Eq)

data SyntaxErrorKind
  = UnclosedBracket
  | UnclosedParen
  | UnclosedBrace
  | UnopenBracket
  | UnopenParen
  | UnopenBrace
  | BrokenQuote
  | BrokenDoubleQuote
  | UnopenComment
  | UnclosedComment
  | NonAsciiChar
  deriving (Show, Eq)

lparen, rparen, rbrace, lbrace, lbracket, rbracket :: ParsecParser Char
lparen = char '('
rparen = char ')'
lbrace = char '{'
rbrace = char '}'
lbracket = char '['
rbracket = char ']'

type Reparser = ParsecParser (Maybe SyntaxError)

reparse :: String -> Maybe SyntaxError
reparse = orFail . parse program ""

program :: Reparser
program = strip False

strip :: Bool -> Reparser
strip nested = fmap msum $ many (choice choices)
  where choices | nested = commonParsers
                | otherwise = commonParsers ++ withModes unopen

        commonParsers = stream : withModes open
        withModes f = map f [bracketMode, parenMode, braceMode]

data Mode = Mode {
  leftParser :: ParsecParser Char,
  rightParser :: ParsecParser Char,
  unopenKind :: SyntaxErrorKind,
  unclosedKind :: SyntaxErrorKind
}

parenMode   = Mode lparen rparen UnopenParen UnclosedParen
bracketMode = Mode lbracket rbracket UnopenBracket UnclosedBracket
braceMode   = Mode lbrace rbrace UnopenBrace UnclosedBrace

open :: Mode -> Reparser
open mode = do
  position <- getPosition
  nested <- startNesting mode
  if isJust nested
    then return nested
    else endNesting mode position

unopen :: Mode -> Reparser
unopen mode = do
  position <- getPosition
  rightParser mode
  returnSyntaxError position (unopenKind mode)

startNesting mode = leftParser mode *> (fmap join . optionMaybe $ strip True)

endNesting mode position = closed mode <|> unclosed mode position

closed mode = rightParser mode *> return Nothing

unclosed mode position = returnSyntaxError position (unclosedKind mode)

stream :: Reparser
stream = many1 (noneOf "{}[]()") *> return Nothing

returnSyntaxError position = return . Just . SyntaxError (sourceLine position) (sourceColumn position)

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

    it "`public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}\n}`" $ do
      reparse "public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}\n}" `shouldBe` Nothing

    it "`public class Main {\npublic static void main(String[] args) \nSystem.out.println(new int[]{1, 2});\n}\n}`" $ do
      reparse "public class Main {\npublic static void main(String[] args) \nSystem.out.println(new int[]{1, 2});\n}\n}" `shouldBe` Just (SyntaxError 5 1 UnopenBrace)

    it "`public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}`" $ do
      reparse "public class Main {\npublic static void main(String[] args) {\nSystem.out.println(new int[]{1, 2});\n}" `shouldBe` Just (SyntaxError 1 19 UnclosedBrace)

{-# LANGUAGE RankNTypes #-}

module ReparserSpec (spec) where

import           Test.Hspec

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Numbers
import Control.Fallible (orFail)

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

reparse :: String -> [SyntaxError]
reparse = orFail . parse (program <* eof) ""

program :: ParsecParser [SyntaxError]
program = strip False

strip :: Bool -> ParsecParser [SyntaxError]
strip nested = fmap concat $ many (choice choices)
  where choices | nested = commonParsers
                | otherwise = commonParsers ++ [unopen bracketMode, unopen parenMode, unopen braceMode]

        commonParsers = [stream, open bracketMode, open parenMode, open braceMode]

data Mode = Mode { l :: ParsecParser Char, r :: ParsecParser Char, uo :: SyntaxErrorKind, uc :: SyntaxErrorKind }

parenMode = Mode lparen rparen UnopenParen UnclosedParen
bracketMode = Mode lbracket rbracket UnopenBracket UnclosedBracket
braceMode = Mode lbrace rbrace UnopenBrace UnclosedBrace

open mode = do
  pos <- getPosition
  l mode
  optionMaybe (strip True)
  choice [end mode, unclosed mode pos]

end mode = do
  r mode
  return []

unopen mode = do
  pos <- getPosition
  r mode
  return [SyntaxError (sourceLine pos) (sourceColumn pos) (uo mode)]

unclosed mode pos = do
  return [SyntaxError (sourceLine pos) (sourceColumn pos) (uc mode)]

stream = do
  many1 letter
  return []

spec :: Spec
spec = do
  describe "reparse" $ do
    it "`return[bar]`" $ do
      reparse "return[bar]" `shouldBe` []

    it "``" $ do
      reparse "" `shouldBe` []

    it "`[]`" $ do
      reparse "[]" `shouldBe` []

    it "`[][]`" $ do
      reparse "[][]" `shouldBe` []

    it "works with letters between brackets" $ do
      reparse "[hello]" `shouldBe` []

    it "works with uncloded bracket" $ do
      reparse "[" `shouldBe` [SyntaxError 1 1 UnclosedBracket]

    it "works with unclosed bracket and streams" $ do
      reparse "[hello" `shouldBe` [SyntaxError 1 1 UnclosedBracket]

    it "`return[bar`" $ do
      reparse "return[bar" `shouldBe` [SyntaxError 1 7 UnclosedBracket]

    it "`[]]`" $ do
      reparse "[]]" `shouldBe` [SyntaxError 1 3 UnopenBracket]

    it "works with many trivial unbalanced brackets, on first pair" $ do
      reparse "[[]" `shouldBe` [SyntaxError 1 1 UnclosedBracket]

    it "works with mixed streams and brackets" $ do
      reparse "hello[world[hello]world" `shouldBe` [SyntaxError 1 6 UnclosedBracket]

    it "works with nested streams and brackets" $ do
      reparse "hello[world[hello]world]" `shouldBe` []

    it "`hello[world[hello[world]]`" $ do
      reparse "hello[world[hello[world]]" `shouldBe` [SyntaxError 1 6 UnclosedBracket]

    it "`hello[world]hello[world]`" $ do
      reparse "hello[world]hello[world]" `shouldBe` []

    it "`return(bar)`" $ do
      reparse "return(bar)" `shouldBe` []

    it "`()`" $ do
      reparse "()" `shouldBe` []

    it "`()()`" $ do
      reparse "()()" `shouldBe` []

    it "works with letters between brackets" $ do
      reparse "(hello)" `shouldBe` []

    it "(" $ do
      reparse "(" `shouldBe` [SyntaxError 1 1 UnclosedParen]

    it "works with unclosed bracket and streams" $ do
      reparse "(hello" `shouldBe` [SyntaxError 1 1 UnclosedParen]

    it "`return(bar`" $ do
      reparse "return(bar" `shouldBe` [SyntaxError 1 7 UnclosedParen]

    it "`)`" $ do
      reparse ")" `shouldBe` [SyntaxError 1 1 UnopenParen]

    it "works with many trivial unbalanced brackets, on first pair" $ do
      reparse "(()" `shouldBe` [SyntaxError 1 1 UnclosedParen]

    it "works with mixed streams and brackets" $ do
      reparse "hello(world(hello)world" `shouldBe` [SyntaxError 1 6 UnclosedParen]

    it "works with nested streams and brackets" $ do
      reparse "hello(world(hello)world)" `shouldBe` []

    it "`hello(world(hello(world)`" $ do
      reparse "hello(world(hello(world)" `shouldBe` [SyntaxError 1 6 UnclosedParen]

    it "`hello(world)hello(world)`" $ do
      reparse "hello(world)hello(world)" `shouldBe` []

    it "`([])`" $ do
      reparse "([])" `shouldBe` []

    it "`{[(hello)]}`" $ do
      reparse "{[(hello)]}" `shouldBe` []

    it "`(]`" $ do
      reparse "(]" `shouldBe` [SyntaxError 1 1 UnclosedParen, SyntaxError 1 2 UnopenBracket]

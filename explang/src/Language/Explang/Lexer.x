{
module Language.Explang.Lexer (Token(..),P,evalP,lexer, tokens) where
import Control.Monad.State
import Control.Monad.Error
import Data.Word
import Codec.Binary.UTF8.String as UTF8 (encode, decode)

import qualified Data.Map as Map
import Control.Monad (liftM)
import Data.List (foldl')
import Numeric (readHex, readOct)

}


-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$eol_char = [$lf $cr] -- any end of line character
$white_char   = [\ \n\r\f\v\t]
$white_no_nl = $white_char # $eol_char
$ident_letter = [a-zA-Z_]
$upper_letter = [A-Z]
$lower_letter = [a-z_]
$digit    = 0-9
$non_zero_digit = 1-9
$str_char = [^ \n \r ' \" \\]
$symbol_char = [^ \n \r ` \" \\]
$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \"

--------------------
-- Macro Definitions
--------------------

-- generic
@eol_pattern = $lf | $cr $lf | $cr $lf
@backslash_pair = \\ (\\|'|\"|\`|@eol_pattern|$str_char)

-- numbers
@int_part = $digit+
@fraction = \. $digit+
@float_number = (@int_part? @fraction) | @int_part \.

-- string
@string = ($str_char|@backslash_pair|')*

-- char
@char = $str_char|@backslash_pair|\"

-- identifier
@symbol = ($symbol_char|@backslash_pair)+

tokens :-

  $white+  ;  -- skip whitespace

  ' @char '    { mkChar }
  \" @string \"     { mkString TString }
  ` @symbol `   { mkString TSymbol }
  ("+" | "-")? @float_number { token TNumber readFloat }
  ("+" | "-")? $digit+ { token TNumber (fromIntegral.readInt) }

  "("   { symbolToken TOpenParen }
  ")"   { symbolToken TCloseParen }
  ","   { symbolToken TComma }

 -- $lower_letter($ident_letter|$digit)*("?"|"!"|"=")?  { keywordOrIdent }


  "calls" { mkInspection }
  "returns" { mkInspection }


  "and" { symbolToken TAnd }
  "any" { symbolToken TAny }
  "at" { symbolToken TAt }
  "distinct" { symbolToken TDistinct }
  "exactly" { symbolToken TExactly }
  "false" { symbolToken TFalse }
  "intransitively" { symbolToken TIntransitively }
  "least" { symbolToken TLeast }
  "like" { symbolToken TLike }
  "logic" { symbolToken TLogic }
  "math" { symbolToken TMath }
  "most" { symbolToken TMost }
  "nil" { symbolToken TNil }
  "not" { symbolToken TNot }
  "of" { symbolToken TOf }
  "self" { symbolToken TSelf }
  "something" { symbolToken TSomething }
  "that" { symbolToken TThat }
  "times" { symbolToken TTimes }
  "true" { symbolToken TTrue }
  "with" { symbolToken TWith }
  "within" { symbolToken TWithin }


{
data Token
  = TOpenParen
  | TAnd
  | TAny
  | TAt
  | TChar { charValue :: Char }
  | TCloseParen
  | TComma
  | TDistinct
  | TEOF
  | TExactly
  | TFalse
  | TInspection String
  | TIntransitively
  | TLeast
  | TLike
  | TLogic
  | TMath
  | TMost
  | TNil
  | TNot
  | TNumber { numberValue :: Double }
  | TOf
  | TSelf
  | TSomething
  | TString { stringValue :: String }
  | TSymbol { symbolValue :: String }
  | TThat
  | TTimes
  | TTrue
  | TWith
  | TWithin
  deriving (Eq,Show)

type AlexInput = [Word8]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (b:bs) = Just (b,bs)
alexGetByte []    = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

type P a = StateT AlexInput (Either String) a

evalP::P a -> AlexInput -> Either String a
evalP = evalStateT

readToken::P Token
readToken = do
    s <- get
    case alexScan s 0 of
      AlexEOF -> return TEOF
      AlexError _ -> throwError "!Lexical error"
      AlexSkip inp' _ -> do
        put inp'
        readToken
      AlexToken inp' len action -> do
        put inp'
        (action (take len (decode s)))

-- The lexer function to be passed to Happy
lexer::(Token -> P a)->P a
lexer cont = readToken >>= cont

tokens :: String -> [Token]
tokens = loop . encode
  where
    run = runStateT (lexer return)
    loop input = case run input of
                  Right (TEOF, _) -> []
                  Right (t, tail) -> t : loop tail
                  _               -> []

----------

type Action = String -> P Token

readFloat :: String -> Double
readFloat str@('.':cs) = read ('0':readFloatRest str)
readFloat str = read (readFloatRest str)
readFloatRest :: String -> String
readFloatRest [] = []
readFloatRest ['.'] = ".0"
readFloatRest (c:cs) = c : readFloatRest cs

readInt :: String -> Int
readInt ('+':s) = read s
readInt s       = read s

token :: (a -> Token) -> (String -> a) -> Action
token mkToken read str = return $ mkToken (read str)

symbolToken :: Token -> Action
symbolToken t _ = return t

mkChar :: Action
mkChar = token TChar (head.tail)

mkString :: (String -> Token) -> Action
mkString kind = token kind (\str -> drop 1 . take (length str - 1) $ str)

mkInspection :: Action
mkInspection = token TInspection id
}

{
module Language.Mulang.Edl.Lexer (Token(..),P,evalP,lexer, tokens) where
import Control.Monad.State
import Control.Monad.Except
import Data.Word
import Codec.Binary.UTF8.String as UTF8 (encode, decode)
}


-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$eol_char = [$lf $cr] -- any end of line character
$not_eol_char = ~$eol_char -- anything but an end of line character
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

-- symbol
@symbol = ($symbol_char|@backslash_pair)+

-- identifier
@identifier = $ident_letter+

tokens :-

  $white+  ;  -- skip whitespace

  "%%" ($not_eol_char)* ;  -- skip comments

  ' @char '     { mkChar }
  \" @string \" { mkString TString }
  ` @symbol `   { mkString TSymbol }
  &` @symbol `  { mkReference }
  ("+" | "-")? @float_number { token TNumber readFloat }
  ("+" | "-")? $digit+ { token TNumber (fromIntegral.readInt) }

  "("   { symbolToken TOpenParen }
  ")"   { symbolToken TCloseParen }
  ","   { symbolToken TComma }
  ";"   { symbolToken TSemi }
  ":"   { symbolToken TColon }
  "&&"  { symbolToken TCAnd }
  "||"  { symbolToken TCOr }
  "!"   { symbolToken TCNot }
  "+"   { symbolToken TPlus }
  ">="  { symbolToken TAtLeast }
  "<="  { symbolToken TAtMost }
  "="   { symbolToken TExactly }

  "and" { symbolToken TAnd }
  "anything" { symbolToken TAnything }
  "count" { symbolToken TCount }
  "except"   { symbolToken TExcept }
  "expectation" { symbolToken TExpectation }
  "false" { symbolToken TFalse }
  "in"   { symbolToken TIn }
  "like"   { symbolToken TLike }
  "literal" { symbolToken TLiteral }
  "logic" { symbolToken TLogic }
  "math" { symbolToken TMath }
  "nil" { symbolToken TNil }
  "nonliteral" { symbolToken TNonliteral }
  "not" { symbolToken TNot }
  "or" { symbolToken TOr }
  "self" { symbolToken TSelf }
  "something" { symbolToken TSomething }
  "somewhere" { symbolToken TSomewhere }
  "that" { symbolToken TThat }
  "through" { symbolToken TThrough }
  "true" { symbolToken TTrue }
  "unlike"   { symbolToken TUnlike }
  "with" { symbolToken TWith }
  "within" { symbolToken TWithin }

  @identifier { mkIdentifier }

{
data Token
  = TAnd
  | TAnything
  | TAtLeast
  | TAtMost
  | TCAnd
  | TChar { charValue :: Char }
  | TCloseParen
  | TCNot
  | TColon
  | TComma
  | TCOr
  | TCount
  | TEOF
  | TExactly
  | TExcept
  | TExpectation
  | TFalse
  | TIdentifier { identifierValue :: String }
  | TIn
  | TLeast
  | TLike
  | TLiteral
  | TLogic
  | TMath
  | TNil
  | TNonliteral
  | TNot
  | TNumber { numberValue :: Double }
  | TOpenParen
  | TOr
  | TPlus
  | TReference { referenceValue :: String }
  | TSelf
  | TSemi
  | TSomething
  | TSomewhere
  | TString { stringValue :: String }
  | TSymbol { symbolValue :: String }
  | TThat
  | TThrough
  | TTrue
  | TUnlike
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
      AlexError _ -> throwError ("Lexical error")
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
readFloat str@('.':_) = read ('0':readFloatRest str)
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
mkString kind = token kind (stripString 1)

mkIdentifier :: Action
mkIdentifier = token TIdentifier id

mkReference :: Action
mkReference = token TReference (stripString 2)

stripString n str = drop n . take (length str - 1) $ str
}

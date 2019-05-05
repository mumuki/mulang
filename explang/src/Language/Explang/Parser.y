{
module Language.Explang.Parser (parse) where

import           Language.Explang.Expectation
import           Language.Explang.Lexer (Token( ..) )
import qualified Language.Explang.Lexer as L

import           Control.Monad.Error
}

%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%name parse
%tokentype{L.Token}
%error {parseError}

%token

  and { TAnd {} }
  any { TAny {} }
  at { TAt {} }
  char { TChar {} }
  closeParen  { TCloseParen {} }
  comma { TComma {} }
  distinct { TDistinct {} }
  exactly { TExactly {} }
  false { TFalse {} }
  inspection { TInspection {} }
  intransitively { TIntransitively {} }
  least { TLeast {} }
  like { TLike {} }
  logic { TLogic {} }
  math { TMath {} }
  most { TMost {} }
  nil { TNil {} }
  not { TNot {} }
  number { TNumber {} }
  of { TOf {} }
  openParen   { TOpenParen {} }
  self { TSelf {} }
  something { TSomething {} }
  string { TString {} }
  symbol { TSymbol {} }
  that { TThat {} }
  times { TTimes {} }
  true { TTrue {} }
  with { TWith {} }
  within { TWithin {} }


%%

Expectation :: { Expectation }
Expectation : Flags Scope Negation Inspection Binding Matcher Count { Expectation $1 $2 $3 $4 $5 $6 $7 }

Flags :: { Flags }
Flags : { noFlags }
  | intransitively { intransitiveFlag }

Scope :: { Scope }
Scope : { Unscoped }
  | within symbol { (Scoped . symbolValue) $2 }

Negation :: { Bool }
Negation : { False }
 | not { True }

Inspection :: { String }
Inspection : inspection { let (TInspection i) = $1 in i }

Binding :: { Binding }
Binding : { Any }
 | symbol { (Named . symbolValue) $1 }
 | something like symbol { (Like . symbolValue) $3 }
 | something distinct of symbol { (Except . symbolValue) $4 }
 | any of openParen Symbols closeParen  { (AnyOf . map symbolValue) $4 }

Symbols :: { [Token] }
Symbols : symbol { [$1] }
 | symbol comma Symbols { ($1:$3) }

Matcher :: { Matcher }
Matcher : { Unmatching }
  | Predicates { Matching $1 }

Predicates :: { [Predicate] }
Predicates : Predicate { [$1] }
  | Predicate and Predicates { ($1:$3) }

Predicate :: { Predicate }
Predicate : with number { IsNumber . numberValue $ $2 }
  | with string { IsString . stringValue $ $2 }
  | with char { IsChar . charValue $ $2 }
  | with symbol { IsSymbol . symbolValue $ $2 }
  | with true { IsTrue }
  | with false { IsFalse }
  | with self { IsSelf }
  | with math { IsMath }
  | with logic { IsLogic }
  | with nil { IsNil }
  | with something that openParen Expectation closeParen { That $5 }

Count :: { Count }
Count : { AnyCount }
  | at least number times { AtLeast . round . numberValue $ $3 }
  | at most number times { AtMost . round . numberValue $ $3 }
  | exactly number times { Exactly . round . numberValue $ $2 }

{
parseError x = throwError ("!Parse Error: " ++ show x)
}

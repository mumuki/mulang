{
module Language.Mulang.Edl.Parser (parseExpectations, parseQuery) where

import           Language.Mulang.Edl.Expectation
import           Language.Mulang.Edl.Lexer (Token( ..) )
import qualified Language.Mulang.Edl.Lexer as L

import           Control.Monad.Error
}

%name parseExpectations Expectations
%name parseQuery        TopQuery

%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%tokentype{L.Token}
%error {parseError}

%token

  and { TAnd {} }
  anything { TAnything {} }
  atLeast { TAtLeast {} }
  atMost { TAtMost {} }
  cand { TCAnd {} }
  char { TChar {} }
  closeParen  { TCloseParen {} }
  cnot { TCNot {} }
  colon { TColon {} }
  comma { TComma {} }
  cor { TCOr {} }
  count { TCount {} }
  exactly { TExactly {} }
  except { TExcept {} }
  expectation { TExpectation {} }
  false { TFalse {} }
  identifier { TIdentifier {} }
  in { TIn {} }
  like { TLike {} }
  literal { TLiteral {} }
  logic { TLogic {} }
  math { TMath {} }
  nil { TNil {} }
  nonliteral { TNonliteral {} }
  not { TNot {} }
  number { TNumber {} }
  openParen  { TOpenParen {} }
  or { TOr {} }
  plus { TPlus {} }
  self { TSelf {} }
  semi { TSemi {} }
  something { TSomething {} }
  somewhere { TSomewhere {} }
  string { TString {} }
  symbol { TSymbol {} }
  that { TThat {} }
  through { TThrough {} }
  true { TTrue {} }
  unlike { TUnlike {} }
  with { TWith {} }
  within { TWithin {} }

%left plus
%left or
%left and
%right not
%left cor
%left cand
%right cnot

%%
Expectations :: { [Expectation] }
Expectations : { [] }
  | Expectation { [$1] }
  | Expectation semi Expectations { $1:$3 }

Expectation :: { Expectation }
Expectation : expectation colon TopQuery { Expectation "" $3 }
  | expectation string colon TopQuery { Expectation (stringValue $2) $4 }

TopQuery :: { Query }
TopQuery : Query { $1 }
  | CQuery { Decontextualize $1 }

Query :: { Query }
Query : somewhere CQuery { Decontextualize $2 }
  | within symbol CQuery { Within (symbolValue $2) $3 }
  | through symbol CQuery { Through (symbolValue $2) $3 }
  | not openParen Query closeParen { Not $3 }
  | openParen Query closeParen or openParen Query closeParen { Or $2 $6 }
  | openParen Query closeParen and openParen Query closeParen { And $2 $6 }

CQuery :: { CQuery }
CQuery : openParen CQuery closeParen { $2 }
  | Consult { let (i, p, m) = $1 in Inspection i p m }
  | cnot CQuery { CNot $2 }
  | CQuery cand CQuery { CAnd $1 $3 }
  | CQuery cor CQuery { COr $1 $3 }
  | TQuery atLeast Times { AtLeast $3 $1 }
  | TQuery atMost Times { AtMost $3 $1  }
  | TQuery exactly Times { Exactly $3 $1 }

TQuery :: { TQuery }
TQuery : openParen TQuery closeParen { $2 }
  | count openParen Consult closeParen { let (i, p, m) = $3 in Counter i p m }
  | TQuery plus TQuery { Plus $1 $3 }

Times :: { Int }
Times : number { round . numberValue $ $1 }

Inspection :: { String }
Inspection : identifier { identifierValue $1 }
  | Keyword { $1 }
  | identifier Inspection { (identifierValue $1) ++ " " ++ $2 }

Keyword :: { String }
Keyword : and { "and" }
  | anything { "anything" }
  | count { "count" }
  | expectation { "expectation" }
  | false { "false" }
  | literal { "literal" }
  | logic { "logic" }
  | math { "math" }
  | nil { "nil" }
  | nonliteral { "nonliteral" }
  | not { "not" }
  | or { "or" }
  | self { "self" }
  | true { "true" }

Consult :: { (String, Predicate, Matcher) }
Consult : Inspection something Predicate Matcher { ($1, $3, $4) }
  | Inspection Predicate Matcher { ($1, $2, $3) } -- relaxed syntax

Predicate :: { Predicate }
Predicate : { Any }
 | symbol { (Named . symbolValue) $1 }
 | except symbol { (Except . symbolValue) $2 }
 | like symbol { (Like . symbolValue) $2 }
 | unlike symbol { (Unlike . symbolValue) $2 }
 | in openParen Symbols closeParen  { (AnyOf . map symbolValue) $3 }
 | except in openParen Symbols closeParen  { (NoneOf . map symbolValue) $4 }
 | like in openParen Symbols closeParen  { (LikeAnyOf . map symbolValue) $4 }
 | unlike in openParen Symbols closeParen  { (LikeNoneOf . map symbolValue) $4 }

Symbols :: { [Token] }
Symbols : symbol { [$1] }
 | symbol comma Symbols { ($1:$3) }

Matcher :: { Matcher }
Matcher : { Unmatching }
  | with Clause { Matching [$2] }
  | with openParen Clauses closeParen { Matching $3 }
  | that openParen TopQuery closeParen { Matching [That $3] } -- relaxed syntax

Clauses :: { [Clause] }
Clauses : Clause { [$1] }
  | Clause comma Clauses { ($1:$3) }

Clause :: { Clause }
Clause : number { IsNumber . numberValue $ $1 }
  | string { IsString . stringValue $ $1 }
  | char { IsChar . charValue $ $1 }
  | symbol { IsSymbol . symbolValue $ $1 }
  | anything { IsAnything }
  | false { IsFalse }
  | literal { IsLiteral }
  | logic { IsLogic }
  | math { IsMath }
  | nil { IsNil }
  | nonliteral { IsNonliteral }
  | self { IsSelf }
  | true { IsTrue }
  | something that openParen TopQuery closeParen { That $4 }
  | that openParen TopQuery closeParen { That $3 } -- relaxed syntax

{
parseError token = throwError ("Parse Error: " ++ m token)

predicateOperatorError operator = unlines [
    "Predicate operator " ++ operator ++ " is not expected here.",
    "Remember it must be used after the inspection.",
    "Valid forms are `except`, `like`, `unlike`, `in`, `except in`, `like in`, `unlike in`"
  ]


scopeOperatorError operator = unlines [
    "Scope operator " ++ operator ++ " is not expected here.",
    "Remember it must be the first part of an scoped query.",
    "For example "++ operator ++" `foo` assigns `bar`"
  ]

m (TChar v) = "char " ++ show v ++ " is not expected here"
m (TIdentifier id) = "Unexpected keyword " ++ id
m (TNumber v) = "number " ++ show v ++ " is not expected here"
m (TString v) = "string " ++ show v ++ " is not expected here"
m (TSymbol v) = "symbol " ++ v ++ " is not expected here"
m TAnd = "and is not expected here"
m TAnything = "anything is not expected here"
m TAtLeast = "least is not expected here"
m TAtMost = "most is not expected here"
m TCAnd = "&& is not expected here"
m TCloseParen = "Unexpected ("
m TCNot = "! is not expected here"
m TComma = "Unexpected ,"
m TCOr = "|| is not expected here"
m TCount = "count is not expected here"
m TEOF = "Unexpected end of file"
m TExactly = "exactly is not expected here"
m TExcept = predicateOperatorError "except"
m TFalse = "false is not expected here"
m TIn = predicateOperatorError "in"
m TLike = predicateOperatorError "like"
m TLiteral = "literal is not expected here"
m TLogic = "logic is not expected here"
m TMath = "math is not expected here"
m TNil = "nil is not expected here"
m TNonliteral = "nonliteral is not expected here"
m TNot = "not is not expected here"
m TOpenParen = "Unexpected )"
m TOr = "or is not expected here"
m TSelf = "self is not expected here"
m TSemi = "Unexpected ;"
m TSomething = "something is not expected here"
m TSomewhere = "somewhere is not expected here"
m TThat = "that is not expected here"
m TThrough = scopeOperatorError "through"
m TTrue = "true is not expected here"
m TUnlike = predicateOperatorError "unlike"
m TWith = "with is not expected here"
m TWithin = scopeOperatorError "within"
m x =  "Unexpected " ++ show x


}

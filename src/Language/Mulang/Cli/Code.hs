{-# LANGUAGE DeriveGeneric #-}

module        Language.Mulang.Cli.Code (
  parseCode,
  Code(..),
  Language(..)) where

import        GHC.Generics

import        Data.Aeson

import        Language.Mulang
import        Language.Mulang.Parsers.Json
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Prolog (parseProlog)
import        Language.Mulang.Parsers.Gobstones (parseGobstones)
import        Text.Read

data Code = Code {
  language :: Language,
  content :: String
} deriving (Show, Eq, Generic)

data Language =  Mulang
              |  Json
              |  JavaScript
              |  Prolog
              |  GobstonesAst
              |  Haskell deriving (Show, Eq, Generic)

instance FromJSON Code
instance FromJSON Language

instance ToJSON Code
instance ToJSON Language


parseCode :: Code -> Maybe Expression
parseCode (Code Mulang content)         = readMaybe content
parseCode (Code Json content)           = parseJson content
parseCode (Code Haskell content)        = parseHaskell content
parseCode (Code JavaScript content)     = parseJavaScript content
parseCode (Code Prolog content)         = parseProlog content
parseCode (Code GobstonesAst content)   = parseGobstones content
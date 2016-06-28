module Language.Mulang.Parsers.Json (json, parseJson) where

import Language.Mulang
import Language.Mulang.Parsers

import Data.Aeson (decode, ToJSON, FromJSON)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)


json :: Parser
json = fromJust . parseJson

parseJson :: MaybeParser
parseJson  = decode . LBS.pack

instance FromJSON Equation
instance ToJSON Equation

instance FromJSON EquationBody
instance ToJSON EquationBody

instance FromJSON Expression
instance ToJSON Expression

instance FromJSON Pattern
instance ToJSON Pattern

instance FromJSON ComprehensionStatement
instance ToJSON ComprehensionStatement


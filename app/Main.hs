{-# LANGUAGE DeriveGeneric, QuasiQuotes, OverloadedStrings #-}

module Main (main) where

import Language.Mulang.Analyzer (analyse)
import Language.Mulang.Analyzer.Analysis.Json ()
import Data.Aeson (eitherDecode, encode)
import System.Environment (getArgs)
import Control.Fallible (orFail)

import           Data.List (intercalate)
import           Version (prettyVersion)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LBS (putStrLn)
import qualified Data.Text.Lazy as T (pack)
import           Data.Text (unpack)
import           NeatInterpolation (text)


main :: IO ()
main = do
  argsBody <- fmap parseArgs $ getArgs
  streamBody <- getContents
  let body = if argsBody == "-s" then streamBody else argsBody
  result <- analyseJson body
  LBS.putStrLn result

analyseJson = fmap encode . analyse . decode . encodeUtf8 . T.pack

parseArgs :: [String] -> String
parseArgs [jsonBody] = jsonBody
parseArgs _          = error (intercalate "\n" [prettyVersion, usage])

decode = orFail . eitherDecode

usage = unpack [text|
        Wrong usage.

         #read from STDIN
         $ mulang -s

         #read from argument
         $ mulang '{
             "sample" : {
                "tag" : "CodeSample",
                "language" : "Haskell",
                "content" : "x = 1"
             },
             "spec" : {
                "expectations" : [
                   {
                      "binding" : ":Intransitive:x",
                      "inspection" : "Uses:*"
                   }
                ],
                "smellsSet" : { "tag" : "NoSmells" }
             }
          }'|]

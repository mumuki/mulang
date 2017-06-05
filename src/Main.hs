{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Language.Mulang.Analyzer (analyse)
import Language.Mulang.Analyzer.Analysis.Json ()
import Data.Aeson (eitherDecode, encode)
import System.Environment (getArgs)
import Control.Fallible (orFail)

import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LBS (putStrLn)
import qualified Data.Text.Lazy as T (pack)

main :: IO ()
main = do
  argsBody <- fmap parseArgs $ getArgs
  streamBody <- getContents
  let body = if argsBody == "-s" then streamBody else argsBody
  LBS.putStrLn . analyseJson $ body

analyseJson = encode . analyse . decode . encodeUtf8 . T.pack

parseArgs :: [String] -> String
parseArgs [jsonBody] = jsonBody
parseArgs _          = error usage

decode = orFail . eitherDecode

usage = "Wrong usage.                                            \n\
        \                                                        \n\
        \  #read from STDIN                                      \n\
        \  $ mulang -s                                           \n\
        \                                                        \n\
        \  #read from argument                                   \n\
        \  $ mulang '{                                           \n\
        \      \"expectations\":[{                               \n\
        \                \"tag\":\"Basic\",                      \n\
        \                \"binding\":\"x\",                      \n\
        \                \"inspection\":\"HasBiniding\"}],       \n\
        \        \"code\": {                                     \n\
        \            \"content\":\"x = 1\",                      \n\
        \            \"language\":\"Haskell\"}                   \n\
        \  }'"
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Language.Mulang.Cli.Interpreter
import Data.Aeson (eitherDecode, encode)
import System.Environment (getArgs)
import Control.Fallible (orFail)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack, putStrLn)

main :: IO ()
main = do
  body <- fmap parseArgs $ getArgs
  LBS.putStrLn . encode . evaluate . decode . LBS.pack $ body

parseArgs :: [String] -> String
parseArgs [jsonBody] = jsonBody
parseArgs _          = error usage

decode = orFail . eitherDecode

usage = "Wrong usage.                                 \n\
        \                                             \n\
        \  $ mulang '{                                \n\
        \      \"expectations\":[{                    \n\
        \                \"tag\":\"Basic\",           \n\
        \                \"binding\":\"x\",           \n\
        \                \"inspection\":\"\"}],       \n\
        \        \"code\": {                          \n\
        \            \"content\":\"x = 1\",           \n\
        \            \"language\":\"Haskell\"}        \n\
        \  }'"
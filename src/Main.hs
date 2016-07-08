{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Language.Mulang.Cli.Interpreter
import Data.Aeson (eitherDecode, encode)
import System.Environment (getArgs)
import Control.Fallible (orFail)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack, putStrLn)

main :: IO ()
main = do
  argsBody <- fmap parseArgs $ getArgs
  streamBody <- getContents
  let body = if argsBody == "-s" then streamBody else argsBody
  LBS.putStrLn . encode . evaluate . decode . LBS.pack $ body

parseArgs :: [String] -> String
parseArgs [jsonBody] = jsonBody
parseArgs _          = error usage

decode = orFail . eitherDecode

usage = "Wrong usage.                                 \n\
        \                                             \n\
        \  #read from STDIN                           \n\
        \  $ mulang -s                                \n\
        \                                             \n\
        \  #read from argument                        \n\
        \  $ mulang '{                                \n\
        \      \"expectations\":[{                    \n\
        \                \"tag\":\"Basic\",           \n\
        \                \"binding\":\"x\",           \n\
        \                \"inspection\":\"\"}],       \n\
        \        \"code\": {                          \n\
        \            \"content\":\"x = 1\",           \n\
        \            \"language\":\"Haskell\"}        \n\
        \  }'"
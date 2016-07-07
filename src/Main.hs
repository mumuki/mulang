{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Language.Mulang.Cli.Interpreter
import Data.Aeson
import System.Environment
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack, putStrLn)

main :: IO ()
main = do
  body <- fmap parseArgs $ getArgs
  LBS.putStrLn . encode . evaluate . fromJust . decode . LBS.pack $ body

parseArgs :: [String] -> String
parseArgs [jsonBody] = jsonBody
parseArgs _          = error usage


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
{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Main (main) where

import Language.Mulang.Cli.Interpreter
import Data.Aeson
import System.Environment
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Char8 as LBS (pack, putStrLn)

main :: IO ()
main = do
  [input] <- getArgs
  LBS.putStrLn . encode . evaluate . fromJust . decode . LBS.pack $ input



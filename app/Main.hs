{-# LANGUAGE CPP, DeriveGeneric, QuasiQuotes, OverloadedStrings #-}

module Main where

import Language.Mulang.Analyzer (analyse, analyseMany)
import Language.Mulang.Analyzer.Analysis.Json ()
import Data.Aeson (eitherDecode, encode)
import System.Environment (getArgs)
import Control.Fallible (orFail)

import           Data.List (intercalate)
import           Version (prettyVersion)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (putStrLn)
import qualified Data.Text.Lazy as T (pack)
import           Data.Text (unpack)
import           NeatInterpolation (text)

#ifdef ghcjs_HOST_OS
import           Data.JSString (JSString)
import qualified Data.JSString as JSS (pack, unpack)

import qualified Data.ByteString.Lazy.Char8 as C8 (unpack)

analyseIO :: JSString -> IO JSString
analyseIO = fmap  (JSS.pack . C8.unpack) . analyseJson . JSS.unpack
#endif

analyseJson :: String -> IO ByteString
analyseJson = fmap encode . analyse . decode . encodeUtf8 . T.pack
  where
    decode = orFail . eitherDecode

analyseJsons :: String -> IO ByteString
analyseJsons = fmap encode . analyseMany . decode . encodeUtf8 . T.pack
   where
      decode = orFail . eitherDecode

main :: IO ()
main = do
  streamBody <- getContents
  args <- getArgs
  result <- run args streamBody
  LBS.putStrLn result

  where
    run :: [String] -> String ->  IO ByteString
    run ["-s"] body = analyseJson body
    run ["-S"] body = analyseJsons body
    run [body] _    = analyseJson body
    run  _     _    = error (intercalate "\n" [prettyVersion, usage])

    usage = unpack [text|
        Wrong usage.

         #read from STDIN
         $ mulang -s

         #read from STDIN a list of analysis
         $ mulang -S

         #read from argument
         $ mulang '{
             "sample" : {
                "tag" : "CodeSample",
                "language" : "Haskell",
                "content" : "x = z + 1"
             },
             "spec" : {
                "expectations" : [
                   {
                      "binding" : "Intransitive:x",
                      "inspection" : "Uses:z"
                   }
                ],
                "smellsSet" : { "tag" : "NoSmells" }
             }
          }'|]

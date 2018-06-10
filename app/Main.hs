module Main (main, analyseIO) where

import Data.JSString (JSString)
import Data.JSString as S (pack, unpack)

import Language.Mulang.Analyzer (analyse)
import Language.Mulang.Analyzer.Analysis.Json ()
import Data.Aeson (eitherDecode, encode)
import Control.Fallible (orFail)

import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T (pack)

import qualified Data.ByteString.Lazy.Char8 as B (unpack)

main :: IO ()
main = return ()

analyseIO :: JSString -> IO JSString
analyseIO body = fmap  S.pack . analyseJson . S.unpack $ body
  where

    analyseJson :: String -> IO String
    analyseJson = fmap (B.unpack . encode) . analyse . decode . encodeUtf8 . T.pack

    decode = orFail . eitherDecode

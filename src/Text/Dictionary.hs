module Text.Dictionary (
  prepareFile,
  fromFile,
  toDictionary,
  exists,
  Dictionary) where

import Data.Set (Set, member, fromList, toList, fromDistinctAscList)
import Data.Char (toLower)

type Dictionary = Set String

prepareFile :: FilePath -> FilePath -> IO ()
prepareFile old new = edit old new (unlines . uniq . lines . lowercase)

fromFile :: FilePath -> IO Dictionary
fromFile path = fmap (toDictionary . lines) (readFile path)

toDictionary :: [String] -> Dictionary
toDictionary = fromDistinctAscList

exists :: String -> Dictionary -> Bool
exists e = member (lowercase e)

lowercase :: String -> String
lowercase = map toLower

uniq = toList . fromList

edit :: FilePath -> FilePath -> (String -> String) -> IO ()
edit old new f = readFile old >>= (return . f) >>= writeFile new

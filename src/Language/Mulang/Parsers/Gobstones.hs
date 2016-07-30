{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}
module Language.Mulang.Parsers.Gobstones (parseGobstones,GobstonesAst) where 

import Language.Mulang
import Data.Aeson
import Data.Traversable (traverse)
import Data.Foldable (toList)
import Control.Applicative
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

import GHC.Generics
import Data.Text (Text)

data GobstonesAst = AST [NodeAst] deriving (Show)

data NodeAst = Node { alias :: Alias   
                     , body :: Body 
                     , from :: From 
                     } deriving (Show)

data Alias = ProgramGobstones deriving (Show)

data Body = NullP deriving (Show)

type From = Int

instance FromJSON GobstonesAst  where
	parseJSON (Array list) =  (\a -> AST . toList <$> traverse parseJSON a) list

instance FromJSON NodeAst where
	parseJSON (Object v) = Node <$> v .: "alias" <*> 
                                    v .: "body" <*> 
                                    v .: "from"

instance FromJSON Alias  where
	parseJSON (String "program") = pure ProgramGobstones

instance FromJSON Body  where
	parseJSON Null = pure NullP

parseGobstones :: String -> Expression
parseGobstones  = translateGobstonesAst . fromJust . parseGobstonesAst

parseGobstonesAst :: String -> Maybe GobstonesAst
parseGobstonesAst = decode . LBS.pack

translateGobstonesAst :: GobstonesAst -> Expression
translateGobstonesAst  (AST ast) = Program []--(map translateNodeAst ast) --TODO : no estoy seguro si deberia ser asi o como abajo 

translateNodeAst :: NodeAst -> Expression
translateNodeAst (Node ProgramGobstones NullP _) = Program []
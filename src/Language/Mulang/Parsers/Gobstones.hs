{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}
module Language.Mulang.Parsers.Gobstones (translateGobstonesAst,GobstonesAst) where 

import Language.Mulang
import Data.Aeson
import Data.Traversable (traverse)
import Data.Foldable (toList)
import Control.Applicative

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


translateGobstonesAst :: GobstonesAst -> Expression
translateGobstonesAst  (AST ast) = Program (map translateNodeAst ast) --TODO : no estoy seguro si deberia ser asi o como abajo 

translateNodeAst :: NodeAst -> Expression
translateNodeAst (Node ProgramGobstones NullP _) = Program []
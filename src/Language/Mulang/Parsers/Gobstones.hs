{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}
module Language.Mulang.Parsers.Gobstones (parseGobstones,GobstonesAst) where 

import Language.Mulang
import Data.Aeson
import Data.HashMap.Lazy as  HashMap (HashMap, lookup, member)
import Data.Traversable (traverse)
import Data.Foldable (toList)
import Control.Applicative
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)


import GHC.Generics
import Data.Text (Text)

data GobstonesAst = AST [NodeAst] | NullProgram deriving (Show)

data NodeAst = ProgramDeclaration  --TODO : falta refactor
					{ alias :: Alias   
                     , body :: Body 
                     , from :: From 
                     } 
             | ProcedureDeclarationG 
             		{ alias :: Alias   
                     , body :: Body 
                     , from :: From 
                     , row :: Row
					 , to :: To
					 , value :: ValueP
					 , arity :: ArityP 
					 , reserved :: Reserved 
					 , led :: Led 
					 , lbp :: Lbp 
					 , name :: Name 
					 , parameters :: [Parameter]
                     } deriving (Show)

data Alias = ProgramGobstones | ProcedureDeclarationA deriving (Show)

data Body = NullP | Body { nodes :: [NodeAst]} deriving (Show)

data Led = NullLed deriving (Show)

type From = Int

type Row = Int

type To = Int

type Lbp = Int

type ValueP = String

type ArityP = String

type Reserved = Bool

type Name = String

data Parameter = P deriving (Show)

------------------------------------------------------

instance FromJSON GobstonesAst where
	parseJSON (Array list) =  (\a -> AST . toList <$> traverse parseJSON a) list
	parseJSON Null = pure NullProgram
	parseJSON _ = fail "Failed to parse GobstonesAst!"

parseNodeAst (Just "program") value = ProgramDeclaration <$> value .: "alias" <*> value .: "body" <*> value .: "from"
parseNodeAst (Just "procedureDeclaration") value = ProcedureDeclarationG <$> value .: "alias" <*> value .: "body" <*> value .: "from" <*> value .: "row" <*> value .: "to" <*> value .: "value" <*> value .: "arity" <*> value .: "reserved" <*> value .: "led" <*> value .: "lbp" <*> value .: "name" <*> value .: "parameters"
parseNodeAst Nothing value = fail "Failed to parse NodeAst!"

instance FromJSON NodeAst where
	parseJSON (Object v) = nodeAst
		where
				alias = HashMap.lookup "alias" v
		  	 	nodeAst = parseNodeAst alias v
	parseJSON _ = fail "Failed to parse NodeAst!"

instance FromJSON Alias  where
	parseJSON (String "program") = pure ProgramGobstones
	parseJSON (String "procedureDeclaration") = pure ProcedureDeclarationA
	parseJSON _ = fail "Failed to parse Alias!"

instance FromJSON Body  where
	parseJSON (Object v) = Body <$> v.: "nodes" 
	parseJSON Null = pure NullP
	parseJSON _ = fail "Failed to parse Body!"

instance FromJSON Parameter where
	parseJSON _ = pure P
	parseJSON _ = fail "Failed to parse Parameter!"

instance FromJSON Led where
	parseJSON Null = pure NullLed
	parseJSON _ = fail "Failed to parse Led!"

------------------------------------------------------

parseGobstones :: String -> Expression
parseGobstones  = translateGobstonesAst . fromJust . parseGobstonesAst

parseGobstonesAst :: String -> Maybe GobstonesAst
parseGobstonesAst = decode . LBS.pack

translateGobstonesAst :: GobstonesAst -> Expression
translateGobstonesAst NullProgram = MuNull
translateGobstonesAst  (AST ast) = Sequence $ map convertToExpression ast 

convertToExpression :: NodeAst -> Expression
convertToExpression n@(ProgramDeclaration _ _ _) = convertProgramToExpression n
convertToExpression n@(ProcedureDeclarationG _ _ _ _ _ _ _ _ _ _ _ _) = convertProcedureToExpression n

convertProgramToExpression :: NodeAst -> Expression
convertProgramToExpression (ProgramDeclaration _ body _) = convertBody body

convertProcedureToExpression :: NodeAst -> Expression
convertProcedureToExpression 
			(ProcedureDeclarationG _ body _ _ _ _ arity _ _ _ name parameters) = ProcedureDeclaration name [Equation (convertParameters parameters)  (UnguardedBody (convertBody body))]

convertParameters :: [Parameter] -> [Pattern]
convertParameters [] = []

convertBody :: Body -> Expression
convertBody NullP = MuNull


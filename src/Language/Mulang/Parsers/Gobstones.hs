{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}
module Language.Mulang.Parsers.Gobstones (parseGobstones) where 

import	Language.Mulang
import	Data.Aeson
import	Data.HashMap.Lazy as  HashMap (HashMap, lookup, member)
import	Data.Traversable (traverse)
import	Data.Foldable (toList)
import	Control.Applicative
import	Data.Maybe (fromJust)
import	qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import	qualified Data.Text as T
import	GHC.Generics
import	Data.Text (Text)
import	Language.Mulang.Builder
import	qualified Data.Vector as V


instance FromJSON Expression where
	parseJSON (Array list) =  (\a -> Sequence . toList <$> traverse parseNodes a) list
	parseJSON Null = pure MuNull
	parseJSON _ = fail "Failed to parse Expression!"

parseNodes (Object v) = nodeAst
		where
				alias = HashMap.lookup "alias" v
		  	 	nodeAst = parseNodeAst alias v

parseBodyExpression (Array list) = (\a -> Sequence . toList <$> traverse parseNodes a) list
parseBodyExpression Null = pure MuNull

parseBodyExpression' Null =  MuNull 

parseParametersPatterns :: Value -> [Pattern]
parseParametersPatterns (Array list) 
									| (V.null list) = []
									| otherwise = []--(\a -> toList <$> parseParameterPatterns a) list

parseParameterPatterns :: Value -> Pattern
parseParameterPatterns (Object value) = VariablePattern (lookupAndParseExpression parseNameExpression "name" value)

parseParametersExpression :: Value -> [Expression]
parseParametersExpression (Array list) 
									| (V.null list) = []
									| otherwise = []--(\a ->  toList <$> parseParameterExpression a) list

parseNameExpression :: Value -> String
parseNameExpression (String n) = T.unpack n 

lookupAndParseExpression :: (Value -> b) -> Text -> Object -> b
lookupAndParseExpression parseFunction string value= parseFunction ( fromJust (HashMap.lookup string value))

 
parseNodeAst (Just "program") value = lookupAndParseExpression parseBodyExpression "body" value                                                                                                                                                                --cambiar parseBodyExpression' por parseBodyExpression
parseNodeAst (Just "procedureDeclaration") value = pure $ normalize $ ProcedureDeclaration (lookupAndParseExpression parseNameExpression "name" value)  [Equation (lookupAndParseExpression parseParametersPatterns "parameters" value) (UnguardedBody (lookupAndParseExpression  parseBodyExpression' "body" value))]
parseNodeAst (Just "ProcedureCall") value = pure $ normalize $ Application (Variable (lookupAndParseExpression parseNameExpression "name" value)) (lookupAndParseExpression parseParametersExpression "parameters" value)
parseNodeAst Nothing value = fail "Failed to parse NodeAst!"


------------------------------------------------

parseGobstones :: String -> Expression
parseGobstones  = fromJust . parseExpressions

parseExpressions :: String -> Maybe Expression
parseExpressions = decode . LBS.pack
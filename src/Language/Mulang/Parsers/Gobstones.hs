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
parseBodyExpression' Null =  MuNull --TODO : no me termina de cerrar porque no me deja usar la funcion de arriba. 
									-- se que tiene que ver con pure y el tipo que devuelve esa funcion.

parseParametersPatterns :: Value -> [Pattern]
parseParametersPatterns (Array list) = if(null list) then []
										 else []--(\a ->  toList <$> traverse parseParameterPatterns a) list

parseParametersExpression :: Value -> [Expression]
parseParametersExpression (Array list) = if(null list) then []
										 else []--(\a ->  toList <$> traverse parseParameterExpression a) list

parseNameExpression :: Value -> String
parseNameExpression (String n) = T.unpack n 

lookupAndParseExpression :: (Value -> b) -> Text -> Object -> b
lookupAndParseExpression parseFunction string value= parseFunction ( fromJust (HashMap.lookup string value))

parseNodeAst (Just "program") value =  lookupAndParseExpression parseBodyExpression "body" value
parseNodeAst (Just "procedureDeclaration") value = pure  $ ProcedureDeclaration (lookupAndParseExpression parseNameExpression "name" value)  [Equation (lookupAndParseExpression parseParametersPatterns "parameters" value) (UnguardedBody (lookupAndParseExpression parseBodyExpression' "body" value))]
parseNodeAst (Just "ProcedureCall") value = pure $ Application (Variable (lookupAndParseExpression parseNameExpression "name" value)) (lookupAndParseExpression parseParametersExpression "parameters" value)
parseNodeAst Nothing value = fail "Failed to parse NodeAst!"


------------------------------------------------

parseGobstones :: String -> Expression
parseGobstones  = fromJust . parseExpressions

parseExpressions :: String -> Maybe Expression
parseExpressions = decode . LBS.pack
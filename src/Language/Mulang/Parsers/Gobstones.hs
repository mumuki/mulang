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
	  parseJSON  =  parseBodyExpression 

parseBodyExpression (Array list) = (\a -> Sequence . toList <$> traverse parseNodes a) list
parseBodyExpression Null = pure MuNull
parseBodyExpression _ = fail "Failed to parse Expression!"

parseNodes (Object v) = nodeAst
		where
				alias = HashMap.lookup "alias" v
		  	 	nodeAst = parseNodeAst alias v

parseParametersPatterns (Array list) 
									| (V.null list) = pure []
									| otherwise = pure []--(\a -> toList <$> parseParameterPatterns a) list


parseParameterPatterns (Object value) = VariablePattern <$> (lookupAndParseExpression parseNameExpression "name" value)


parseParametersExpression (Array list) 
									| (V.null list) = pure []
									| otherwise = pure []--(\a ->  toList <$> parseParameterExpression a) list

parseNameExpression (String n) = pure (T.unpack n) 

lookupAndParseExpression :: (Value -> b) -> Text -> Object -> b
lookupAndParseExpression parseFunction string value= parseFunction ( fromJust (HashMap.lookup string value))
 
parseNodeAst (Just "program") value = lookupAndParseExpression parseBodyExpression "body" value
parseNodeAst (Just "procedureDeclaration") value = ProcedureDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression parseParametersPatterns "parameters" value) <*> (UnguardedBody <$> (lookupAndParseExpression  parseBodyExpression "body" value))))
parseNodeAst (Just "ProcedureCall") value = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "name" value)) <*> (lookupAndParseExpression parseParametersExpression "parameters" value)
parseNodeAst Nothing value = fail "Failed to parse NodeAst!"


------------------------------------------------

parseGobstones :: String -> Expression
parseGobstones  = fromJust . parseExpressions

parseExpressions :: String -> Maybe Expression
parseExpressions = decode . LBS.pack
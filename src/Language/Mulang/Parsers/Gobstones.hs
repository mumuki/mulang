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
import	Language.Mulang.Builder as Builder
import	qualified Data.Vector as V
import	Data.Scientific as Scientific



instance FromJSON Expression where
	  parseJSON  =  parseBodyExpression 

parseBodyExpression (Array list) = Builder.normalize <$> (\a -> Sequence . toList <$> traverse parseNodes a) list
parseBodyExpression Null = pure MuNull
parseBodyExpression _ = fail "Failed to parse Expression!"

parseNodes (Object v) = nodeAst
		where
				alias = HashMap.lookup "alias" v
		  	 	nodeAst = parseNodeAst alias v

parseParametersPatterns (Array list) 
									| (V.null list) = pure []
									| otherwise = (\a -> toList <$> traverse parseParameterPatterns a) list

parseParameterPatterns (Object value) = VariablePattern <$> (lookupAndParseExpression parseNameExpression "value" value)


parseParametersExpression (Array list) 
									| (V.null list) = pure []
									| otherwise = (\a -> toList <$> traverse parseParameterExpression a) list

parseParameterExpression (Object value) = parseParameterValue (lookUpValue "value" value) (HashMap.lookup "reserved" value)

parseParameterValue n@(Number number) Nothing = MuNumber <$> (parseJSON n)
parseParameterValue b@(Bool bool) _ = MuBool <$> (parseJSON b)
parseParameterValue (Array direction) _ = MuSymbol <$> (parseListToDirection direction)--(\a -> MuSymbol . toList <$> traverse parseJSON a) direction--
parseParameterValue (Number number) _ = MuSymbol <$> (parseToColour number)
parseParameterValue s@(String text) _ = MuString <$> (parseNameExpression s)


parseToColour number = case (Scientific.floatingOrInteger number) 
						of 	(Right 0) -> parseJSON "Azul"
							(Right 1) -> parseJSON "Rojo"
							(Right 2) -> parseJSON "Negro"
							(Right 3) -> parseJSON "Verde"



parseListToDirection direction = let (Number n1,Number n2) = (V.head direction,V.last direction) 
								 in parseToDirection n1 n2
	where 
		parseToDirection number1 number2 = case ((Scientific.floatingOrInteger number1),(Scientific.floatingOrInteger number2))
											of	((Right 1),(Right 0)) -> parseJSON "Este"
												((Right 0),(Right 1)) -> parseJSON "Norte"
												((Right (-1)),(Right 0)) -> parseJSON "Oeste"
												((Right 0),(Right (-1))) -> parseJSON "Sur"

parseNameExpression (String n) = pure (T.unpack n) 

lookUpValue :: Text -> Object -> Value
lookUpValue string value = fromJust (HashMap.lookup string value)

lookupAndParseExpression :: (Value -> b) -> Text -> Object -> b
lookupAndParseExpression parseFunction string value= parseFunction (lookUpValue string value)

parseVariableName (Object value) =  parseNameExpression (lookUpValue "value" value) 

variableName value = lookupAndParseExpression parseVariableName "variable" value

variableValue value = lookupAndParseExpression  parseParameterExpression "expression" value
 
parseNodeAst (Just "program") value = lookupAndParseExpression parseBodyExpression "body" value
parseNodeAst (Just "procedureDeclaration") value = ProcedureDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression parseParametersPatterns "parameters" value) <*> (UnguardedBody <$> (lookupAndParseExpression  parseBodyExpression "body" value))))
parseNodeAst (Just "ProcedureCall") value = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "name" value)) <*> (lookupAndParseExpression parseParametersExpression "parameters" value)
parseNodeAst (Just ":=") value = VariableAssignment <$> (variableName value) <*> (variableValue value) 
parseNodeAst Nothing value = fail "Failed to parse NodeAst!"


------------------------------------------------

parseGobstones :: String -> Expression
parseGobstones  = fromJust . parseExpressions

parseExpressions :: String -> Maybe Expression
parseExpressions = decode . LBS.pack
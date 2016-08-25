{-# LANGUAGE OverloadedStrings #-}

module Language.Mulang.Parsers.Gobstones (gbs, parseGobstones) where 

import            Language.Mulang
import            Language.Mulang.Builder as Builder
import            Language.Mulang.Parsers


import            Data.Aeson
import            Data.HashMap.Lazy as  HashMap (HashMap, lookup, member)
import            Data.Traversable (traverse)
import            Data.Foldable (toList)
import            Data.Maybe (fromJust, isJust)
import            Data.Text (Text)
import            Data.Scientific as Scientific
import  qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import  qualified Data.Text as T
import  qualified Data.Vector as V

import            Control.Applicative
import            GHC.Generics

instance FromJSON Expression where
    parseJSON  =  parseBodyExpression 

parseBodyExpression (Array list) = Builder.normalize <$> simplify <$> (\a -> Sequence . toList <$> traverse parseNodes a) list
parseBodyExpression Null         = pure MuNull
parseBodyExpression _ = fail "Failed to parse Expression!"

parseNodes (Object v) = nodeAst
    where
        alias = HashMap.lookup "alias" v
        nodeAst = parseNodeAst alias v

mapObjectArray f (Array list) 
              | (V.null list) = pure []
              | otherwise = (\a -> toList <$> traverse f a) list

parseCaseValue (Object value) = (\x y -> (x,y)) <$> (expressionValue value "case") <*> (lookupAndParseExpression parseBodyExpression "body" value)

parseParameterPatterns (Object value) = VariablePattern <$> (lookupAndParseExpression parseNameExpression "value" value)

parseFunctionCall (Object value) = parseNodeAst (Just "ProcedureCall") value

parseSimpleValue (Object value) = parseSimpleExpressionValue (lookUpValue "value" value) (HashMap.lookup "reserved" value)

parseBinaryValue (Object value) = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "value" value)) <*> ((\x y -> [x,y]) <$> (expressionValue value "left") <*> (expressionValue value "right"))

parseSimpleExpressionValue n@(Number number) Nothing = MuNumber <$> (parseJSON n)
parseSimpleExpressionValue b@(Bool bool) _ = MuBool <$> (parseJSON b)
parseSimpleExpressionValue (Array direction) _ = MuSymbol <$> (parseListToDirection direction)
parseSimpleExpressionValue (Number number) _ = MuSymbol <$> (parseToColour number)
parseSimpleExpressionValue s@(String text) _ = MuString <$> (parseNameExpression s)


parseToColour number = case (Scientific.floatingOrInteger number) 
            of  (Right 0) -> parseJSON "Azul"
                (Right 1) -> parseJSON "Rojo"
                (Right 2) -> parseJSON "Negro"
                (Right 3) -> parseJSON "Verde"

parseListToDirection direction = let (Number n1,Number n2) = (V.head direction,V.last direction) 
                                 in parseToDirection n1 n2
  where 
    parseToDirection number1 number2 = case ((Scientific.floatingOrInteger number1),(Scientific.floatingOrInteger number2))
                      of  ((Right 1),(Right 0)) -> parseJSON "Este"
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

expressionValue value text | isFunctionCall = lookupAndParseExpression parseFunctionCall text value
                           | isBinary       = lookupAndParseExpression parseBinaryValue text value  
                           | otherwise      = lookupAndParseExpression parseSimpleValue text value
  where
    expression = let (Object  v) = lookUpValue text value
                 in v
    maybeName = HashMap.lookup "name" expression
    arity = HashMap.lookup "arity" expression
    isFunctionCall = isJust maybeName
    isBinary = case (fromJust arity) 
          of  (String "binary") -> True
              _                 -> False

addReturn (Sequence xs) e 
            | (null xs) = Return e
            | otherwise = Sequence (xs ++ [Return e]) 

parseNodeAst (Just "program") value = lookupAndParseExpression parseBodyExpression "body" value
parseNodeAst (Just "procedureDeclaration") value = ProcedureDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value) <*> (UnguardedBody <$> (lookupAndParseExpression  parseBodyExpression "body" value))))
parseNodeAst (Just "ProcedureCall") value = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "name" value)) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseNodeAst (Just ":=") value = VariableAssignment <$> (variableName value) <*> (expressionValue value "expression") 
parseNodeAst (Just "functionDeclaration") value = FunctionDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value) <*> (UnguardedBody <$> (addReturn <$> (lookupAndParseExpression  parseBodyExpression "body" value) <*> (expressionValue value "return")))))
parseNodeAst (Just "conditional") value = If <$> (expressionValue value "condition") <*> (lookupAndParseExpression parseBodyExpression "left" value) <*> (lookupAndParseExpression parseBodyExpression "right" value)
parseNodeAst (Just "while") value = While <$> (expressionValue value "expression") <*> (lookupAndParseExpression parseBodyExpression "body" value)
parseNodeAst (Just "repeat") value = Repeat <$> (expressionValue value "expression") <*> (lookupAndParseExpression parseBodyExpression "body" value)
parseNodeAst (Just "switch") value = Switch <$> (expressionValue value "value") <*> (lookupAndParseExpression (mapObjectArray parseCaseValue) "cases" value)
parseNodeAst (Just "PutStone") value = Application <$> (Variable <$> (pure "Poner")) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseNodeAst (Just "RemoveStone") value = Application <$> (Variable <$> (pure "Sacar")) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseNodeAst (Just "MoveClaw") value = Application <$> (Variable <$> (pure "Mover")) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseNodeAst Nothing value = fail "Failed to parse NodeAst!"

------------------------------------------------

simplify :: Expression -> Expression
simplify (Sequence [MuNull]) = MuNull
simplify (Sequence ((Sequence xs):es) ) = Sequence $ (map simplify xs) ++ (map simplify es) 
simplify  n = n

------------------------------------------------

gbs :: Parser
gbs  = fromJust . parseGobstones

parseGobstones :: MaybeParser
parseGobstones = decode . LBS.pack
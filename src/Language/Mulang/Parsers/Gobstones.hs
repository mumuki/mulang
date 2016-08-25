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

parseBodyExpression (Array list) = Builder.normalize . simplify . Sequence . toList <$> traverse parseNodes list
parseBodyExpression Null         = pure MuNull
parseBodyExpression _            = fail "Failed to parse Expression!"

parseNodes (Object v) = nodeAst
    where
        alias = HashMap.lookup "alias" v
        nodeAst = parseNodeAst alias v

mapObjectArray f (Array list) 
              | (V.null list) = pure []
              | otherwise = toList <$> traverse f list

parseCaseValue (Object value) = (\x y -> (x, y)) <$> (expressionValue value "case") <*> (lookupAndParseExpression parseBodyExpression "body" value)

parseParameterPatterns (Object value) = VariablePattern <$> (lookupAndParseExpression parseNameExpression "value" value)

parseFunctionCall (Object value) = parseNodeAst (Just "ProcedureCall") value

parseSimpleValue (Object value) = parseSimpleExpressionValue (lookUpValue "value" value) (HashMap.lookup "reserved" value)

parseBinaryValue (Object value) = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "value" value)) <*> ((\x y -> [x,y]) <$> (expressionValue value "left") <*> (expressionValue value "right"))

parseSimpleExpressionValue n@(Number number) Nothing = MuNumber <$> (parseJSON n)
parseSimpleExpressionValue b@(Bool bool) _ = MuBool <$> (parseJSON b)
parseSimpleExpressionValue (Array direction) _ = MuSymbol <$> (parseListToDirection direction)
parseSimpleExpressionValue (Number number) _ = MuSymbol <$> (parseToColour number)
parseSimpleExpressionValue s@(String text) _ = MuString <$> (parseNameExpression s)


parseToColour = parseColour . Scientific.floatingOrInteger
                where parseColour (Right n) =  parseJSON . numberToColour $ n

                      numberToColour 0 = "Azul"
                      numberToColour 1 = "Rojo"
                      numberToColour 2 = "Negro"
                      numberToColour 3 = "Verde"


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
lookUpValue string = fromJust .  HashMap.lookup string

lookupAndParseExpression :: (Value -> b) -> Text -> Object -> b
lookupAndParseExpression parseFunction string = parseFunction . lookUpValue string 

parseVariableName (Object value) =  parseNameExpression . lookUpValue "value" $ value 

variableName = lookupAndParseExpression parseVariableName "variable"

expressionValue value text | isFunctionCall = lookupAndParseExpression parseFunctionCall text value
                           | isBinary       = lookupAndParseExpression parseBinaryValue text value  
                           | otherwise      = lookupAndParseExpression parseSimpleValue text value
  where
    expression | (Object  v) <- lookUpValue text value = v
    maybeName      = HashMap.lookup "name" expression
    arity          = HashMap.lookup "arity" expression
    isFunctionCall = isJust maybeName    
    isBinary | String "binary"  <- fromJust arity = True
             | otherwise = False

parseToken "program" value              = lookupAndParseExpression parseBodyExpression "body" value
parseToken "procedureDeclaration" value = ProcedureDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value) <*> (UnguardedBody <$> (lookupAndParseExpression  parseBodyExpression "body" value))))
parseToken "ProcedureCall" value        = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "name" value)) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseToken ":=" value                   = VariableAssignment <$> (variableName value) <*> (expressionValue value "expression") 
parseToken "functionDeclaration" value  = FunctionDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value) <*> (UnguardedBody <$> (addReturn <$> (lookupAndParseExpression  parseBodyExpression "body" value) <*> (expressionValue value "return")))))
parseToken "conditional" value          = If <$> (expressionValue value "condition") <*> (lookupAndParseExpression parseBodyExpression "left" value) <*> (lookupAndParseExpression parseBodyExpression "right" value)
parseToken "while" value                = While <$> (expressionValue value "expression") <*> (lookupAndParseExpression parseBodyExpression "body" value)
parseToken "repeat" value               = Repeat <$> (expressionValue value "expression") <*> (lookupAndParseExpression parseBodyExpression "body" value)
parseToken "switch" value               = Switch <$> (expressionValue value "value") <*> (lookupAndParseExpression (mapObjectArray parseCaseValue) "cases" value)
parseToken "PutStone" value             = Application <$> (Variable <$> (pure "Poner")) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseToken "RemoveStone" value          = Application <$> (Variable <$> (pure "Sacar")) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseToken "MoveClaw" value             = Application <$> (Variable <$> (pure "Mover")) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)

parseNodeAst (Just token)  = parseToken token
parseNodeAst Nothing       = fail "Failed to parse NodeAst!"

------------------------------------------------

addReturn (Sequence []) e = Return e
addReturn (Sequence xs) e = Sequence (xs ++ [Return e]) 


simplify :: Expression -> Expression
simplify (Sequence [MuNull]) = MuNull
simplify (Sequence ((Sequence xs):es) ) = Sequence $ (map simplify xs) ++ (map simplify es) 
simplify  n = n

------------------------------------------------

gbs :: Parser
gbs  = fromJust . parseGobstones

parseGobstones :: MaybeParser
parseGobstones = decode . LBS.pack
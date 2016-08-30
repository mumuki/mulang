{-# LANGUAGE OverloadedStrings #-}

module Language.Mulang.Parsers.Gobstones (gbs, parseGobstones) where 

import            Language.Mulang
import            Language.Mulang.Builder as Builder
import            Language.Mulang.Parsers


import            Data.Aeson
import            Data.HashMap.Lazy as  HashMap (HashMap, lookup, member,insert,empty,fromList)
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

parseSimpleExpressionValue n@(Number _) Nothing = MuNumber <$> (parseJSON n)
parseSimpleExpressionValue b@(Bool _) _ = MuBool <$> (parseJSON b)
parseSimpleExpressionValue (Array direction) _ = MuSymbol <$> (parseListToDirection direction)
parseSimpleExpressionValue (Number number) _ = MuSymbol <$> (parseToColour number)
parseSimpleExpressionValue s@(String _) _ = MuString <$> (parseNameExpression s)


parseToColour = parseColour . Scientific.floatingOrInteger
                where parseColour (Right n) =  parseJSON . numberToColour $ n

                      numberToColour 0 = "Azul"
                      numberToColour 1 = "Rojo"
                      numberToColour 2 = "Negro"
                      numberToColour 3 = "Verde"


parseListToDirection direction = let (Number n1,Number n2) = (V.head direction,V.last direction) 
                                 in parseToDirection n1 n2
  where 
    parseToDirection number1 number2 = parseDirection  ((Scientific.floatingOrInteger number1),(Scientific.floatingOrInteger number2))
                where parseDirection ((Right n1),(Right n2)) = parseJSON . numbersToDirection $ (n1,n2)
                      
                      numbersToDirection (1, 0)  = "Este"
                      numbersToDirection (0, 1)  = "Norte"
                      numbersToDirection (-1, 0) = "Oeste"
                      numbersToDirection (0, -1) = "Sur"




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

parseToken "program" value                = lookupAndParseExpression parseBodyExpression "body" value
parseToken "procedureDeclaration" value   = ProcedureDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value) <*> (UnguardedBody <$> (lookupAndParseExpression  parseBodyExpression "body" value))))
parseToken "ProcedureCall" value          = Application <$> (Variable <$> (lookupAndParseExpression parseNameExpression "name" value)) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)
parseToken ":=" value                     = VariableAssignment <$> (variableName value) <*> (expressionValue value "expression") 
parseToken "functionDeclaration" value    = FunctionDeclaration <$> (lookupAndParseExpression parseNameExpression "name" value) <*> ((\x -> [x]) <$> (Equation <$> (lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value) <*> (UnguardedBody <$> (addReturn <$> (lookupAndParseExpression  parseBodyExpression "body" value) <*> (expressionValue value "return")))))
parseToken "conditional" value            = If <$> (expressionValue value "condition") <*> (lookupAndParseExpression parseBodyExpression "left" value) <*> (lookupAndParseExpression parseBodyExpression "right" value)
parseToken "while" value                  = parseRepetitionFunction While value
parseToken "repeat" value                 = parseRepetitionFunction Repeat value
parseToken "switch" value                 = Switch <$> (expressionValue value "value") <*> (lookupAndParseExpression (mapObjectArray parseCaseValue) "cases" value)
parseToken "PutStone" value               = parsePrimitive "Poner" value
parseToken "RemoveStone" value            = parsePrimitive "Sacar" value
parseToken "MoveClaw" value               = parsePrimitive "Mover" value
parseToken "hasStones" value              = parsePrimitive "hayBolitas" value
parseToken "canMove" value                = parsePrimitive "puedeMover" value


parsePrimitive primitiveName value = Application <$> (Variable <$> (pure primitiveName)) <*> (lookupAndParseExpression (mapObjectArray parseSimpleValue) "parameters" value)

parseRepetitionFunction f value = f <$> (expressionValue value "expression") <*> (lookupAndParseExpression parseBodyExpression "body" value)

parseNodeAst (Just token)  = parseToken token
parseNodeAst Nothing       = fail "Failed to parse NodeAst!"


------------------------------------------------

addReturn (Sequence []) e = Return e
addReturn (Sequence xs) e = Sequence (xs ++ [Return e]) 
addReturn x e = Sequence [x,(Return e)]

-- TODO : por alguna razon si se compone en parseJson , la funcion conver .. no funciona bien.. lo cual es raro.  
simplify :: Expression -> Expression
simplify (Sequence [x]) = simplify x
simplify (Sequence ((Sequence xs):es) ) = convertVariableAssignmentToDeclaration $ Sequence $ (map simplify xs) ++ (map simplify es) 
simplify  n = n




convertVariableAssignmentToDeclaration :: Expression ->Expression
convertVariableAssignmentToDeclaration (Sequence xs) = Sequence (convertListWithMap xs HashMap.empty)
convertVariableAssignmentToDeclaration x = x

convertListWithMap [] hashMap = []
convertListWithMap (a@(VariableAssignment _ _):xs) hashMap = let (v,newMap) =  convertVariable a hashMap in  v : (convertListWithMap xs newMap)
convertListWithMap (f@(FunctionDeclaration _ _):xs) hashMap                 =  (convertVariablesInFunctionOrProcedure f HashMap.empty) : (convertListWithMap xs hashMap)
convertListWithMap (p@(ProcedureDeclaration _ _):xs) hashMap                =  (convertVariablesInFunctionOrProcedure p HashMap.empty) : (convertListWithMap xs hashMap)
convertListWithMap (x:xs) hashMap                                           =  (convertVariablesInConditionals x hashMap) : (convertListWithMap xs hashMap)


--  TODO : de aca para abajo falta refactor.
convertVariable v@(VariableAssignment identifier body) map | HashMap.member identifier map = (v,map)
                                                           | otherwise                     = (VariableDeclaration identifier body,HashMap.insert identifier identifier map)


convertVariablesInFunctionOrProcedure  (FunctionDeclaration name [eq]) map                 = FunctionDeclaration name [(convertVariablesInEquation eq)]
convertVariablesInFunctionOrProcedure  (ProcedureDeclaration name [eq] ) map               = ProcedureDeclaration name [(convertVariablesInEquation eq)]


convertVariablesInConditionals c@(If e bodyL bodyR) hashMap               = If e (convertBody bodyL hashMap) (convertBody bodyR hashMap)
convertVariablesInConditionals w@(While e body) hashMap                   = While e (convertBody body hashMap)
convertVariablesInConditionals r@(Repeat e body) hashMap                  = Repeat e (convertBody body hashMap)
convertVariablesInConditionals s@(Switch e cases) hashMap                 = Switch e (convertCases cases hashMap)  
convertVariablesInConditionals x _                                        = x


convertBody (Sequence xs) hashMap              = (Sequence $ convertListWithMap xs hashMap)
convertBody a@(VariableAssignment _ _) hashMap =  let (v,newMap) =  convertVariable a hashMap in  v
convertBody a _ = a


convertCases [] hashMap              = []
convertCases ((e1,b1):cases) hashMap = (e1,convertBody b1 hashMap):convertCases cases hashMap

convertVariablesInEquation (Equation xs (UnguardedBody e)) = Equation xs (UnguardedBody (convertVariableAssignmentToDeclaration e) )


------------------------------------------------

gbs :: Parser
gbs  = fromJust . parseGobstones

parseGobstones :: MaybeParser
parseGobstones = decode . LBS.pack
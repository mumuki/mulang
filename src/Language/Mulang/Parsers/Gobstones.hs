{-# LANGUAGE OverloadedStrings #-}

module Language.Mulang.Parsers.Gobstones (
    gbs,
    gba,
    parseGobstones,
    parseGobstonesAst) where

import            Language.Mulang.Ast hiding (Object)
import            Language.Mulang.Builder as Builder
import            Language.Mulang.Parsers


import            Data.Aeson
import  qualified Data.Aeson.Types (Parser)
import            Data.HashMap.Lazy as  HashMap (HashMap, lookup, member, insert, empty)
import            Data.Traversable (traverse)
import            Data.Foldable (toList)
import            Data.Maybe (fromJust, isJust)
import            Data.Text (Text)
import            Data.Scientific as Scientific
import  qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import  qualified Data.Text as T
import  qualified Data.Vector as V

import            GHC.Generics ()

import            System.Process (readProcessWithExitCode)
import            System.IO.Unsafe (unsafePerformIO)

type JsonParser a = Value ->  Data.Aeson.Types.Parser a

instance FromJSON Expression where
    parseJSON  =  parseBodyExpression

parseBodyExpression :: JsonParser Expression
parseBodyExpression (Array list) | (V.null list) = pure MuNull
parseBodyExpression (Array list) = Builder.normalize . simplify . Sequence . toList <$> traverse parseNodes list
parseBodyExpression Null         = pure MuNull
parseBodyExpression _            = fail "Failed to parse Expression!"

parseNodes :: JsonParser Expression
parseNodes (Object v) = nodeAst
    where
        alias = HashMap.lookup "alias" v
        nodeAst = parseNodeAst alias v

mapObjectArray f (Array list)
              | (V.null list) = pure []
              | otherwise = toList <$> traverse f list

parseCaseValue (Object value) = (\x y -> (x, y)) <$> expressionValue "case" value <*> lookupAndParseExpression parseBodyExpression "body" value

parseParameterPatterns :: JsonParser Pattern
parseParameterPatterns (Object value) = VariablePattern <$> lookupAndParseExpression parseNameExpression "value" value

parseFunctionCall :: JsonParser Expression
parseFunctionCall (Object value) = parseNodeAst (Just "ProcedureCall") value

parseSimpleValue :: JsonParser Expression
parseSimpleValue (Object value) = parseSimpleExpressionValue (HashMap.lookup "alias" value) $ lookUpValue "value" value
    where
          parseSimpleExpressionValue (Just "NumericLiteral") n@(Number _) = MuNumber <$> parseJSON n
          parseSimpleExpressionValue _ b@(Bool _) = MuBool <$> parseJSON b
          parseSimpleExpressionValue _ (Number number) = MuSymbol <$> parseToColor number
          parseSimpleExpressionValue _ s@(String _) = Reference <$> parseNameExpression s
          parseSimpleExpressionValue _ (Array direction) = MuSymbol <$> parseListToDirection direction

          parseToColor :: Scientific -> Data.Aeson.Types.Parser String
          parseToColor = parseColor . scientificToInteger

          parseColor :: Integer -> Data.Aeson.Types.Parser String
          parseColor n =  parseJSON . numberToColor $ n

          numberToColor :: Integer -> Value
          numberToColor 0 = "Azul"
          numberToColor 1 = "Rojo"
          numberToColor 2 = "Negro"
          numberToColor 3 = "Verde"

          parseListToDirection direction = let (Number n1, Number n2) = (V.head direction , V.last direction)
                                           in parseToDirection n1 n2

          parseToDirection :: Scientific -> Scientific -> Data.Aeson.Types.Parser String
          parseToDirection number1 number2 = parseDirection  (scientificToInteger number1 , scientificToInteger number2)

          parseDirection (n1 , n2) = parseJSON . numbersToDirection $ (n1,n2)

          numbersToDirection (1, 0)  = "Este"
          numbersToDirection (0, 1)  = "Norte"
          numbersToDirection (-1, 0) = "Oeste"
          numbersToDirection (0, -1) = "Sur"

scientificToInteger :: Scientific -> Integer
scientificToInteger = extractInteger . Scientific.floatingOrInteger
          where extractInteger :: Either Double Integer -> Integer
                extractInteger (Right i) = i
                extractInteger (Left d)  = error $ "Tried to parse an integer, but a floting " ++ show d ++" was found"

parseBinaryValue :: JsonParser Expression
parseBinaryValue (Object value) = Application <$> evaluatedFunction <$> lookupAndParseExpression parseNameExpression "alias" value <*> ((\x y -> [x,y]) <$> expressionValue  "left" value <*> expressionValue "right" value)

parseNotValue :: JsonParser Expression
parseNotValue (Object value) = Application <$> evaluatedFunction <$> lookupAndParseExpression parseNameExpression "alias" value <*> (\x-> [x]) <$> expressionValue  "expression" value

parseNameExpression (String n) = pure $ T.unpack n

lookUpValue :: Text -> Object -> Value
lookUpValue string = fromJust . HashMap.lookup string

lookupAndParseExpression :: (Value -> b) -> Text -> Object -> b
lookupAndParseExpression parseFunction string = parseFunction . lookUpValue string


parseVariableName :: JsonParser String
parseVariableName (Object value) =  parseNameExpression . lookUpValue "value" $ value


variableName = lookupAndParseExpression parseVariableName "left"

evaluatedFunction "EqOperation"                      = Equal
evaluatedFunction "NotEqualOperation"                = NotEqual
evaluatedFunction "AndOperation"                     = Reference "&&"
evaluatedFunction "OrOperation"                      = Reference "||"
evaluatedFunction "LessEqualOperation"               = Reference "<="
evaluatedFunction "LessOperation"                    = Reference "<"
evaluatedFunction "GraterOperation"                  = Reference ">"
evaluatedFunction "GreaterEqualOperation"            = Reference ">="
evaluatedFunction  fun                               = Reference fun

parseExpression :: JsonParser Expression
parseExpression value = switchParser $ value
      where switchParser | isJust maybeName = parseFunctionCall
                         | isBinary         = parseBinaryValue
                         | isNot            = parseNotValue
                         | otherwise        = parseSimpleValue

            expression | (Object  v) <- value = v

            maybeName      = HashMap.lookup "name" expression
            arity          = HashMap.lookup "arity" expression
            alias          = HashMap.lookup "alias" expression

            isNot    | isJust alias && (String "not"  == fromJust alias)    = True
                     | otherwise                                            = False

            isBinary | isJust arity && (String "binary"  == fromJust arity) = True
                     | otherwise                                            = False

expressionValue text = parseExpression . lookUpValue text

convertReturn :: JsonParser Expression
convertReturn (Object value) = expressionValue "expression" value


parseToken :: Value -> Object -> Data.Aeson.Types.Parser Expression
parseToken "program" value                = EntryPoint <$> lookupAndParseExpression parseBodyExpression "body" value
parseToken "procedureDeclaration" value   = Procedure <$> lookupAndParseExpression parseNameExpression "name" value <*> return <$> (Equation <$> lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value <*> (UnguardedBody <$> lookupAndParseExpression  parseBodyExpression "body" value))
parseToken "ProcedureCall" value          = Application <$> evaluatedFunction <$> lookupAndParseExpression parseNameExpression "name" value <*> lookupAndParseExpression (mapObjectArray parseExpression) "parameters" value
parseToken ":=" value                     = Assignment <$> variableName value <*> expressionValue "right" value
parseToken "functionDeclaration" value    = Function <$> lookupAndParseExpression parseNameExpression "name" value <*> return <$> (Equation <$> lookupAndParseExpression (mapObjectArray parseParameterPatterns) "parameters" value <*> (UnguardedBody <$> (addReturn <$> lookupAndParseExpression  parseBodyExpression "body" value <*> lookupAndParseExpression  convertReturn "return" value)))
parseToken "if" value                     = If <$> expressionValue "condition" value <*> lookupAndParseExpression parseBodyExpression "trueBranch" value <*> lookupAndParseExpression parseBodyExpression "falseBranch" value
parseToken "while" value                  = parseRepetitionFunction While value
parseToken "repeat" value                 = parseRepetitionFunction Repeat value
parseToken "switch" value                 = Switch <$> expressionValue "expression" value <*> lookupAndParseExpression (mapObjectArray parseCaseValue) "cases" value
parseToken "return" value                 = Return <$> expressionValue "expression" value
parseToken "Drop" value                   = parsePrimitive "Poner" value
parseToken "Grab" value                   = parsePrimitive "Sacar" value
parseToken "MoveClaw" value               = parsePrimitive "Mover" value
parseToken "hasStones" value              = parsePrimitive "hayBolitas" value
parseToken "canMove" value                = parsePrimitive "puedeMover" value


parsePrimitive primitiveName value = Application <$> evaluatedFunction <$> pure primitiveName <*> lookupAndParseExpression (mapObjectArray parseExpression) "parameters" value

parseRepetitionFunction f value = f <$> expressionValue "expression" value <*> lookupAndParseExpression parseBodyExpression "body" value

parseNodeAst (Just token)  = parseToken token
parseNodeAst Nothing       = fail "Failed to parse NodeAst!"


------------------------------------------------
addReturn :: Expression -> Expression -> Expression
addReturn (Sequence []) e = Return e
addReturn (Sequence xs) e = Sequence $ xs ++ [Return e]
addReturn x e = Sequence [x,(Return e)]

simplify :: Expression -> Expression
simplify (Sequence ((Sequence xs):es) ) = convertAssignmentToDeclaration $ Sequence $ (map simplify xs) ++ map simplify es
simplify (Sequence [x]) = convertAssignmentToDeclaration $ simplify x
simplify  n = n

convertAssignmentToDeclaration :: Expression ->Expression
convertAssignmentToDeclaration (Sequence xs) = Sequence $ convertListWithMap xs HashMap.empty
convertAssignmentToDeclaration x = head $ convertListWithMap [x] HashMap.empty

convertListWithMap :: [Expression] -> HashMap Identifier Identifier-> [Expression]
convertListWithMap [] _ = []
convertListWithMap (a@(Assignment _ _):xs) hashMap = let (v,newMap) =  convertVariable a hashMap in  v : convertListWithMap xs newMap
convertListWithMap (f@(Function _ _):xs) hashMap                 =  (convertVariablesInFunctionOrProcedure f HashMap.empty) : convertListWithMap xs hashMap
convertListWithMap (p@(Procedure _ _):xs) hashMap                =  (convertVariablesInFunctionOrProcedure p HashMap.empty) : convertListWithMap xs hashMap
convertListWithMap (x:xs) hashMap                                           =  (convertVariablesInConditionals x hashMap) : convertListWithMap xs hashMap


--  TODO : de aca para abajo falta refactor.
convertVariable v@(Assignment identifier body) map | HashMap.member identifier map = (v,map)
                                                           | otherwise                     = (Variable identifier body,HashMap.insert identifier identifier map)



convertVariablesInFunctionOrProcedure  (Function name [eq]) _                 = Function name [(convertVariablesInEquation eq)]
convertVariablesInFunctionOrProcedure  (Procedure name [eq] ) _               = Procedure name [(convertVariablesInEquation eq)]


convertVariablesInConditionals (If e bodyL bodyR) hashMap               = If e (convertBody bodyL hashMap) (convertBody bodyR hashMap)
convertVariablesInConditionals (While e body) hashMap                   = While e (convertBody body hashMap)
convertVariablesInConditionals (Repeat e body) hashMap                  = Repeat e (convertBody body hashMap)
convertVariablesInConditionals (Switch e cases) hashMap                 = Switch e (convertCases cases hashMap)
convertVariablesInConditionals x _                                      = x


convertBody (Sequence xs) hashMap              = (Sequence $ convertListWithMap xs hashMap)
convertBody a@(Assignment _ _) hashMap =  let (v,_) =  convertVariable a hashMap in  v
convertBody a _ = a


convertCases [] _                    = []
convertCases ((e1,b1):cases) hashMap = (e1,convertBody b1 hashMap):convertCases cases hashMap

convertVariablesInEquation (SimpleEquation xs e) = SimpleEquation xs (convertAssignmentToDeclaration e)


------------------------------------------------

gba :: Parser
gba  = fromJust . parseGobstonesAst

parseGobstonesAst :: MaybeParser
parseGobstonesAst = decode . LBS.pack

gbs :: Parser
gbs  = fromJust . parseGobstones

parseGobstones :: MaybeParser
parseGobstones = parseGobstonesAst . gobstonesToAst

gobstonesToAst :: String -> String
gobstonesToAst = result . unsafePerformIO . readProcessWithExitCode "rungs" []
                where result (_, out, _) = out

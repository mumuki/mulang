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

-- Combinators

get :: Text -> Object -> Maybe Value
get = HashMap.lookup

getJust :: Text -> Object -> Value
getJust key = fromJust . get key

getValue :: Object -> Value
getValue = getJust "value"

getWith :: (Value -> b) -> Text -> Object -> b
getWith f key = f . getJust key

-- Actual Parser

instance FromJSON Expression where
    parseJSON  =  parseBodyExpression

parseBodyExpression :: JsonParser Expression
parseBodyExpression (Array list) | (V.null list) = pure MuNull
parseBodyExpression (Array list) = Builder.normalize . simplify . Sequence . toList <$> traverse parseNodes list
parseBodyExpression Null         = pure MuNull
parseBodyExpression _            = fail "Failed to parse Expression!"

parseNodes :: JsonParser Expression
parseNodes (Object v) = nodeAst
    where nodeAst = parseToken (getJust "alias" v) v

mapObjectArray f (Array list)
              | (V.null list) = pure []
              | otherwise = toList <$> traverse f list

parseCaseValue (Object value) = (\x y -> (x, y)) <$> expressionValue "case" value <*> getWith parseBodyExpression "body" value

parseParameterPatterns :: JsonParser Pattern
parseParameterPatterns (Object value) = VariablePattern <$> getWith parseNameExpression "value" value

parseFunctionCall :: JsonParser Expression
parseFunctionCall (Object value) = parseNodeAst (Just "ProcedureCall") value

parseSimpleValue :: JsonParser Expression
parseSimpleValue (Object value) = parseSimpleExpressionValue (get "alias" value) $ getValue value
    where
          parseSimpleExpressionValue (Just "NumericLiteral") n@(Number _) = MuNumber <$> parseJSON n
          parseSimpleExpressionValue _ b@(Bool _)         = MuBool <$> parseJSON b
          parseSimpleExpressionValue _ (Number number)   = MuSymbol <$> parseColor number
          parseSimpleExpressionValue _ s@(String _)      = Reference <$> parseNameExpression s
          parseSimpleExpressionValue _ (Array direction) = MuSymbol <$> parseListToDirection direction

          parseColor :: Scientific -> Data.Aeson.Types.Parser String
          parseColor = parseJSON . numberToColor . scientificToInteger

          numberToColor :: Integer -> Value
          numberToColor = (!!) ["Azul", "Rojo", "Negro", "Verde"] . fromIntegral

          parseListToDirection direction = let (Number n1, Number n2) = (V.head direction , V.last direction)
                                           in parseDirection n1 n2

          parseDirection :: Scientific -> Scientific -> Data.Aeson.Types.Parser String
          parseDirection number1 number2 = parseJSON . numbersToDirection $ (scientificToInteger number1 , scientificToInteger number2)

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
parseBinaryValue (Object value) = Application <$> parseFunction <$> getWith parseNameExpression "alias" value <*> ((\x y -> [x,y]) <$> expressionValue  "left" value <*> expressionValue "right" value)

parseNotValue :: JsonParser Expression
parseNotValue (Object value) = Application <$> parseFunction <$> getWith parseNameExpression "alias" value <*> (:[]) <$> expressionValue  "expression" value

parseNameExpression (String n) = pure $ T.unpack n


parseVariableName :: JsonParser String
parseVariableName (Object value) =  parseNameExpression . getValue $ value


variableName = getWith parseVariableName "left"

parseFunction :: String -> Expression
parseFunction "EqOperation"                      = Equal
parseFunction "NotEqualOperation"                = NotEqual
parseFunction "AndOperation"                     = Reference "&&"
parseFunction "OrOperation"                      = Reference "||"
parseFunction "LessEqualOperation"               = Reference "<="
parseFunction "LessOperation"                    = Reference "<"
parseFunction "GraterOperation"                  = Reference ">"
parseFunction "GreaterEqualOperation"            = Reference ">="
parseFunction  fun                               = Reference fun

parseExpression :: JsonParser Expression
parseExpression value = switchParser value
      where switchParser | isJust maybeName = parseFunctionCall
                         | isBinary         = parseBinaryValue
                         | isNot            = parseNotValue
                         | otherwise        = parseSimpleValue

            expression | (Object  v) <- value = v

            maybeName      = get "name" expression
            arity          = get "arity" expression
            alias          = get "alias" expression

            isNot    = isJust alias && (String "not"  == fromJust alias)
            isBinary = isJust arity && (String "binary"  == fromJust arity)

expressionValue = getWith parseExpression

convertReturn :: JsonParser Expression
convertReturn (Object value) = expressionValue "expression" value


parseToken :: Value -> Object -> Data.Aeson.Types.Parser Expression
parseToken "program" value                = EntryPoint "program" <$> parseProgramBody value
parseToken "procedureDeclaration" value   = Procedure <$> getWith parseNameExpression "name" value <*> return <$> (Equation <$> getWith (mapObjectArray parseParameterPatterns) "parameters" value <*> (UnguardedBody <$> getWith  parseBodyExpression "body" value))
parseToken "ProcedureCall" value          = Application <$> parseFunction <$> getWith parseNameExpression "name" value <*> getWith (mapObjectArray parseExpression) "parameters" value
parseToken ":=" value                     = Assignment <$> variableName value <*> expressionValue "right" value
parseToken "functionDeclaration" value    = Function <$> getWith parseNameExpression "name" value <*> return <$> (Equation <$> getWith (mapObjectArray parseParameterPatterns) "parameters" value <*> (UnguardedBody <$> (addReturn <$> getWith  parseBodyExpression "body" value <*> getWith  convertReturn "return" value)))
parseToken "if" value                     = If <$> expressionValue "condition" value <*> getWith parseBodyExpression "trueBranch" value <*> getWith parseBodyExpression "falseBranch" value
parseToken "while" value                  = parseRepetitionFunction While value
parseToken "repeat" value                 = parseRepetitionFunction Repeat value
parseToken "switch" value                 = Switch <$> expressionValue "expression" value <*> getWith (mapObjectArray parseCaseValue) "cases" value
parseToken "return" value                 = Return <$> expressionValue "expression" value
parseToken "Drop" value                   = parsePrimitive "Poner" value
parseToken "Grab" value                   = parsePrimitive "Sacar" value
parseToken "MoveClaw" value               = parsePrimitive "Mover" value
parseToken "hasStones" value              = parsePrimitive "hayBolitas" value
parseToken "canMove" value                = parsePrimitive "puedeMover" value

parseProgramBody value = getWith parseBodyExpression "body" value


parsePrimitive primitiveName value = Application <$> parseFunction <$> pure primitiveName <*> getWith (mapObjectArray parseExpression) "parameters" value

parseRepetitionFunction f value = f <$> expressionValue "expression" value <*> getWith parseBodyExpression "body" value

parseNodeAst (Just token)  = parseToken token


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

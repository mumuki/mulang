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
import            Data.HashMap.Lazy as  HashMap (HashMap, lookup, member, insert, empty)
import            Data.Maybe (fromJust, isJust)
import            Data.Text (Text)
import            Data.Scientific as Scientific
import  qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import  qualified Data.Text as T
import  qualified Data.Vector as V

import            System.Process (readProcessWithExitCode)
import            System.IO.Unsafe (unsafePerformIO)

-- Combinators

get :: Text -> Value -> Maybe Value
get key (Object o) = HashMap.lookup key o

getJust :: Text -> Value -> Value
getJust key = fromJust . get key

getValue :: Value -> Value
getValue = getJust "value"

getWith :: (Value -> b) -> Text -> Value -> b
getWith f key = f . getJust key

getArrayWith :: (Value -> b) -> Text -> Value -> [b]
getArrayWith f = getWith (mapObjectArray f)

getString :: Text -> Value -> String
getString = getStringWith id

getStringWith :: (String -> b) -> Text -> Value -> b
getStringWith f = getWith (f . (\(String s) -> T.unpack s))

-- Actual Parser

parseBody :: Value -> Expression
parseBody (Array list) | (V.null list) = MuNull
parseBody a@(Array _)  = Builder.normalize . simplify . Sequence . mapObjectArray  parseNodes $ a
parseBody Null         = MuNull
parseBody _            = error "Failed to parse Expression!"

parseNodes :: Value -> Expression
parseNodes o = parseToken (getJust "alias" o) o

mapObjectArray :: (Value -> a) -> Value -> [a]
mapObjectArray f (Array vector) = V.toList . V.map f $ vector

parseCaseValue :: Value -> (Expression, Expression)
parseCaseValue o = (expressionValue "case" o, getWith parseBody "body" o)

parseParameter :: Value -> Pattern
parseParameter  = VariablePattern . getString "value"

parseFunctionCall :: Value -> Expression
parseFunctionCall = parseToken "ProcedureCall"

parseSimpleValue :: Value -> Expression
parseSimpleValue o = parseSimpleExpressionValue (get "alias" o) $ getValue o
    where
          parseSimpleExpressionValue (Just "NumericLiteral") (Number n) = MuNumber $ toRealFloat n
          parseSimpleExpressionValue _ (Bool b)         = MuBool b
          parseSimpleExpressionValue _ (Number number)   = MuSymbol $ parseColor number
          parseSimpleExpressionValue _ (String s)      = Reference $ T.unpack s
          parseSimpleExpressionValue _ (Array direction) = MuSymbol $ parseListToDirection direction


          parseListToDirection direction = let (Number n1, Number n2) = (V.head direction , V.last direction)
                                           in parseDirection n1 n2

          parseDirection :: Scientific -> Scientific -> String
          parseDirection number1 number2 = numbersToDirection $ (scientificToInteger number1 , scientificToInteger number2)

          numbersToDirection (1, 0)  = "Este"
          numbersToDirection (0, 1)  = "Norte"
          numbersToDirection (-1, 0) = "Oeste"
          numbersToDirection (0, -1) = "Sur"

parseColor :: Scientific -> String
parseColor = ((!!) ["Azul", "Rojo", "Negro", "Verde"]) . fromIntegral . scientificToInteger

scientificToInteger :: Scientific -> Integer
scientificToInteger = extractInteger . Scientific.floatingOrInteger
          where extractInteger :: Either Double Integer -> Integer
                extractInteger (Right i) = i
                extractInteger (Left d)  = error $ "Tried to parse an integer, but a floting " ++ show d ++" was found"

parseBinaryValue :: Value -> Expression
parseBinaryValue o = Application (getStringWith parseFunction "alias" o) [expressionValue  "left" o, expressionValue "right" o]

parseNotValue :: Value -> Expression
parseNotValue o = Application (getStringWith parseFunction "alias" o) [expressionValue  "expression" o]


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

parseExpression :: Value -> Expression
parseExpression o | isJust maybeName = parseFunctionCall o
                  | isBinary         = parseBinaryValue o
                  | isNot            = parseNotValue o
                  | otherwise        = parseSimpleValue o
          where
            maybeName      = get "name" o
            arity          = get "arity" o
            alias          = get "alias" o

            isNot    = isJust alias && (String "not"  == fromJust alias)
            isBinary = isJust arity && (String "binary"  == fromJust arity)

expressionValue = getWith parseExpression

parseReturn :: Value -> Expression
parseReturn = expressionValue "expression"


parseToken :: Value -> Value -> Expression
parseToken "program" o                = EntryPoint "program" (parseProgramBody o)
parseToken "procedureDeclaration" o   = (Procedure
                                          (getString "name" o)
                                          [Equation
                                            (getArrayWith parseParameter "parameters" o)
                                            (UnguardedBody (getWith parseBody "body" o))])
parseToken "ProcedureCall" o          = (Application
                                          (getStringWith parseFunction "name" o)
                                          (getArrayWith parseExpression "parameters" o))
parseToken ":=" o                     = Assignment (getWith (getString "value") "left" o) (expressionValue "right" o)
parseToken "functionDeclaration" o    = (Function
                                          (getString "name" o)
                                          [Equation
                                            (getArrayWith parseParameter "parameters" o)
                                            (UnguardedBody (addReturn (getWith  parseBody "body" o) (getWith  parseReturn "return" o)))])
parseToken "if" o                     = (If
                                          (expressionValue "condition" o)
                                          (getWith parseBody "trueBranch" o)
                                          (getWith parseBody "falseBranch" o))
parseToken "while" o                  = parseRepetitionFunction While o
parseToken "repeat" o                 = parseRepetitionFunction Repeat o
parseToken "switch" o                 = Switch (expressionValue "expression" o) (getArrayWith parseCaseValue "cases" o)
parseToken "return" o                 = Return (expressionValue "expression" o)
parseToken "Drop" o                   = parsePrimitive "Poner" o
parseToken "Grab" o                   = parsePrimitive "Sacar" o
parseToken "MoveClaw" o               = parsePrimitive "Mover" o
parseToken "hasStones" o              = parsePrimitive "hayBolitas" o
parseToken "canMove" o                = parsePrimitive "puedeMover" o

parseProgramBody o = getWith parseBody "body" o

parsePrimitive primitiveName value = Application (parseFunction primitiveName) (getArrayWith parseExpression "parameters" value)

parseRepetitionFunction f value = f (expressionValue "expression" value) (getWith parseBody "body" value)

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
gba  =  fromJust . parseGobstonesAst

parseGobstonesAst :: MaybeParser
parseGobstonesAst = fmap parseBody . decode . LBS.pack

gbs :: Parser
gbs  = fromJust . parseGobstones

parseGobstones :: MaybeParser
parseGobstones = parseGobstonesAst . gobstonesToAst

gobstonesToAst :: String -> String
gobstonesToAst = result . unsafePerformIO . readProcessWithExitCode "rungs" []
                where result (_, out, _) = out

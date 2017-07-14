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
import            Data.Maybe (fromJust)
import            Data.Text (Text)
import            Data.Scientific as Scientific
import  qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import  qualified Data.Text as T
import  qualified Data.Vector as V

import            System.Process (readProcessWithExitCode)
import            System.IO.Unsafe (unsafePerformIO)

-------------
-- Getters --
-------------

type Getter a = Text -> Value -> a

get :: Getter (Maybe Value)
get key (Object o) = HashMap.lookup key o

getJust :: Getter Value
getJust key = fromJust . get key

getWith :: (Value -> b) -> Getter b
getWith f key = f . getJust key

getArrayWith :: (Value -> b) -> Getter [b]
getArrayWith f = getWith (parseArray f)

getString :: Getter String
getString = getStringWith id

getStringWith :: (String -> b) -> Getter b
getStringWith f = getWith (f . (\(String s) -> T.unpack s))

getExpression :: Getter Expression
getExpression = getWith parseExpression

getBody :: Getter Expression
getBody = getWith parseBody

-------------------
-- Actual Parser --
-------------------

parseBody :: Value -> Expression
parseBody (Array list) | (V.null list) = MuNull
parseBody a@(Array _)  = Builder.normalize . simplify . Sequence . parseArray  parseKeyword' $ a
parseBody Null         = MuNull
parseBody _            = error "Failed to parse Expression!"

parseArray :: (Value -> a) -> Value -> [a]
parseArray f (Array vector) = V.toList . V.map f $ vector

parseCaseValue :: Value -> (Expression, Expression)
parseCaseValue o = (getExpression "case" o, getBody "body" o)

parseParameter :: Value -> Pattern
parseParameter  = VariablePattern . getString "value"

parseFunctionCall :: Value -> Expression
parseFunctionCall = parseKeyword "ProcedureCall"

parseLiteral :: Value -> Expression
parseLiteral o = f (get "alias" o) (getJust "value" o)
    where
          f (Just "NumericLiteral") (Number n) = MuNumber $ toRealFloat n
          f _                (Bool b)          = MuBool b
          f _                (Number number)   = MuSymbol $ parseColor number
          f _                (String s)        = Reference $ T.unpack s
          f _                (Array direction) = MuSymbol $ parseListToDirection direction


          parseListToDirection direction = let (Number n1, Number n2) = (V.head direction , V.last direction)
                                           in parseDirection n1 n2

parseDirection :: Scientific -> Scientific -> String
parseDirection number1 number2 = f (scientificToInteger number1 , scientificToInteger number2)
  where
    f (1, 0)  = "Este"
    f (0, 1)  = "Norte"
    f (-1, 0) = "Oeste"
    f (0, -1) = "Sur"

parseColor :: Scientific -> String
parseColor = ((!!) ["Azul", "Rojo", "Negro", "Verde"]) . fromIntegral . scientificToInteger

scientificToInteger :: Scientific -> Integer
scientificToInteger = extractInteger . Scientific.floatingOrInteger
  where extractInteger :: Either Double Integer -> Integer
        extractInteger (Right i) = i
        extractInteger (Left d)  = error $ "Tried to parse an integer, but a floting " ++ show d ++" was found"

parseBinary :: Value -> Expression
parseBinary o = Application (getStringWith parseFunction "alias" o) [getExpression  "left" o, getExpression "right" o]

parseNot :: Value -> Expression
parseNot o = Application (getStringWith parseFunction "alias" o) [getExpression  "expression" o]


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
parseExpression o | (Just _) <- get "name" o                  = parseFunctionCall o
                  | (Just (String "binary")) <- get "arity" o = parseBinary o
                  | (Just (String "not"))    <- get "alias" o = parseNot o
                  | otherwise        = parseLiteral o

parseReturn :: Value -> Expression
parseReturn = getExpression "expression"

parseKeyword' :: Value -> Expression
parseKeyword' o = parseKeyword (getJust "alias" o) o

parseKeyword :: Value -> Value -> Expression
parseKeyword "program" o                = EntryPoint "program" (parseProgramBody o)
parseKeyword "procedureDeclaration" o   = (Procedure
                                            (getString "name" o)
                                            [Equation
                                              (getArrayWith parseParameter "parameters" o)
                                              (UnguardedBody (getBody "body" o))])
parseKeyword "ProcedureCall" o          = (Application
                                            (getStringWith parseFunction "name" o)
                                            (getArrayWith parseExpression "parameters" o))
parseKeyword ":=" o                     = Assignment (getWith (getString "value") "left" o) (getExpression "right" o)
parseKeyword "functionDeclaration" o    = (Function
                                            (getString "name" o)
                                            [Equation
                                              (getArrayWith parseParameter "parameters" o)
                                              (UnguardedBody (addReturn (getWith  parseBody "body" o) (getWith  parseReturn "return" o)))])
parseKeyword "if" o                     = (If
                                            (getExpression "condition" o)
                                            (getBody "trueBranch" o)
                                            (getBody "falseBranch" o))
parseKeyword "while" o                  = parseRepeat While o
parseKeyword "repeat" o                 = parseRepeat Repeat o
parseKeyword "switch" o                 = Switch (getExpression "expression" o) (getArrayWith parseCaseValue "cases" o)
parseKeyword "return" o                 = Return (getExpression "expression" o)
parseKeyword "Drop" o                   = parsePrimitive "Poner" o
parseKeyword "Grab" o                   = parsePrimitive "Sacar" o
parseKeyword "MoveClaw" o               = parsePrimitive "Mover" o
parseKeyword "hasStones" o              = parsePrimitive "hayBolitas" o
parseKeyword "canMove" o                = parsePrimitive "puedeMover" o

parseProgramBody o = getBody "body" o

parsePrimitive primitiveName value = Application (parseFunction primitiveName) (getArrayWith parseExpression "parameters" value)

parseRepeat f value = f (getExpression "expression" value) (getBody "body" value)

---------------------------
-- Expression Transforms --
---------------------------

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


----------------------
-- Public Interface --
----------------------

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

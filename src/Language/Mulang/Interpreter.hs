{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mulang.Interpreter (
  defaultContext,
  dereference,
  dereference',
  eval,
  eval',
  evalExpr,
  evalRaising,
  nullRef,
  ExecutionContext(..),
  Value(..)
) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List (find, intercalate)
import           Control.Monad (forM)
import           Control.Monad.State.Class
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Control.Monad.Cont
import           Data.Fixed (mod')

import qualified Language.Mulang.Ast as M
import qualified Language.Mulang.Ast.Operator as O
import           Language.Mulang.Interpreter.Internals

eval' :: ExecutionContext -> Executable Reference -> IO (Reference, ExecutionContext)
eval' ctx ref = runStateT (runContT ref return) ctx

eval :: ExecutionContext -> M.Expression -> IO (Reference, ExecutionContext)
eval ctx expr = eval' ctx (evalExpr expr)

evalRaising :: ExecutionContext -> M.Expression ->  Executable (Reference, Maybe Reference)
evalRaising context expr = do
  resultRef <- callCC $ \raiseCallback -> do
    put (context { currentRaiseCallback = raiseCallback })
    evalExpr expr
    return nullRef
  lastException <- gets currentException
  return (resultRef, lastException)

evalExpressionsWith :: [M.Expression] -> ([Value] -> Executable Reference) -> Executable Reference
evalExpressionsWith expressions f = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  f params

evalExpr :: M.Expression -> Executable Reference
evalExpr (M.Sequence expressions) = last <$> forM expressions evalExpr
evalExpr (M.Lambda params body) = do
  executionFrames <- gets scopes
  createReference $
    MuFunction executionFrames [M.Equation params (M.UnguardedBody body)]

evalExpr (M.Subroutine name body) = do
  executionFrames <- gets scopes
  let function = MuFunction executionFrames body
  ref <- createReference function
  unless (null name) (setLocalVariable name ref) -- if function has no name we avoid registering it
  return ref

evalExpr (M.Print expression) = do
  parameter :: Value <- evalExpr expression >>= dereference
  liftIO $ print parameter
  return nullRef

evalExpr (M.Assert negated (M.Truth expression)) =
  evalExpressionsWith [expression] f
  where f [MuBool result]
          | result /= negated = return nullRef
          | otherwise         = raiseString $ "Expected " ++ (show . not $ negated) ++ " but got: " ++ show result

evalExpr (M.Assert negated (M.Equality expected actual)) =
  evalExpressionsWith [expected, actual] f
  where f [v1, v2]
          | muEquals v1 v2 /= negated = return nullRef
          | otherwise                 = raiseString $ "Expected " ++ show v1 ++ " but got: " ++ show v2

evalExpr (M.Application (M.Primitive O.Negation) expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b] = createBool $ not b
        f params     = raiseTypeError "expected one boolean" params

evalExpr (M.Application (M.Primitive O.Equal) expressions) = do
  params <- mapM evalExpr expressions
  let [r1, r2] = params
  muValuesEqual r1 r2

evalExpr (M.Application (M.Primitive O.NotEqual) expressions) = do
  evalExpr $ M.Application (M.Primitive O.Negation) [M.Application (M.Primitive O.Equal) expressions]

evalExpr (M.Application (M.Primitive op) expressions) | Just op' <- reifyOperator op = evalOperator op' expressions

evalExpr (M.MuList expressions) = do
  refs <- forM expressions evalExpr
  createReference $ MuList refs

evalExpr (M.New klass expressions) = do
  (MuFunction locals ([M.SimpleEquation params body])) <- evalExpr klass >>= dereference
  objReference <- createReference $ MuObject Map.empty
  thisContext <- createReference $ MuObject $ Map.singleton "this" objReference
  paramsContext <- evalParams params expressions
  runFunction (thisContext:paramsContext:locals) body
  return objReference

evalExpr (M.Application function expressions) = do
  (MuFunction locals ([M.SimpleEquation params body])) <- evalExpr function >>= dereference
  paramsContext <- evalParams params expressions
  returnValue <- runFunction (paramsContext:locals) (body)
  return returnValue

evalExpr (M.If cond thenBranch elseBranch) = do
  v <- evalCondition cond
  if v then evalExpr thenBranch else evalExpr elseBranch

evalExpr (M.MuNumber n) = createNumber n
evalExpr (M.MuNil) = return nullRef
evalExpr (M.MuBool b) = createBool b
evalExpr (M.MuString s) = createReference $ MuString s
evalExpr (M.Return e) = do
  ref <- evalExpr e
  currentReturn <- gets (currentReturnCallback)
  currentReturn ref
  return ref -- Unreachable
evalExpr (M.Variable name expr) = do
  r <- evalExpr expr
  setLocalVariable name r
  return r

evalExpr (M.While cond expr) = do
  whileM (evalCondition cond) (evalExpr expr)
  return nullRef

evalExpr (M.ForLoop beforeExpr cond afterExpr expr) = do
  evalExpr beforeExpr
  whileM (evalCondition cond) $ do
    evalExpr expr
    evalExpr afterExpr
  return nullRef

evalExpr (M.Assignment name expr) = do
  valueRef <- evalExpr expr
  frameRef <- findFrameForName' name
  case frameRef of
    Just ref -> updateRef ref (addAttrToObject name valueRef)
    Nothing -> setLocalVariable name valueRef
  return valueRef

evalExpr (M.Try expr [( M.VariablePattern exName, catchExpr)] finallyExpr) = do
  context <- get
  (resultRef, lastException) <- evalRaising context expr
  modify' (\c ->
              c { currentReturnCallback = currentReturnCallback context
                , currentRaiseCallback = currentRaiseCallback context
                , currentException = Nothing
                })
  case lastException of
    Nothing -> return resultRef
    Just ref -> do
      setLocalVariable exName ref
      evalExpr catchExpr

  evalExpr finallyExpr

evalExpr (M.Raise expr) = raiseInternal =<< evalExpr expr

evalExpr (M.Reference name) = findReferenceForName name
evalExpr (M.None) = return nullRef
evalExpr e = raiseString $ "Unkown expression: " ++ show e

evalCondition :: M.Expression -> Executable Bool
evalCondition cond = evalExpr cond >>= dereference >>= muBool
  where
    muBool (MuBool value) = return value
    muBool v              = raiseTypeError ("expected " ++ debugType "Boolean") [v]

evalParams :: [M.Pattern] -> [M.Expression] -> Executable Reference
evalParams params arguments = do
  evaluatedParams <- forM arguments evalExpr
  let localsAfterParameters = Map.fromList $ zip (getParamNames params) (evaluatedParams ++ repeat nullRef)
  createReference $ MuObject localsAfterParameters

raiseInternal :: Reference -> Executable b
raiseInternal exceptionRef = do
  raiseCallback <- gets currentRaiseCallback
  modify' (\c -> c {currentException = Just exceptionRef})
  raiseCallback exceptionRef
  raiseString "Unreachable" -- the callback above should never allow this to execute

raiseString :: String -> Executable a
raiseString s = do
  raiseInternal =<< (createReference $ MuString s)

raiseTypeError :: String -> [Value] ->Executable a
raiseTypeError message values = raiseString $ "Type error: " ++ message ++ " but got " ++ (intercalate ", " . map debugValue $ values)

muValuesEqual r1 r2
  | r1 == r2 = createReference $ MuBool True
  | otherwise = do
      v1 <- dereference r1
      v2 <- dereference r2
      createBool $ muEquals v1 v2

muEquals (MuBool b1)   (MuBool b2)   = b1 == b2
muEquals (MuNumber n1) (MuNumber n2) = n1 == n2
muEquals (MuString s1) (MuString s2) = s1 == s2
muEquals MuNull        MuNull        = True
muEquals _             _             = False

getParamNames :: [M.Pattern] -> [String]
getParamNames = fmap (\(M.VariablePattern n) -> n)

runFunction :: [Reference] -> M.Expression -> Executable Reference
runFunction functionEnv body = do
  context <- get
  returnValue <- callCC $ \(returnCallback) -> do
    put (context { scopes = functionEnv, currentReturnCallback = returnCallback })
    evalExpr body
    return nullRef
  modify' (\c -> c { scopes = scopes context
                   , currentReturnCallback = currentReturnCallback context
                   , currentRaiseCallback = currentRaiseCallback context
                   , currentException = currentException context
                   })
  return returnValue

findFrameForName :: String -> Executable Reference
findFrameForName name = do
  maybe (raiseString $ "Reference not found for name '" ++ name ++ "'") return
    =<< findFrameForName' name

findFrameForName' :: String -> Executable (Maybe Reference)
findFrameForName' name = do
  framesRefs <- gets scopes
  frames :: [(Reference, Map String Reference)] <- forM framesRefs $ \ref -> do
    dereference ref >>= \value -> case value of
      (MuObject context) -> return (ref, context)
      v -> error $ "Finding '" ++ name ++ "' the frame I got a non object " ++ show v

  return $ fmap fst . find (Map.member name . snd) $ frames

findReferenceForName :: String -> Executable Reference
findReferenceForName name = do
  ref <- findFrameForName name
  (MuObject context) <- dereference ref
  return $ context Map.! name

nullRef = Reference 0

createBool = createReference . MuBool
createNumber = createReference . MuNumber

setLocalVariable :: String -> Reference -> Executable ()
setLocalVariable name ref = do
  frame <- currentFrame
  updateRef frame (addAttrToObject name ref)

  where
      currentFrame :: Executable Reference
      currentFrame = gets (head . scopes)

addAttrToObject :: String -> Reference -> Value -> Value
addAttrToObject k r (MuObject map) = MuObject $ Map.insert k r map
addAttrToObject k _r v = error $ "Tried adding " ++ k ++ " to a non object: " ++ show v

-- TODO make this evaluation non strict on both parameters
evalBinaryBoolean :: String -> (Bool -> Bool -> Bool) -> [M.Expression] -> Executable Reference
evalBinaryBoolean name op expressions = evalExpressionsWith expressions f
  where f [MuBool b1, MuBool b2]      = createBool $ op b1 b2
        f params                      = raiseTypeError (name ++ " expected two " ++ debugType "Boolean") params

evalBinaryNumeric :: String -> (Double -> Double -> a) -> (a -> Executable Reference) -> [M.Expression] -> Executable Reference
evalBinaryNumeric name op pack expressions = evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2]       = pack $ op n1 n2
        f params                           = raiseTypeError (name ++ " expected two " ++ debugType "Number") params


-- ==================
-- Operators handling
-- ==================

data Operator
  = NumericBinaryFunction String (Double -> Double -> Double)
  | NumericBinaryPredicate String  (Double -> Double -> Bool)
  | BinaryPredicate String (Bool -> Bool -> Bool)

reifyOperator :: O.Operator -> Maybe Operator
reifyOperator O.And                  = Just $ BinaryPredicate  "And" (&&)
reifyOperator O.GreatherOrEqualThan  = Just $ NumericBinaryPredicate "GreatherOrEqualThan" (>=)
reifyOperator O.GreatherThan         = Just $ NumericBinaryPredicate "GreatherThan" (>)
reifyOperator O.LessOrEqualThan      = Just $ NumericBinaryPredicate "LessOrEqualThan" (<=)
reifyOperator O.LessThan             = Just $ NumericBinaryPredicate "LessThan" (<)
reifyOperator O.Minus                = Just $ NumericBinaryFunction "Minus" (-)
reifyOperator O.Modulo               = Just $ NumericBinaryFunction  "Modulo" (mod')
reifyOperator O.Multiply             = Just $ NumericBinaryFunction "Multiply" (*)
reifyOperator O.Or                   = Just $ BinaryPredicate  "Or" (||)
reifyOperator O.Plus                 = Just $ NumericBinaryFunction "Plus" (+)
reifyOperator _                      = Nothing

evalOperator :: Operator -> [M.Expression] -> Executable Reference
evalOperator (NumericBinaryFunction name op)    = evalBinaryNumeric (debug ["Operator", name]) op createNumber
evalOperator (NumericBinaryPredicate name op)   = evalBinaryNumeric (debug ["Operator", name]) op createBool
evalOperator (BinaryPredicate name op)          = evalBinaryBoolean (debug ["Operator", name]) op

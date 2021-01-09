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
import           Data.List (find, intercalate, genericLength)
import           Control.Monad (forM, (>=>))
import           Control.Monad.State.Class
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Control.Monad.Cont
import           Data.Fixed (mod')

import qualified Language.Mulang.Ast as M
import qualified Language.Mulang.Ast.Operator as O
import           Language.Mulang.Ast.Operator (opposite)
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
  params <- forM expressions evalExprValue
  f params

evalExpressionsWith' :: [M.Expression] -> ([(Reference, Value)] -> Executable Reference) -> Executable Reference
evalExpressionsWith' expressions f = do
  refs <- forM expressions evalExpr
  values <- forM refs dereference
  f $ zip refs values

evalExprValue :: M.Expression -> Executable Value
evalExprValue = evalExpr >=> dereference

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
  parameter <- evalExprValue expression
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

evalExpr (M.Application (M.Primitive O.GreatherOrEqualThan) expressions) = evalBinaryNumeric expressions (>=) createBool
evalExpr (M.Application (M.Primitive O.Modulo) expressions) = evalBinaryNumeric expressions (mod') createNumber
evalExpr (M.Application (M.Primitive O.GreatherThan) expressions) = evalBinaryNumeric expressions (>) createBool

evalExpr (M.Application (M.Primitive O.Or) expressions) = evalBinaryBoolean expressions (||)
evalExpr (M.Application (M.Primitive O.And) expressions) = evalBinaryBoolean expressions (&&)

evalExpr (M.Application (M.Primitive O.Negation) expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b] = createBool $ not b
        f params     = raiseTypeError "expected one boolean" params

evalExpr (M.Application (M.Primitive O.Size) expressions) =
  evalExpressionsWith expressions f
  where f [MuList xs] = createNumber $ genericLength xs
        f params      = raiseTypeError "expected a list" params

evalExpr (M.Application (M.Primitive O.GetAt) expressions) =
  evalExpressionsWith expressions f
  where f [MuObject m, MuString s] | Just ref <- Map.lookup s m = return ref
                                   | otherwise = raiseString ("key error: " ++ s)
        f params        = raiseTypeError "expected an object" params

evalExpr (M.Application (M.Primitive O.SetAt) expressions) =
  evalExpressionsWith' expressions f
  where f [(or, MuObject _), (_, MuString s), (vr, _)] = updateRef or (setObjectAt s vr) >> return vr
        f params                                       = raiseTypeError "expected an object" (map snd params)


evalExpr (M.Application (M.Primitive O.Multiply) expressions) = evalBinaryNumeric expressions (*) createNumber

evalExpr (M.Application (M.Primitive O.Like) expressions) = do
  params <- forM expressions evalExpr
  let [r1, r2] = params
  muValuesEqual r1 r2

evalExpr (M.Application (M.Primitive op@O.NotLike) expressions) = do
  evalExpr $ M.Application (M.Primitive O.Negation) [M.Application (M.Primitive (opposite op)) expressions]

evalExpr (M.Application (M.Primitive O.LessOrEqualThan) expressions) = evalBinaryNumeric expressions (<=) createBool
evalExpr (M.Application (M.Primitive O.LessThan) expressions) = evalBinaryNumeric expressions (<) createBool
evalExpr (M.Application (M.Primitive O.Plus) expressions) = evalBinaryNumeric expressions (+) createNumber
evalExpr (M.Application (M.Primitive O.Minus) expressions) = evalBinaryNumeric expressions (-) createNumber

evalExpr (M.MuList expressions) = do
  refs <- forM expressions evalExpr
  createReference $ MuList refs

evalExpr (M.MuDict expression)   = evalObject expression
evalExpr (M.MuObject expression) = evalObject expression
evalExpr (M.Object name expression) = evalExpr (M.Variable name (M.MuObject expression))

evalExpr (M.FieldAssignment e1 k e2) = evalExpr (M.Application (M.Primitive O.SetAt) [e1, M.MuString k, e2])
evalExpr (M.FieldReference expression k) = evalExpr (M.Application (M.Primitive O.GetAt) [expression, M.MuString k])

evalExpr (M.New klass expressions) = do
  (MuFunction locals ([M.SimpleEquation params body])) <- evalExprValue klass
  objReference <- createObject Map.empty
  thisContext <- createObject $ Map.singleton "this" objReference
  paramsContext <- evalParams params expressions
  runFunction (thisContext:paramsContext:locals) body
  return objReference

evalExpr (M.Application function expressions) = do
  (MuFunction locals ([M.SimpleEquation params body])) <- evalExprValue function
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
    Just ref -> updateRef ref (setObjectAt name valueRef)
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

evalObject :: M.Expression -> Executable Reference
evalObject M.None          = createObject (Map.empty)
evalObject (M.Sequence es) = do
  arrowRefs <- forM es evalArrow
  createObject $ Map.fromList arrowRefs
evalObject e             = do
  (s, vRef) <- evalArrow e
  createObject $ Map.singleton s vRef

evalArrow :: M.Expression -> Executable (String, Reference)
evalArrow (M.LValue n v)  = evalArrow (M.Arrow (M.MuString n) v)
evalArrow (M.Arrow k v)   = do
  (MuString s) <- evalExprValue k
  vRef <- evalExpr v
  return (s, vRef)
evalArrow e               = raiseString ("malformed object arrow: " ++ show e)

-- TODO make this evaluation non strict on both parameters
evalBinaryBoolean :: [M.Expression] -> (Bool -> Bool -> Bool) -> Executable Reference
evalBinaryBoolean expressions op = evalExpressionsWith expressions f
  where f [MuBool b1, MuBool b2] = createBool $ op b1 b2
        f params                 = raiseTypeError "expected two booleans" params

evalBinaryNumeric :: [M.Expression] -> (Double -> Double -> a) -> (a -> Executable Reference) -> Executable Reference
evalBinaryNumeric expressions op pack = evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = pack $ op n1 n2
        f params                     = raiseTypeError "expected two numbers" params

evalCondition :: M.Expression -> Executable Bool
evalCondition cond = evalExprValue cond >>= muBool
  where
    muBool (MuBool value) = return value
    muBool v              = raiseTypeError "expected boolean" [v]

evalParams :: [M.Pattern] -> [M.Expression] -> Executable Reference
evalParams params arguments = do
  evaluatedParams <- forM arguments evalExpr
  let localsAfterParameters = Map.fromList $ zip (getParamNames params) (evaluatedParams ++ repeat nullRef)
  createObject localsAfterParameters

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
raiseTypeError message values = raiseString $ "Type error: " ++ message ++ " but got " ++ (intercalate ", " . map debug $ values)

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
createObject = createReference . MuObject

setLocalVariable :: String -> Reference -> Executable ()
setLocalVariable name ref = do
  frame <- currentFrame
  updateRef frame (setObjectAt name ref)

  where
      currentFrame :: Executable Reference
      currentFrame = gets (head . scopes)

setObjectAt :: String -> Reference -> Value -> Value
setObjectAt k r (MuObject map) = MuObject $ Map.insert k r map
setObjectAt k _r v = error $ "Tried adding " ++ k ++ " to a non object: " ++ show v

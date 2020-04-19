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
import           Data.Maybe (fromMaybe, fromJust)
import           Data.List (find, intercalate)
import qualified Data.Map.Strict as Map
import           Control.Monad (forM)
import           Control.Monad.State.Class
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Control.Monad.Cont
import           Data.Fixed (mod')

import qualified Language.Mulang.Ast as M
import qualified Language.Mulang.Ast.Operator as O

type Executable m = ContT Reference (StateT ExecutionContext IO) m

type Callback = Reference -> Executable ()

newtype Reference = Reference Int deriving (Show, Eq, Ord)

type ObjectSpace = Map Reference Value

instance Show ExecutionContext where
  show (ExecutionContext globalObjects scopes _ _ _) =
    "ExecutionContext { globalObjects = " ++ show globalObjects ++ ", scopes = " ++ show scopes ++ " }"

data ExecutionContext = ExecutionContext { globalObjects :: ObjectSpace
                                         , scopes :: [Reference]
                                         , currentException :: Maybe Reference
                                         , currentReturnCallback :: Callback
                                         , currentRaiseCallback :: Callback
                                         }

data Value = MuString String
           | MuFunction [Reference] M.SubroutineBody
           | MuList [Reference]
           -- | The reference is the reference to the localScope
           | MuNumber Double
           | MuBool Bool
           | MuObject (Map String Reference)
           | MuNull
           deriving (Show, Eq)

defaultContext = ExecutionContext
  { globalObjects = Map.singleton (Reference 1) (MuObject Map.empty)
  , scopes = [Reference 1]
  , currentException = Nothing
  , currentRaiseCallback = \r -> do
      v <- dereference r
      error $ "Exception thrown outside try: " ++ asString v
  , currentReturnCallback = \_r -> error "Called return from outside a function"
  }

asString :: Value -> String
asString (MuString v) = v
asString other        = debug other

debug :: Value -> String
debug (MuString v)   = "(string) " ++ v
debug (MuBool True)  = "(boolean) true"
debug (MuBool False) = "(boolean) false"
debug (MuNumber v)   = "(number) " ++ show v


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

evalExpr (M.Application (M.Primitive O.GreatherOrEqualThan) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 >= n2

evalExpr (M.Application (M.Primitive O.Modulo) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 `mod'` n2
        f params                     = raiseTypeError "expected two numbers" params

evalExpr (M.Application (M.Primitive O.GreatherThan) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 > n2
        f params                     = raiseTypeError "expected two booleans" params

-- TODO make this evaluation non strict on both parameters
evalExpr (M.Application (M.Primitive O.Or) expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b1, MuBool b2] = createReference $ MuBool $ b1 || b2
        f params                 = raiseTypeError "expected two booleans" params

-- TODO make this evaluation non strict on both parameters
evalExpr (M.Application (M.Primitive O.And) expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b1, MuBool b2] = createReference $ MuBool $ b1 && b2
        f params                 = raiseTypeError "expected two booleans" params

evalExpr (M.Application (M.Primitive O.Negation) expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b] = createReference $ MuBool $ not b
        f params     = raiseTypeError "expected one boolean" params

evalExpr (M.Application (M.Primitive O.Multiply) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 * n2

evalExpr (M.Application (M.Primitive O.Equal) expressions) = do
  params <- mapM evalExpr expressions
  let [r1, r2] = params
  muValuesEqual r1 r2

evalExpr (M.Application (M.Primitive O.NotEqual) expressions) = do
  evalExpr $ M.Application (M.Primitive O.Negation) [M.Application (M.Primitive O.Equal) expressions]

evalExpr (M.Application (M.Primitive O.LessOrEqualThan) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 <= n2
        f params                     = raiseTypeError "expected two numbers" params

evalExpr (M.Application (M.Primitive O.LessThan) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 < n2
        f params                     = raiseTypeError "expected two numbers" params

evalExpr (M.Application (M.Primitive O.Plus) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 + n2
        f params                     = raiseTypeError "expected two numbers" params

evalExpr (M.Application (M.Primitive O.Minus) expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 - n2
        f params                     = raiseTypeError "expected two numbers" params

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

evalExpr (M.MuNumber n) = createReference $ MuNumber n
evalExpr (M.MuNil) = return nullRef
evalExpr (M.MuBool b) = createReference $ MuBool b
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
    muBool v              = raiseTypeError "expected boolean" [v]

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
raiseTypeError message values = raiseString $ "Type error: " ++ message ++ " but got " ++ (intercalate ", " . map debug $ values)

muValuesEqual r1 r2
  | r1 == r2 = createReference $ MuBool True
  | otherwise = do
      v1 <- dereference r1
      v2 <- dereference r2
      createReference $ MuBool $ muEquals v1 v2

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

getMaxKey :: Map k a -> Maybe k
getMaxKey m = case Map.maxViewWithKey m of
  Just ((k, _a), _) -> Just k
  _ -> Nothing

nullRef = Reference 0

dereference' :: ObjectSpace -> Reference -> Value
dereference' _ (Reference 0) = MuNull
dereference' objectSpace ref = do
  fromMaybe (error $ "Failed to find ref " ++ show ref ++ " in " ++ show objectSpace) .
    Map.lookup ref $
    objectSpace

dereference :: Reference -> Executable Value
dereference ref = do
  objectSpace <- gets globalObjects
  return $ dereference' objectSpace ref

updateGlobalObjects f context =
  context { globalObjects = f $ globalObjects context }

incrementRef (Reference n) = Reference $ n + 1

createReference :: Value -> Executable Reference
createReference value = do
  nextReferenceId :: Reference <- gets (fromJust . fmap incrementRef . getMaxKey . globalObjects)
  modify (updateGlobalObjects $ Map.insert nextReferenceId value)
  return nextReferenceId

currentFrame :: Executable Reference
currentFrame = gets (head . scopes)

setLocalVariable :: String -> Reference -> Executable ()
setLocalVariable name ref = do
  frame <- currentFrame
  updateRef frame (addAttrToObject name ref)

addAttrToObject :: String -> Reference -> Value -> Value
addAttrToObject k r (MuObject map) = MuObject $ Map.insert k r map
addAttrToObject k _r v = error $ "Tried adding " ++ k ++ " to a non object: " ++ show v

putRef :: Reference -> Value -> Executable ()
putRef ref = modify . updateGlobalObjects . Map.insert ref

updateRef :: Reference -> (Value -> Value) -> Executable ()
updateRef ref f = do
  val <- dereference ref
  putRef ref (f val)

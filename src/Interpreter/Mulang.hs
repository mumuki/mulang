{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter.Mulang where

import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe, fromJust)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Control.Monad (forM)
import           Control.Monad.State.Class
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Control.Monad.Cont
import           Data.Fixed (mod')

import qualified Language.Mulang as Mu

type Executable m = ContT Reference (StateT ExecutionContext IO) m

type Callback = Reference -> Executable ()

newtype Reference = Reference Int deriving (Show, Eq, Ord)

type ObjectSpace = Map Reference Value
type LocalVariables = Map String Reference

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
           | MuFunction [Reference] Mu.SubroutineBody
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
      error $ "Exception thrown outside try: " ++ show v
  , currentReturnCallback = \_r -> error "Called return from outside a function"
  }

eval' :: ExecutionContext -> Executable Reference -> IO (Reference, ExecutionContext)
eval' ctx ref = runStateT (runContT ref return) ctx

eval :: ExecutionContext -> Mu.Expression -> IO (Reference, ExecutionContext)
eval ctx expr = eval' ctx (evalExpr expr)

evalExpressionsWith :: [Mu.Expression] -> ([Value] -> Executable Reference) -> Executable Reference
evalExpressionsWith expressions f = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  f params

evalExpr :: Mu.Expression -> Executable Reference
evalExpr (Mu.Sequence expressions) = last <$> forM expressions evalExpr
evalExpr (Mu.Lambda params body) = do
  executionFrames <- gets scopes
  createReference $
    MuFunction executionFrames [Mu.Equation params (Mu.UnguardedBody body)]

evalExpr (Mu.Subroutine name body) = do
  executionFrames <- gets scopes
  let function = MuFunction executionFrames body
  ref <- createReference function
  unless (null name) (setLocalVariable name ref) -- if function has no name we avoid registering it
  return ref

evalExpr (Mu.Print expression) = do
  parameter :: Value <- evalExpr expression >>= dereference
  liftIO $ print parameter
  return nullRef

evalExpr (Mu.Assert negated (Mu.Truth expression)) =
  evalExpressionsWith [expression] f
  where f [MuBool result]
          | result /= negated = return nullRef
          | otherwise         = raiseString $ "Expected " ++ (show . not $ negated) ++ " but got: " ++ show result

evalExpr (Mu.Assert negated (Mu.Equality expected actual)) =
  evalExpressionsWith [expected, actual] f
  where f [v1, v2]
          | muEquals v1 v2 /= negated = return nullRef
          | otherwise                 = raiseString $ "Expected " ++ show v1 ++ " but got: " ++ show v2

evalExpr (Mu.Application (Mu.Reference ">=") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 >= n2

evalExpr (Mu.Application (Mu.Reference "%") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 `mod'` n2
        f params                     = error $ "Bad parameters, expected two numbers but got " ++ show params

evalExpr (Mu.Application (Mu.Reference ">") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 > n2
        f params                     = error $ "Bad parameters, expected two bools but got " ++ show params

-- TODO make this evaluation non strict on both parameters
evalExpr (Mu.Application (Mu.Reference "||") expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b1, MuBool b2] = createReference $ MuBool $ b1 || b2
        f params                 = error $ "Bad parameters, expected two bools but got " ++ show params

-- TODO make this evaluation non strict on both parameters
evalExpr (Mu.Application (Mu.Reference "&&") expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b1, MuBool b2] = createReference $ MuBool $ b1 && b2
        f params                 = error $ "Bad parameters, expected two bools but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "!") expressions) =
  evalExpressionsWith expressions f
  where f [MuBool b] = createReference $ MuBool $ not b
        f params     = error $ "Bad parameters, expected one bool but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "*") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 * n2

evalExpr (Mu.Application Mu.Equal expressions) = do
  params <- mapM evalExpr expressions
  let [r1, r2] = params
  muValuesEqual r1 r2

evalExpr (Mu.Application Mu.NotEqual expressions) = do
  evalExpr $ Mu.Application (Mu.Reference "!") [Mu.Application Mu.Equal expressions]

evalExpr (Mu.Application (Mu.Reference "<=") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 <= n2
        f params                     = raiseString $ "Bad parameters, expected two numbers but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "<") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuBool $ n1 < n2
        f params                     = raiseString $ "Bad parameters, expected two numbers but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "+") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 + n2

evalExpr (Mu.Application (Mu.Reference "-") expressions) =
  evalExpressionsWith expressions f
  where f [MuNumber n1, MuNumber n2] = createReference $ MuNumber $ n1 - n2

evalExpr (Mu.MuList expressions) = do
  refs <- forM expressions evalExpr
  createReference $ MuList refs

evalExpr (Mu.New klass expressions) = do
  (MuFunction locals ([Mu.SimpleEquation params body])) <- evalExpr klass >>= dereference
  objReference <- createReference $ MuObject Map.empty
  thisContext <- createReference $ MuObject $ Map.singleton "this" objReference
  paramsContext <- evalParams params expressions
  runFunction (thisContext:paramsContext:locals) body
  return objReference

evalExpr (Mu.Application function expressions) = do
  (MuFunction locals ([Mu.SimpleEquation params body])) <- evalExpr function >>= dereference
  paramsContext <- evalParams params expressions
  returnValue <- runFunction (paramsContext:locals) (body)
  return returnValue

evalExpr (Mu.If cond thenBranch elseBranch) = do
  v <- evalExpr cond >>= dereference
  case v of
    MuBool True ->  evalExpr thenBranch
    MuBool False -> evalExpr elseBranch
    _ -> raiseString $ "Got a non boolean on an if: " ++ show v

evalExpr (Mu.MuNumber n) = createReference $ MuNumber n
evalExpr (Mu.MuNil) = return nullRef
evalExpr (Mu.MuBool b) = createReference $ MuBool b
evalExpr (Mu.MuString s) = createReference $ MuString s
evalExpr (Mu.Return e) = do
  ref <- evalExpr e
  currentReturn <- gets (currentReturnCallback)
  currentReturn ref
  return ref -- Unreachable
evalExpr (Mu.Variable name expr) = do
  r <- evalExpr expr
  setLocalVariable name r
  return r

evalExpr (Mu.While cond expr) = do
  whileM (evalCondition cond) (evalExpr expr)
  return nullRef

evalExpr (Mu.ForLoop beforeExpr cond afterExpr expr) = do
  evalExpr beforeExpr
  whileM (evalCondition cond) $ do
    evalExpr expr
    evalExpr afterExpr
  return nullRef

evalExpr (Mu.Assignment name expr) = do
  valueRef <- evalExpr expr
  frameRef <- findFrameForName' name
  case frameRef of
    Just ref -> updateRef ref (addAttrToObject name valueRef)
    Nothing -> setLocalVariable name valueRef
  return valueRef

evalExpr (Mu.Try expr [( Mu.VariablePattern exName, catchExpr)] finallyExpr) = do
  context <- get
  resultRef <- callCC $ \raiseCallback -> do
    put (context { currentRaiseCallback = raiseCallback })
    evalExpr expr
    return nullRef
  lastException <- gets currentException
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

evalExpr (Mu.Raise expr) = raiseInternal =<< evalExpr expr

evalExpr (Mu.Reference name) = findReferenceForName name
evalExpr (Mu.None) = return nullRef
evalExpr e = raiseString $ "Unkown expression: " ++ show e

evalCondition :: Mu.Expression -> Executable Bool
evalCondition cond = evalExpr cond >>= dereference >>= \v -> return (v == MuBool True)

evalParams :: [Mu.Pattern] -> [Mu.Expression] -> Executable Reference
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

getParamNames :: [Mu.Pattern] -> [String]
getParamNames = fmap (\(Mu.VariablePattern n) -> n)

runFunction :: [Reference] -> Mu.Expression -> Executable Reference
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

updateLocalVariables f context =
  context { scopes = f $ scopes context }

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


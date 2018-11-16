{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}

module Interpreter.Mulang where

import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Control.Monad (forM)
import           Control.Monad.State.Class
import           Control.Monad.Loops
import           Control.Monad.State.Strict
import           Control.Monad.Cont
import           Data.Fixed (mod')

import qualified Language.Mulang as Mu



type ExecutionMonad m =
  ((ContT Reference (StateT ExecutionContext IO)) ~ m)
  -- ( MonadIO m
  -- , MonadState ExecutionContext m
  -- , MonadCont m
  -- )

type Callback = Reference -> (ContT Reference (StateT ExecutionContext IO)) ()

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

eval' :: ExecutionMonad m => ExecutionContext -> m Reference -> IO (Reference, ExecutionContext)
eval' ctx elCoso = (`runStateT` ctx) $ (`runContT` return) $ elCoso

eval :: ExecutionContext -> Mu.Expression -> IO (Reference, ExecutionContext)
eval ctx expr = (`runStateT` ctx) $ (`runContT` return) $ (evalExpr expr)

evalExpr :: forall m. (ExecutionMonad m) => Mu.Expression -> m Reference
evalExpr (Mu.Sequence expressions) = last <$> forM expressions evalExpr
evalExpr (Mu.Lambda params body) = do
  executionFrames <- gets scopes
  createReference $
    MuFunction executionFrames $ [Mu.Equation params (Mu.UnguardedBody body)]

evalExpr (Mu.Subroutine name body) = do
  executionFrames <- gets scopes
  let function = MuFunction executionFrames body
  ref <- createReference function
  unless (null name) $ do -- if function has no name we avoid registering it
    setLocalVariable name ref
  return ref

evalExpr (Mu.Send receptor (Mu.Reference msg) paramExprs) = do
  receptorRef <- evalExpr receptor
  propRef <- getAttribute receptorRef
  dereference propRef >>= \case
    (MuFunction locals ([Mu.SimpleEquation params body])) -> do

      parameters :: [Reference] <- mapM evalExpr paramExprs
      let localsAfterParameters = Map.fromList $ zipWith
            (\name ref -> (name, ref))
            (getParamNames params)
            (parameters ++ repeat nullRef)

      contextRef <- createReference $ MuObject localsAfterParameters

      returnValue <- runFunction (contextRef:locals) body
      return returnValue
    _ -> return propRef

  where
    getAttribute ref = do
      dereference ref >>= \case
        (MuObject o) -> do
          maybe (raiseString $ "MNU: " ++ msg) return $ Map.lookup msg o
        v -> raiseString $ "Expected an object but got: " ++ show v

evalExpr (Mu.Application (Mu.Reference "print") expressions) = do
  parameters :: [Value] <- forM expressions (\e -> evalExpr e >>= dereference)
  liftIO $ print parameters
  return $ nullRef

evalExpr (Mu.Application (Mu.Reference "primitive_list_length") expressions) = do
  vals <- mapM (evalExpr >=> dereference) expressions
  case vals of
    [MuList l] ->
      createReference $ MuNumber $ fromIntegral $  length l
    v -> raiseString $ "primitive_list_length expects a primitive array but got: " ++ show v

evalExpr (Mu.Application (Mu.Reference "primitive_assign_prop") expressions) = do
  (refs@[objRef, _propRef, valRef]) <- mapM evalExpr expressions
  [obj, prop, val] <- mapM dereference refs
  case (obj, prop, val) of
    (MuObject _, MuString prop, _) -> do
      updateRef objRef (addAttrToObject prop valRef)
      return valRef
    (_, _, _) -> raiseString $ "Expected assign but got: " ++ show val

evalExpr (Mu.Application (Mu.Reference "primitive_create_list") expressions) = do
  content <- mapM evalExpr expressions
  createReference $ MuList content
evalExpr (Mu.Application (Mu.Reference "primitive_list_push") expressions) = do
  (refs@[listRef, elemRef]) <- mapM evalExpr expressions
  mapM dereference refs >>= \case
    [MuList l, _] -> do
      updateRef listRef $ const (MuList $ l ++ [elemRef])
      return listRef
    ps -> raiseString $ "Expected list and a value but got: " ++ show ps



evalExpr (Mu.Application (Mu.Reference "primitive_list_index") expressions) = do
  mapM (evalExpr >=> dereference) expressions >>= \case
    [MuList l, MuNumber n] -> do
      return $ l !! round n

evalExpr (Mu.Application (Mu.Reference "assert") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  case params of
    [MuBool True] -> return nullRef
    [v] -> raiseString $ "Expected true but got: " ++ show v

evalExpr (Mu.Application (Mu.Reference ">=") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  let [MuNumber n1, MuNumber n2] = params
  createReference $ MuBool $ n1 >= n2

evalExpr (Mu.Application (Mu.Reference "%") expressions) = do
  params <- mapM (evalExpr >=> dereference) expressions
  -- liftIO $ print params
  case params of
    [MuNumber n1, MuNumber n2] -> createReference $ MuNumber $ n1 `mod'` n2
    _ -> error $ "Bad parameters, expected two bools but got " ++ show params

evalExpr (Mu.Application (Mu.Reference ">") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  case params of
    [MuNumber n1, MuNumber n2] -> createReference $ MuBool $ n1 > n2
    _ -> error $ "Bad parameters, expected two bools but got " ++ show params

-- TODO make this evaluation non strict on both parameters
evalExpr (Mu.Application (Mu.Reference "||") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  case params of
    [MuBool b1, MuBool b2] -> createReference $ MuBool $ b1 || b2
    _ -> error $ "Bad parameters, expected two bools but got " ++ show params

-- TODO make this evaluation non strict on both parameters
evalExpr (Mu.Application (Mu.Reference "&&") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  case params of
    [MuBool b1, MuBool b2] -> createReference $ MuBool $ b1 && b2
    _ -> error $ "Bad parameters, expected two bools but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "!") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  case params of
    [MuBool b] -> createReference $ MuBool $ not b
    _ -> error $ "Bad parameters, expected one bool but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "*") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  let [MuNumber n1, MuNumber n2] = params
  createReference $ MuNumber $ n1 * n2

evalExpr (Mu.Application Mu.Equal expressions) = do
  params <- mapM (evalExpr) expressions
  -- liftIO $ print params
  let [r1, r2] = params
  muEquals r1 r2

evalExpr (Mu.Application Mu.NotEqual expressions) = do
  evalExpr $ Mu.Application (Mu.Reference "!") [Mu.Application Mu.Equal expressions]

evalExpr (Mu.Application (Mu.Reference "<=") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  case params of
    [MuNumber n1, MuNumber n2] -> createReference $ MuBool $ n1 <= n2
    _ -> raiseString $ "Bad parameters, expected two numbers but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "<") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  case params of
    [MuNumber n1, MuNumber n2] -> createReference $ MuBool $ n1 < n2
    _ -> raiseString $ "Bad parameters, expected two numbers but got " ++ show params

evalExpr (Mu.Application (Mu.Reference "+") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  let [MuNumber n1, MuNumber n2] = params
  createReference $ MuNumber $ n1 + n2

evalExpr (Mu.Application (Mu.Reference "-") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  let [MuNumber n1, MuNumber n2] = params
  createReference $ MuNumber $ n1 - n2

evalExpr (Mu.MuList expressions) = do
  refs <- forM expressions evalExpr
  list <- createReference $ MuList $ refs
  arrayRef <- evalExpr $ Mu.New (Mu.Reference "Array") []
  updateRef arrayRef $ addAttrToObject "_elements" list
  return arrayRef

evalExpr (Mu.New klass expressions) = do
  (MuFunction locals ([Mu.SimpleEquation params body])) <- evalExpr klass >>= dereference
  objReference <- createReference $ MuObject Map.empty
  thisContext <- createReference $ MuObject $ Map.singleton "this" objReference

  parameters :: [Reference] <- forM expressions evalExpr
  let localsAfterParameters = Map.fromList $ zipWith
        (\name ref -> (name, ref))
        (getParamNames params)
        (parameters ++ repeat nullRef)
  -- change this
  paramsContext <- createReference $ MuObject localsAfterParameters

  runFunction (thisContext:paramsContext:locals) body
  return objReference

evalExpr (Mu.Application function expressions) = do
  (MuFunction locals ([Mu.SimpleEquation params body])) <- evalExpr function >>= dereference

  parameters :: [Reference] <- forM expressions evalExpr
  let localsAfterParameters = Map.fromList $ zipWith
        (\name ref -> (name, ref))
        (getParamNames params)
        (parameters ++ repeat nullRef)
  -- change this
  contextRef <- createReference $ MuObject localsAfterParameters

  returnValue <- runFunction (contextRef:locals) (body)
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
  whileM (evalExpr cond >>= dereference >>= \v -> return (v == MuBool True)) $ do
    evalExpr expr
  return nullRef

evalExpr (Mu.ForLoop beforeExpr condExpr afterExpr expr) = do
  evalExpr beforeExpr
  whileM (evalExpr condExpr >>= dereference >>= \v -> return (v == MuBool True)) $ do
    evalExpr expr
    evalExpr afterExpr
  return nullRef

evalExpr (Mu.Assignment name expr) = do
  valueRef <- evalExpr expr
  frameRef <- findFrameForName' name
  case frameRef of
    Just ref -> do
      updateRef ref (addAttrToObject name valueRef)
    Nothing -> do
      setLocalVariable name valueRef
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

evalExpr (Mu.Raise expr) = do
  raiseInternal =<< evalExpr expr

evalExpr (Mu.Reference name) = findReferenceForName name
evalExpr (Mu.Self) = findReferenceForName "this"
evalExpr (Mu.None) = return nullRef
evalExpr e = raiseString $ "Unkown expression: " ++ show e

raiseInternal :: ExecutionMonad m => Reference -> m b
raiseInternal exceptionRef = do
  raiseCallback <- gets currentRaiseCallback
  modify' (\c -> c {currentException = Just exceptionRef})
  raiseCallback exceptionRef
  raiseString "Unreachable" -- the callback above should never allow this to execute

raiseString :: ExecutionMonad m => String -> m a
raiseString s = do
  raiseInternal =<< (createReference $ MuString s)

muEquals r1 r2
  | r1 == r2 = createReference $ MuBool $ True
  | otherwise = do
      v1 <- dereference r1
      v2 <- dereference r2
      createReference $ case (v1, v2) of
        (MuBool b1, MuBool b2) -> MuBool $ b1 == b2
        (MuNumber n1, MuNumber n2) -> MuBool $ n1 == n2
        (MuString s1, MuString s2) -> MuBool $ s1 == s2
        (MuNull, MuNull) -> MuBool True
        (_, _) -> MuBool $ False

getParamNames :: [Mu.Pattern] -> [String]
getParamNames params =
  fmap (\(Mu.VariablePattern n) -> n) params

runFunction :: (ExecutionMonad m) => [Reference] -> Mu.Expression -> m Reference
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

findFrameForName :: ExecutionMonad m => String -> m Reference
findFrameForName name = do
  maybe (raiseString $ "Reference not found for name '" ++ name ++ "'") return
    =<< findFrameForName' name

findFrameForName' :: ExecutionMonad m => String -> m (Maybe Reference)
findFrameForName' name = do
  framesRefs <- gets scopes
  frames :: [(Reference, Map String Reference)] <- forM framesRefs $ \ref -> do
    dereference ref >>= \case
      (MuObject context) -> return (ref, context)
      v -> error $ "Finding '" ++ name ++ "' the frame I got a non object " ++ show v

  return $ fmap fst <$> find (Map.member name . snd) $ frames


findReferenceForName :: ExecutionMonad m => String -> m Reference
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

dereference :: ExecutionMonad m => Reference -> m Value
dereference ref = do
  objectSpace <- gets globalObjects
  return $ dereference' objectSpace ref

updateGlobalObjects f context =
  context { globalObjects = f $ globalObjects context
          }

updateLocalVariables f context =
  context { scopes = f $ scopes context
          }

incrementRef (Reference n) = Reference $ n + 1

createReference :: (ExecutionMonad m) => Value -> m Reference
createReference value = do
  nextReferenceId :: Reference <- gets (fromMaybe (Reference 10) . fmap incrementRef . getMaxKey . globalObjects)
  modify (updateGlobalObjects $ Map.insert nextReferenceId value)
  return nextReferenceId

currentFrame :: ExecutionMonad m => m Reference
currentFrame = do
  gets (head . scopes)

setLocalVariable :: (ExecutionMonad m) => String -> Reference -> m ()
setLocalVariable name ref = do
  frame <- currentFrame
  updateRef frame (addAttrToObject name ref)

addAttrToObject :: String -> Reference -> Value -> Value
addAttrToObject k r (MuObject map) = MuObject $ Map.insert k r map
addAttrToObject k _r v = error $ "Tried adding " ++ k ++ " to a non object: " ++ show v

putRef :: ExecutionMonad m => Reference -> Value -> m ()
putRef ref val = do
  modify $ updateGlobalObjects $ Map.insert ref val

updateRef :: ExecutionMonad m => Reference -> (Value -> Value) -> m ()
updateRef ref f = do
  val <- dereference ref
  putRef ref (f val)


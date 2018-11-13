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
import           Control.Monad.State.Strict
import           Control.Monad.Cont

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
  show (ExecutionContext globalObjects scopes _) = "ExecutionContext { globalObjects = " ++ show globalObjects++ ", scopes = " ++ show scopes ++" }"

data ExecutionContext = ExecutionContext { globalObjects :: ObjectSpace
                                         , scopes :: [Reference]
                                         , currentReturnCallback :: Callback
                                         }

data Value = MuString String
           | MuFunction [Reference] Mu.SubroutineBody
           -- | The reference is the reference to the localScope
           | MuNumber Double
           | MuBool Bool
           | MuObject (Map String Reference)
           | MuNull
           deriving (Show)

data TestResult = TestResult
                deriving (Show)

eval :: Mu.Expression -> IO (Reference, ExecutionContext)
eval expr = let
    defaultContext = ExecutionContext
      { globalObjects = Map.singleton (Reference 1) (MuObject Map.empty)
      , scopes = [Reference 1]
      , currentReturnCallback = \_r -> (
          error "Called return from outside a function"
            )
      }
  in (`runStateT` defaultContext) $ (`runContT` return) $ (evalExpr expr)

evalExpr :: forall m. (ExecutionMonad m) => Mu.Expression -> m Reference
evalExpr (Mu.Sequence expressions) = last <$> forM expressions evalExpr
evalExpr (Mu.Subroutine name body) = do
  executionFrames <- gets scopes
  let function = MuFunction executionFrames body
  ref <- createReference function
  unless (null name) $ do -- if function has no name we avoid registering it
    setLocalVariable name ref
  return ref

evalExpr (Mu.Application (Mu.Reference "print") expressions) = do
  parameters :: [Value] <- forM expressions (\e -> evalExpr e >>= dereference)
  liftIO $ print parameters
  return $ nullRef

evalExpr (Mu.Application (Mu.Reference ">=") expressions) = do
  params <- mapM (\e -> evalExpr e >>= dereference) expressions
  -- liftIO $ print params
  let [MuNumber n1, MuNumber n2] = params
  createReference $ MuBool $ n1 > n2

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
    _ -> error $ "Got a non boolean on an if: " ++ show v

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

evalExpr (Mu.Assignment name expr) = do
  valueRef <- evalExpr expr
  frameRef <- findFrameForName name
  updateRef frameRef (addAttrToObject name valueRef)
  return valueRef

evalExpr (Mu.Reference name) = findReferenceForName name
evalExpr (Mu.None) = return nullRef
evalExpr e = error $ "Unkown expression: " ++ show e

getParamNames :: [Mu.Pattern] -> [String]
getParamNames params =
  fmap (\(Mu.VariablePattern n) -> n) params

runFunction :: (ExecutionMonad m) => [Reference] -> Mu.Expression -> m Reference
runFunction functionEnv body = do
  context <- get
  returnValue <- callCC $ \(returnCallback) -> do
    put $ ExecutionContext { scopes = functionEnv
                           , currentReturnCallback = returnCallback
                           , globalObjects = globalObjects context
                           }


    -- put (context { scopes = functionEnv, currentReturnCallback = (returnCallback :: Reference -> m b) })
    evalExpr body
    return nullRef
  modify' (\c -> c { scopes = scopes context })
  return returnValue

findFrameForName :: ExecutionMonad m => String -> m Reference
findFrameForName name =
  fromMaybe (error $ "Reference not found for name '" ++ name ++ "'") <$> findFrameForName' name

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

dereference (Reference 0) = return MuNull
dereference ref = do
  objectSpace <- gets globalObjects
  return .
    fromMaybe (error $ "Failed to find ref " ++ show ref ++ " in " ++ show objectSpace) .
    Map.lookup ref $
    objectSpace

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


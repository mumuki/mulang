module Language.Mulang.Interpreter.Internals (
  Executable,
  Reference (..),
  Value (..),
  Callback,
  ExecutionContext (..),
  defaultContext,
  debug,
  createRef,
  dereference,
  dereference',
  modifyRef,
  updateRef,
  updateGlobalObjects) where

import           Language.Mulang.Ast (SubroutineBody)

import           Data.Maybe (fromMaybe, fromJust)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Cont

type Executable m = ContT Reference (StateT ExecutionContext IO) m

newtype Reference = Reference Int deriving (Show, Eq, Ord)

data Value = MuString String
  | MuFunction [Reference] SubroutineBody
  | MuList [Reference]
  -- | The reference is the reference to the localScope
  | MuNumber Double
  | MuBool Bool
  | MuObject (Map String Reference)
  | MuNull
  deriving (Show, Eq)

type Callback = Reference -> Executable ()

type ObjectSpace = Map Reference Value

data ExecutionContext = ExecutionContext {
  globalObjects :: ObjectSpace,
  scopes :: [Reference],
  currentException :: Maybe Reference,
  currentReturnCallback :: Callback,
  currentRaiseCallback :: Callback
}

instance Show ExecutionContext where
  show (ExecutionContext globalObjects scopes _ _ _) =
    "ExecutionContext { globalObjects = " ++ show globalObjects ++ ", scopes = " ++ show scopes ++ " }"

-- ================================
-- Construction of ExecutionContext
-- ================================

defaultContext :: ExecutionContext
defaultContext = ExecutionContext {
  globalObjects = Map.singleton (Reference 1) (MuObject Map.empty),
  scopes = [Reference 1],
  currentException = Nothing,
  currentRaiseCallback = defaultRaiseCallback,
  currentReturnCallback = defaultReturnCallback
}

defaultRaiseCallback :: Callback
defaultRaiseCallback = \r -> do
    v <- dereference r
    error $ "Exception thrown outside try: " ++ asString v

defaultReturnCallback :: Callback
defaultReturnCallback = \_r -> error "Called return from outside a function"

-- ================
-- Values Debugging
-- ================

asString :: Value -> String
asString (MuString v) = v
asString other        = debug other

debug :: Value -> String
debug (MuString v)   = "(string) " ++ v
debug (MuBool True)  = "(boolean) true"
debug (MuBool False) = "(boolean) false"
debug (MuNumber v)   = "(number) " ++ show v

-- ==================
-- Reference Creation
-- ==================

createRef :: Value -> Executable Reference
createRef value = do
  nextReferenceId  <- gets (fromJust . fmap incrementRef . getMaxKey . globalObjects)
  modify (updateGlobalObjects $ Map.insert nextReferenceId value)
  return nextReferenceId

  where
    incrementRef :: Reference -> Reference
    incrementRef (Reference n) = Reference $ n + 1

    getMaxKey :: Map k a -> Maybe k
    getMaxKey m = case Map.maxViewWithKey m of
      Just ((k, _a), _) -> Just k
      _ -> Nothing

-- ====================
-- Reference Resolution
-- ====================

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


-- ================
-- Reference Update
-- ================

updateRef :: Reference -> Value -> Executable ()
updateRef ref = modify . updateGlobalObjects . Map.insert ref

modifyRef :: Reference -> (Value -> Value) -> Executable ()
modifyRef ref f = do
  val <- dereference ref
  updateRef ref (f val)

updateGlobalObjects :: (ObjectSpace -> ObjectSpace) -> ExecutionContext -> ExecutionContext
updateGlobalObjects f context =
  context { globalObjects = f $ globalObjects context }

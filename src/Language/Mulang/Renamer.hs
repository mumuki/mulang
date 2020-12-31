module Language.Mulang.Renamer (rename) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Language.Mulang.Ast
import           Data.Maybe (catMaybes)
import           Control.Monad.State

type RenameState a = State ReferencesMap a

data ReferencesMap = ReferencesMap {
    variables :: Map String String,
    parameters :: Map String String
  } deriving Show

emptyReferencesMap :: ReferencesMap
emptyReferencesMap = ReferencesMap (Map.empty) (Map.empty)

rename :: Expression -> Expression
rename e = evalState (renameState e) emptyReferencesMap


renameState :: Expression -> RenameState Expression
renameState (Application e es)               = do
  e' <- renameState e
  es' <- mapM (renameState) es
  return $ Application e' es'
renameState (Arrow e1 e2)                    = do
  e1' <- renameState e1
  e2' <- renameState e2
  return $ Arrow e1' e2'
renameState (Attribute n e)                  = do
  e' <- renameState e
  return $ Attribute n e'
renameState (Fact n args)                    = do
  return $ Fact n args
renameState (For stms e1)                    = do
  e1' <- renameState e1
  return $ For stms e1'
renameState (Forall e1 e2)                   = do
  e1' <- renameState e1
  e2' <- renameState e2
  return $ Forall e1' e2'
renameState (ForLoop init cond prog stmt)    = do
  init' <- renameState init
  cond' <- renameState cond
  prog' <- renameState prog
  stmt' <- renameState stmt
  return $ ForLoop init' cond' prog' stmt'
renameState (Function n equations)           = do
  equations' <- renameEquations equations
  return $ Function n equations'
renameState (If e1 e2 e3)                    = do
  e1' <- renameState e1
  e2' <- renameState e2
  e3' <- renameState e3
  return $ If e1' e2' e3'
renameState (Lambda ps e2)                   = do
  e2' <- renameState e2
  return $ Lambda ps e2'
renameState (Match e1 equations)             = do
  e1' <- renameState e1
  equations' <- renameEquations equations
  return $ Match e1' equations'
renameState (Method n equations)             = do
  equations' <- renameEquations equations
  return $ Method n equations'
renameState (MuDict e)                       = do
  e' <- renameState e
  return $ MuDict e'
renameState (MuList es)                      = do
  es' <- mapM (renameState) es
  return $ MuList es'
renameState (MuObject e)                     = do
  e' <- renameState e
  return $ MuObject e'
renameState (MuTuple es)                     = do
  es' <- mapM (renameState) es
  return $ MuTuple es'
renameState (Not e)                          = do
  e' <- renameState e
  return $ Not e'
renameState (Object n e)                     = do
  e' <- renameState e
  return $ Object n e'
renameState (Procedure n equations)          = do
  equations' <- renameEquations equations
  return $ Procedure n equations'
renameState (Return e)                       = do
  e' <- renameState e
  return $ Return e'
renameState (Rule n args es)                 = do
  es' <- mapM (renameState) es
  return $ Rule n args es'
renameState (Send r e es)                    = do
  r' <- renameState r
  e' <- renameState e
  es' <- mapM (renameState) es
  return $ Send r' e' es'
renameState (Sequence es)                    = do
  es' <- mapM renameState es
  return $ Sequence es'
renameState (Other n (Just e))               = do
  e' <- renameState e
  return $ Other n (Just e')
renameState (While e1 e2)                    = do
  e1' <- renameState e1
  e2' <- renameState e2
  return $ While e1' e2'
renameState (Yield e)                        = do
  e' <- renameState e
  return $ Yield e'
renameState (TypeCast e t)                   = do
  e' <- renameState e
  return $ TypeCast e' t
renameState (Assignment i e)                 = do
  e' <- renameState e
  return $ Assignment i e'
renameState (Break e)                        = do
  e' <- renameState e
  return $ Break e'
renameState (Class i pi e)                   = do
  e' <- renameState e
  return $ Class i pi e'
renameState (Continue e)                     = do
  e' <- renameState e
  return $ Continue e'
renameState (EntryPoint i e)                 = do
  e' <- renameState e
  return $ EntryPoint i e'
renameState (EqualMethod equations)          = do
  equations' <- renameEquations equations
  return $ EqualMethod equations'
renameState (FieldAssignment e1 i e2)        = do
  e1' <- renameState e1
  e2' <- renameState e2
  return $ FieldAssignment e1' i e2'
renameState (FieldReference e i)             = do
  e' <- renameState e
  return $ FieldReference e' i
renameState (Findall e1 e2 e3)               = do
  e1' <- renameState e1
  e2' <- renameState e2
  e3' <- renameState e3
  return $ Findall e1' e2' e3'
renameState (HashMethod equations)           = do
  equations' <- renameEquations equations
  return $ HashMethod equations'
renameState (Implement e)                    = do
  e' <- renameState e
  return $ Implement e'
renameState (Include e)                      = do
  e' <- renameState e
  return $ Include e'
renameState (Interface i is e)               = do
  e' <- renameState e
  return $ Interface i is e'
renameState (New e es)                       = do
  e' <- renameState e
  es' <- mapM renameState es
  return $ New e' es'
renameState (PrimitiveMethod op equations)   = do
  equations' <- renameEquations equations
  return $ PrimitiveMethod op equations'
renameState (Print e)                        = do
  e' <- renameState e
  return $ Print e'
renameState (Raise e)                        = do
  e' <- renameState e
  return $ Raise e'
renameState (Repeat e1 e2)                   = do
  e1' <- renameState e1
  e2' <- renameState e2
  return $ Repeat e1' e2'
renameState (Variable n e)                   = renameVariable n e
renameState (Reference r)                    = renameReference r
renameState e                                = return e

renameEquations :: [Equation] -> RenameState [Equation]
renameEquations equations = do
  m <- get
  equations' <- mapM renameEquation equations
  put m
  return equations'

renameEquation :: Equation -> RenameState Equation
renameEquation (Equation ps b) = do
  ps' <- renameParameters ps
  b' <- renameEquationBody b
  return $ Equation ps' b'

renameParameters :: [Pattern] -> RenameState [Pattern]
renameParameters [p] = do
  p' <- renameParameter p
  return [p']
renameParameters (p:ps) = do
  p'<- renameParameter p
  ps' <- renameParameters ps
  return (p':ps')

renameParameter :: Pattern -> RenameState Pattern
renameParameter (VariablePattern n) = fmap VariablePattern . createParameter $ n
renameParameter e                   = return e

renameEquationBody (UnguardedBody e) = fmap UnguardedBody . renameState $ e
renameEquationBody (GuardedBody es)  = fmap GuardedBody . mapM renameGuard $ es
  where
    renameGuard (e1, e2) = do
      e1' <- renameState e1
      e2' <- renameState e2
      return (e1', e2')

renameVariable :: String -> Expression -> RenameState Expression
renameVariable n e = do
  n' <- createVariable n
  e1' <- renameState e
  return $ Variable n' e1'

renameReference :: String -> RenameState Expression
renameReference n = do
    m <- get
    return . Reference . head . catMaybes $ [lookupVariable n m, lookupParameter n m, Just n]

createVariable :: String -> RenameState String
createVariable n = do
    m <- get
    let n' = makeRef "mulang_var_n" variables m
    put (m { variables = insertRef n n' variables m })
    return n'

createParameter :: String -> RenameState String
createParameter n = do
    m <- get
    let n' = makeRef "mulang_param_n" parameters m
    put (m { parameters = insertRef n n' parameters m })
    return n'

makeRef :: String -> (ReferencesMap -> Map String String) -> ReferencesMap -> String
makeRef kind f = (kind++) . show . length . f

insertRef :: String -> String -> (ReferencesMap -> Map String String) -> ReferencesMap -> Map String String
insertRef n n' f = Map.insert n n' . f

lookupVariable :: String -> ReferencesMap -> (Maybe String)
lookupVariable n m = Map.lookup n (variables m)

lookupParameter :: String -> ReferencesMap -> (Maybe String)
lookupParameter n m = Map.lookup n (parameters m)

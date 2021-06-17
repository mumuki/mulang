module Language.Mulang.Transform.Renamer (rename) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Language.Mulang.Ast
import           Language.Mulang.Ast.Visitor
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
renameState (Reference r)       = renameReference r
renameState (Variable n e)      = renameVariable n e
renameState e@(Exist _ _)       = return e
renameState f@(Fact _ _)        = return f
renameState f@(Findall _ _ _)   = return f
renameState f@(Forall _ _)      = return f
renameState n@(Not _)           = return n
--
renameState (For stms e1)       = do { stms' <- mapM renameStatement stms; e1' <- renameState e1; return $ For stms' e1' }
renameState (ForLoop i c a b)   = do { [i', c', a', b'] <- mapM renameState [i, c, a, b]; return $ ForLoop i' c' a' b' }
renameState (Lambda ps e2)      = do { e2' <- renameState e2; return $ Lambda ps e2' }
renameState (Match e1 eqs)      = do { e1' <- renameState e1; eqs' <- renameEquations eqs; return $ Match e1' eqs' }
renameState (Send r e es)       = do { (r':e':es') <- mapM renameState (r:e:es); return $ Send r' e' es' }
renameState (Switch v cs d)     = do { v' <- renameState v; cs' <- renameSwitchCases cs; d' <- renameState d; return $ Switch v' cs' d' }
renameState (Try t cs f)        = do { t' <- renameState t; cs' <- renameTryCases cs; f' <- renameState f; return $ Try t' cs' f' }
renameState (RecordUpdate r us) = do { r' <- renameState r; us' <- renameRecordUpdates us; return $ RecordUpdate r' us' }
renameState a@(Assert _ _)      = return a
renameState r@(Rule _ _ _)      = return r
--
renameState (ExpressionAndExpressionsList e es c)  = do { (e':es') <- mapM renameState (e:es); return $ c e' es' }
renameState (SingleEquationsList eqs c)            = do { eqs' <- renameEquations eqs; return $ c eqs' }
renameState (SingleExpression e c)                 = do { e' <- renameState e; return $ c e' }
renameState (SingleExpressionsList es c)           = do { es' <- mapM renameState es; return $ c es' }
renameState (ThreeExpressions e1 e2 e3 c)          = do { [e1', e2', e3'] <- mapM renameState [e1, e2, e3]; return $ c e1' e2' e3' }
renameState (TwoExpressions e1 e2 c)               = do { e1' <- renameState e1; e2' <- renameState e2; return $ c e1' e2' }
renameState e@(SinglePatternsList _ _)             = return e
renameState e@Terminal                             = return e

renameRecordUpdates = mapM (\(i, e) -> do { e' <- renameState e; return (i, e') })
renameTryCases      = mapM (\(p, e) -> do { e' <- renameState e; return (p, e') })
renameSwitchCases   = mapM (\(e1, e2) -> do { e1' <- renameState e1; e2' <- renameState e2; return (e1', e2') })

renameStatement :: Statement -> RenameState Statement
renameStatement (Generator p e)  = do { p' <- renameParameter p; e' <- renameState e; return $ Generator p' e' }
renameStatement (Guard e)        = do { e' <- renameState e; return $ Guard e' }

renameEquations :: [Equation] -> RenameState [Equation]
renameEquations equations = do
  m <- get
  equations' <- mapM renameEquation equations
  put m
  return equations'

renameEquation :: Equation -> RenameState Equation
renameEquation (Equation ps b) = do
  ps' <- mapM renameParameter ps
  b' <- renameEquationBody b
  return $ Equation ps' b'

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

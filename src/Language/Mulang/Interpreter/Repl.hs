module Language.Mulang.Interpreter.Repl (
  Session (..),
  newSession,
  repl,
  dump,
  load,
  reload) where

import           Language.Mulang.Ast (Expression)
import           Language.Mulang.Interpreter (eval)
import           Language.Mulang.Interpreter.Internals (Value, Reference (..), ExecutionContext (..), defaultContext)

import qualified Data.Map as Map
import           Data.Map ((!))

type SessionState = ([(Int, Value)], [Int])
type Language = (String -> Expression)

data Session = Session { language :: Language, context :: ExecutionContext }

newSession :: Language -> Session
newSession language = Session language defaultContext

repl :: String -> Session -> IO (Value, Session)
repl line session = do
  (ref, newContext) <- eval (context session) (language session line)
  return (globalObjects newContext ! ref, session { context = newContext } )

dump :: Session -> SessionState
dump (Session _ (ExecutionContext globals scopes _ _ _ )) = (dumpGlobals globals, dumpScopes scopes)
  where
    dumpGlobals = Map.toList . Map.mapKeys asInt
    dumpScopes = map asInt

load :: Language -> SessionState -> Session
load language (globalsState, scopesState) = Session language (defaultContext { globalObjects = loadGlobals globalsState, scopes = loadScopes scopesState } )
  where
    loadGlobals = Map.mapKeys fromInt . Map.fromList
    loadScopes  = map fromInt

reload :: Session -> Session
reload s@(Session l _) = load l (dump s)

fromInt :: Int -> Reference
fromInt = Reference

asInt :: Reference -> Int
asInt (Reference i) = i

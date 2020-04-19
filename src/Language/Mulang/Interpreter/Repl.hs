module Language.Mulang.Interpreter.Repl (
  repl,
  newSession) where

import Data.Map ((!))
import Language.Mulang.Interpreter (Reference, ExecutionContext (..), globalObjects, defaultContext, eval)

type SessionState = ([(Int, Value)], [Int])
type SessionLanguage = (String -> Expression)

data Session = Session { language :: Language, context :: ExecutionContext }

newSession :: Language -> Session
newSession language = Session language defaultContext

repl :: Session -> String -> IO (Session, Value)
repl session line = do
  (ref, newContext) <- eval (context session) (language . session $ line)
  return (globalObjects newContext ! ref, newContext)

dump :: Session -> SessionState
dump (Session _ (ExecutationContext globals scopes _ _ _ )) = (dumpGlobals globals, dumpScopes scopes)
  where
    dumpGlobals = Map.toList . Map.mapKeys asInt
    dumpScopes = map asInt

load :: SessionLanguage -> SessionState -> Session
load language (globalsState, scopesState) = Session language (defaultContext { globalObjects = loadGlobals state, scopes = loadScopes state } )
  where
    loadGlobals = Map.fromList . map fromInt
    loadScopes  = map fromInt

fromInt :: Int -> Reference
fromInt = Reference

toInt :: Reference -> Int
toInt (Reference i) = i

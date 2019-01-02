module Language.Mulang.Inspector.Query (
  inspect,
  select,
  selectCount,
  Query) where

import Control.Monad (guard)

import Language.Mulang.Ast (Expression)

type Query a = [a]

inspect :: Query () -> Bool
inspect = not.null

select :: Bool -> Query ()
select = guard

selectCount :: (Int -> Bool) -> Query Expression -> Query ()
selectCount condition list = select (condition . length $ list)

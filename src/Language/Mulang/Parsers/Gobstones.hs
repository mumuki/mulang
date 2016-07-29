module Language.Mulang.Parsers.Gobstones (translateGobstonesAst,GobstonesAst) where 

import  Language.Mulang

{-
instance FromJSON NodeAst where
  parseJSON (Object v) =
    Node <$>
    (v .: "Alias")                  <*>
    (v .: "Body")	                <*>
    (v .: "From")


instance FromJSON GobstonesAst where
	parseJSON (Object list) = (map parseJSON list)  --no se si esto funciona, tal ves asuma recursion.
    parseJSON 		_       = empty
-}

type GobstonesAst = [NodeAst]

data NodeAst = Node Alias Body From 

data Alias = ProgramGobstones

data Body = Null 

type From = Int



translateGobstonesAst :: GobstonesAst -> Expression
translateGobstonesAst  ast = Program (map translateNodeAst ast) --TODO : no estoy seguro si deberia ser asi o como abajo 

translateNodeAst :: NodeAst -> Expression
translateNodeAst (Node ProgramGobstones Null _) = Program []
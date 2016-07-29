module Language.Mulang.Parsers.Gobstones (
	translateGobstonesAst,
	GobstonesAst) where 

import  Language.Mulang


instance FromJSON NodeAst where
	parseJSON (Object v) =
    Node <$>
    (v .: "Alias")     <*>
    (v .: "Body")     <*>
    (v .: "From")


instance FromJSON GobstonesAst where
	parseJSON (Object list) = map parseJSON list  --no se si esto funciona, tal ves asuma recursion.
    parseJSON 		_       = empty


type GobstonesAst =  [NodeAst]

data NodeAst = Node Alias Body From 

data Alias = Program

data Body = Null 

type From = Int



translateGobstonesAst :: GobstonesAst -> Expression
translateGobstonesAst  = map translateNodeAst 

translateNodeAst :: NodeAst -> Expression
translateNodeAst (Node Program Null _) = Mulang.Program []
module Language.Mulang.Parsers.Haskell (hs, parseHaskell) where

import Language.Mulang.Ast
import Language.Mulang.Operators.Haskell (haskellTokensTable)
import Language.Mulang.Operators (parseOperator)
import Language.Mulang.Builder (compact, normalizeWith, defaultNormalizationOptions, NormalizationOptions(..), SequenceSortMode(..))
import Language.Mulang.Parsers

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty (prettyPrint)

import Data.List (intercalate)

import Control.Fallible

instance Fallible ParseResult where
  failure (ParseOk v)       = Left v
  failure (ParseFailed _ m) = Right m

hs :: Parser
hs = orFail . parseHaskell'

parseHaskell :: EitherParser
parseHaskell = orLeft . parseHaskell'

parseHaskell' :: String -> ParseResult Expression
parseHaskell' = fmap (normalize . mu) . parseModule . (++"\n")
    where normalize = normalizeWith (defaultNormalizationOptions { sortSequenceDeclarations = SortAll })

mu :: HsModule -> Expression
mu (HsModule _ _ _ _ decls) = compact (concatMap muDecls decls)
  where
    mergeDecls decls exp = compact (decls ++ [exp])

    muDecls (HsTypeDecl _ name args t)   = [TypeAlias (unwords . map muName $ name : args) (muTypeId t)]
    muDecls (HsDataDecl _ _ name _ _ _ ) = [Record (muName name)]
    muDecls (HsTypeSig _ names (HsQualType constraints t))
                                         = map (muTypeSignature constraints t) names
    muDecls (HsFunBind equations) | (HsMatch _ name _ _ _) <- head equations =
                                        [Function (muName name) (map muEquation equations)]
    muDecls (HsPatBind _ (HsPVar name) (HsUnGuardedRhs exp) _) = [Variable (muName name) (muExp exp)]
    muDecls _ = []

    muEquation :: HsMatch -> Equation
    muEquation (HsMatch _ _ patterns rhs decls) =
         Equation (map muPat patterns) (muRhs (concatMap muDecls decls) rhs)

    muRhs decls (HsUnGuardedRhs body)        = UnguardedBody (mergeDecls decls (muBody body))
    muRhs decls (HsGuardedRhss  guards)      = GuardedBody (map (muGuardedRhs decls) guards )

    muGuardedRhs decls (HsGuardedRhs _ condition body) = (mergeDecls decls (muExp condition), muBody body)

    muBody = Return . muExp

    muPat (HsPVar name) = VariablePattern (muName name)
    muPat (HsPLit literal) = LiteralPattern (prettyPrint literal)
    muPat (HsPInfixApp e1 name e2) = InfixApplicationPattern (muPat e1) (muQName name) (muPat e2)
    muPat (HsPApp name elements) = ApplicationPattern (muQName name) (map muPat elements)
    muPat (HsPTuple elements) = TuplePattern (map muPat elements)
    muPat (HsPList elements) = ListPattern (map muPat elements)
    muPat (HsPParen pattern) = muPat pattern
    muPat (HsPAsPat name pattern) = AsPattern (muName name) (muPat pattern)
    muPat HsPWildCard = WildcardPattern
    muPat p = debugPattern p

    muExp (HsVar (UnQual (HsIdent "undefined"))) = Raise (MuString "undefined")

    muExp (HsVar name) = muVar (muQName name)
    muExp (HsCon (UnQual (HsIdent "True")))  = MuTrue
    muExp (HsCon (UnQual (HsIdent "False"))) = MuFalse
    muExp (HsCon name)                       = Reference (muQName name)
    muExp (HsLit lit) = muLit lit

    muExp (HsInfixApp e1 op e2)                                  = Application ((muVar.muQOp) op) [muExp e1, muExp e2]
    muExp (HsApp (HsVar (UnQual (HsIdent "error"))) e1)          = Raise (muExp e1)
    muExp (HsApp (HsApp (HsApp (HsApp e1 e2) e3) e4) e5)         = Application (muExp e1) [muExp e2, muExp e3, muExp e4, muExp e5]
    muExp (HsApp (HsApp (HsApp e1 e2) e3) e4)                    = Application (muExp e1) [muExp e2, muExp e3, muExp e4]
    muExp (HsApp (HsApp e1 e2) e3)                               = Application (muExp e1) [muExp e2, muExp e3]
    muExp (HsApp e1 e2)                                          = Application (muExp e1) [muExp e2]
    muExp (HsLeftSection e1 e2)                                  = Application (muVar $ muQOp e2) [muExp e1]
    muExp (HsRightSection e1 e2)                                 = Application (Reference "flip") [muVar $ muQOp e1, muExp e2]
    muExp (HsNegApp e) = Application (Reference "-") [muExp e]
    muExp (HsLambda _ args body) = Lambda (map muPat args) (muBody body)
    --muExp HsLet = Let [Declaration] Expression          -- ^ local declarations with @let@
    muExp (HsIf e1 e2 e3) = If (muExp e1) (muExp e2) (muExp e3)
    --muExp HsMatch = Match Expression [Alternative]          -- ^ @case@ /exp/ @of@ /alts/
    muExp (HsTuple elements) = MuTuple (map muExp elements)               -- ^ tuple Expression
    muExp (HsList elements) = MuList (map muExp elements)
    muExp (HsParen e) = (muExp e)
    muExp (HsEnumFrom from)              = Application (Reference "enumFrom") [(muExp from)]
    muExp (HsEnumFromTo from to)         = Application (Reference "enumFromTo") [(muExp from), (muExp to)]
    muExp (HsEnumFromThen from thn)      = Application (Reference "enumFromThen") [(muExp from), (muExp thn)]
    muExp (HsEnumFromThenTo from thn to) = Application (Reference "enumFromThenTo") [(muExp from), (muExp thn), (muExp to)]
    muExp (HsListComp exp stmts)         = For (map muStmt stmts) (Yield (muExp exp))
    muExp (HsDo stmts) | (HsQualifier exp) <- last stmts  = For (map muStmt stmts)  (Yield (muExp exp))
    muExp (HsExpTypeSig _ exp (HsQualType cs t))          = TypeCast (muExp exp) (muType t cs)
    muExp e = debug e

    muLit (HsCharPrim    v) = MuChar v
    muLit (HsStringPrim  v) = MuString v
    muLit (HsChar        v) = MuChar v
    muLit (HsString      v) = MuString v
    muLit (HsIntPrim     v) = MuNumber . fromIntegral $ v
    muLit (HsInt         v) = MuNumber . fromIntegral $ v
    muLit (HsFrac        v) = MuNumber . fromRational $ v
    muLit (HsFloatPrim   v) = MuNumber . fromRational $ v
    muLit (HsDoublePrim  v) = MuNumber . fromRational $ v

    muVar :: String -> Expression
    muVar v | (Just op) <- parseOperator v haskellTokensTable = Primitive op
    muVar v = Reference v

    muName :: HsName -> String
    muName (HsSymbol n) = n
    muName (HsIdent  n) = n

    muQName (Qual _ n) = muName n
    muQName (UnQual n) = muName n
    muQName (Special HsUnitCon) = "()"
    muQName (Special HsListCon) = "[]"
    muQName (Special HsFunCon) =  "->"
    muQName (Special (HsTupleCon times)) =  intercalate "" . replicate times $ ","
    muQName (Special (HsCons)) =  ":"

    muQOp (HsQVarOp name) = muQName name
    muQOp (HsQConOp name) = muQName name

    muStmt (HsGenerator _ pat exp) = Generator (muPat pat) (muExp exp)
    muStmt (HsQualifier exp)       = Guard (muExp exp)

    muTypeSignature :: [HsAsst] -> HsType -> HsName -> Expression
    muTypeSignature cs t name = TypeSignature (muName name) (muType t cs)

    muType :: HsType -> [HsAsst] -> Type
    muType t cs | null initTypes = SimpleType lastType constraints
                | otherwise      = ParameterizedType initTypes lastType constraints
      where
        initTypes   = init topTypes
        lastType    = last topTypes
        topTypes    = muTopTypes t
        constraints = map muConstraint cs

    muConstraint :: HsAsst -> Identifier
    muConstraint (constraint, targets) =
        intercalate " " (muQName constraint : map muTypeId targets)

    muTopTypes (HsTyFun i o) = muTypeId i : muTopTypes o
    muTopTypes t             = [muTypeId t]

    muTypeId :: HsType -> Identifier
    muTypeId (HsTyFun i o)                              = muTypeId i ++ " -> " ++ muTypeId o
    muTypeId (HsTyCon name)                             = muQName name
    muTypeId (HsTyVar name)                             = muName name
    muTypeId (HsTyTuple ts)                             = "(" ++ (intercalate ", " . map muTypeId $ ts) ++ ")"
    muTypeId (HsTyApp (HsTyCon (Special HsListCon)) t2) = "[" ++ muTypeId t2 ++ "]"
    muTypeId (HsTyApp t1 t2)                            = muTypeId t1 ++ " " ++ muTypeId t2


module Language.Mulang.Parsers.Haskell (hs, parseHaskell) where

import Language.Mulang
import Language.Mulang.Builder
import Language.Mulang.Parsers

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Data.List (intercalate)

import Control.Fallible

instance Fallible ParseResult where
  failure (ParseOk v)       = Left v
  failure (ParseFailed _ m) = Right m

hs :: Parser
hs = orFail . parseHaskell'

parseHaskell :: MaybeParser
parseHaskell = orNothing . parseHaskell'

parseHaskell' :: String -> ParseResult Expression
parseHaskell' = fmap (normalize . mu) . parseModule

mu :: HsModule -> Expression
mu (HsModule _ _ _ _ decls) = compact (concatMap muDecls decls)
  where
    mergeDecls decls exp = compact (decls ++ [exp])

    muDecls (HsTypeDecl _ name _ _)      = [TypeAliasDeclaration (muName name)]
    muDecls (HsDataDecl _ _ name _ _ _ ) = [RecordDeclaration (muName name)]
    muDecls (HsTypeSig _ names _) = map (\name -> TypeSignature (muName name)) names
    muDecls (HsFunBind equations) | (HsMatch _ name _ _ _) <- head equations =
                                        [FunctionDeclaration (muName name) (map muEquation equations)]
    muDecls (HsPatBind _ (HsPVar name) (HsUnGuardedRhs exp) _) = [VariableDeclaration (muName name) (muExp exp)]
    muDecls _ = []

    muEquation :: HsMatch -> Equation
    muEquation (HsMatch _ _ patterns rhs decls) =
         Equation (map muPat patterns) (muRhs (concatMap muDecls decls) rhs)

    muRhs decls (HsUnGuardedRhs body)        = UnguardedBody (mergeDecls decls (muBody body))
    muRhs decls (HsGuardedRhss  guards)      = GuardedBody (map (muGuardedRhs decls) guards )

    muGuardedRhs decls (HsGuardedRhs _ condition body) = (mergeDecls decls (muExp condition), muBody body)

    muBody = Return . muExp

    muPat (HsPVar name) = VariablePattern (muName name)                 -- ^ variable
    muPat (HsPLit _) = LiteralPattern ""              -- ^ literal constant
    --Pattern HsPInfixApp = InfixApplicationPattern Pattern MuQName Pattern
    --Pattern HsPApp = ApplicationPattern MuQName [Pattern]        -- ^ data constructor and argument
    muPat (HsPTuple elements) = TuplePattern (map muPat elements)
    muPat (HsPList elements) = ListPattern (map muPat elements)
    muPat (HsPParen pattern) = muPat pattern
    --Pattern HsPAsPat = AsPattern String Pattern
    muPat HsPWildCard = WildcardPattern
    muPat _ = OtherPattern

    muExp (HsVar name) = muVar (muQName name)
    muExp (HsCon (UnQual (HsIdent "True")))  = MuBool True
    muExp (HsCon (UnQual (HsIdent "False"))) = MuBool False
    muExp (HsCon name)                       = Variable (muQName name)
    muExp (HsLit lit) = muLit lit
    muExp (HsInfixApp e1 op e2)                                  = Application ((muVar.muQOp) op) [muExp e1, muExp e2]
    muExp (HsApp (HsApp (HsApp (HsApp e1 e2) e3) e4) e5)         = Application (muExp e1) [muExp e2, muExp e3, muExp e4, muExp e5]
    muExp (HsApp (HsApp (HsApp e1 e2) e3) e4)                    = Application (muExp e1) [muExp e2, muExp e3, muExp e4]
    muExp (HsApp (HsApp e1 e2) e3)                               = Application (muExp e1) [muExp e2, muExp e3]
    muExp (HsApp e1 e2)                                          = Application (muExp e1) [muExp e2]
    muExp (HsNegApp e) = Application (Variable "-") [muExp e]
    muExp (HsLambda _ args body) = Lambda (map muPat args) (muBody body)
    --muExp HsLet = Let [Declaration] Expression          -- ^ local declarations with @let@
    muExp (HsIf e1 e2 e3) = If (muExp e1) (muExp e2) (muExp e3)
    --muExp HsMatch = Match Expression [Alternative]          -- ^ @case@ /exp/ @of@ /alts/
    muExp (HsTuple elements) = MuTuple (map muExp elements)               -- ^ tuple Expression
    muExp (HsList elements) = MuList (map muExp elements)
    muExp (HsParen e) = (muExp e)
    muExp (HsEnumFrom from)              = Application (Variable "enumFrom") [(muExp from)]
    muExp (HsEnumFromTo from to)         = Application (Variable "enumFromTo") [(muExp from), (muExp to)]
    muExp (HsEnumFromThen from thn)      = Application (Variable "enumFromThen") [(muExp from), (muExp thn)]
    muExp (HsEnumFromThenTo from thn to) = Application (Variable "enumFromThenTo") [(muExp from), (muExp thn), (muExp to)]
    muExp (HsListComp exp stmts)         = Comprehension (muExp exp) (map muStmt stmts)
    muExp (HsDo stmts) | (HsQualifier exp) <- last stmts  = Comprehension (muExp exp) (map muStmt stmts)
    muExp _ = ExpressionOther

    muLit (HsCharPrim    v) = MuString [v]
    muLit (HsStringPrim  v) = MuString v
    muLit (HsChar        v) = MuString [v]
    muLit (HsString      v) = MuString v
    muLit (HsIntPrim     v) = MuNumber . fromIntegral $ v
    muLit (HsInt         v) = MuNumber . fromIntegral $ v
    muLit (HsFrac        v) = MuNumber . fromRational $ v
    muLit (HsFloatPrim   v) = MuNumber . fromRational $ v
    muLit (HsDoublePrim  v) = MuNumber . fromRational $ v

    muVar :: String -> Expression
    muVar "==" = Equal
    muVar "/=" = NotEqual
    muVar v = Variable v

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

    muStmt (HsGenerator _ pat exp) = MuGenerator (muPat pat) (muExp exp)
    muStmt (HsQualifier exp) = MuQualifier (muExp exp)
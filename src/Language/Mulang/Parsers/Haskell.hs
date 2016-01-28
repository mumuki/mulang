module Language.Mulang.Parsers.Haskell (parseHaskell) where

import Language.Mulang
import Language.Haskell.Syntax
import Language.Haskell.Parser

import Data.String (IsString(..))
import Data.Maybe (fromJust)
import Data.List (intercalate)

instance IsString Expression where
  fromString = fromJust.parseHaskell

parseHaskell :: String -> Maybe Expression
parseHaskell code | ParseOk ast <- parseModule code = Just (mu ast)
                  | otherwise = Nothing

mu :: HsModule -> Expression
mu (HsModule _ _ _ _ decls) = Sequence (map (DeclarationExpression) $ concatMap muDecls decls)
  where
    muDecls (HsTypeDecl _ name _ _)      = [TypeAlias (muName name)]
    muDecls (HsDataDecl _ _ name _ _ _ ) = [RecordDeclaration (muName name)]
    muDecls (HsTypeSig _ names _) = map (\name -> TypeSignature (muName name)) names
    muDecls (HsFunBind equations) | (HsMatch _ name _ _ _) <- head equations =
                                        [FunctionDeclaration (muName name) (map muEquation equations)]
    muDecls (HsPatBind _ (HsPVar name) rhs _) = [ConstantDeclaration (muName name) (muRhs rhs)]
    muDecls _ = []

    muEquation :: HsMatch -> Equation
    muEquation (HsMatch _ _ patterns rhs _) =
         Equation (map muPat patterns) (muRhs rhs)

    muRhs (HsUnGuardedRhs exp)          = UnguardedRhs (muExp exp)
    muRhs (HsGuardedRhss  guards) = GuardedRhss (map muGuardedRhs guards)

    muGuardedRhs (HsGuardedRhs _ condition body) = (GuardedRhs (muExp condition) (muExp body))

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

    muExp (HsVar name) = Variable (muQName name)
    muExp (HsCon (UnQual (HsIdent "True")))  = Literal (MuBool True)
    muExp (HsCon (UnQual (HsIdent "False"))) = Literal (MuBool False)
    muExp (HsCon name)                       = Variable (muQName name)
    muExp (HsLit lit) = Literal (muLit lit)
    muExp (HsInfixApp e1 op e2) = InfixApplication (muExp e1) (muQOp op) (muExp e2)  -- ^ infix application
    muExp (HsApp e1 e2) = Application (muExp e1) (muExp e2)             -- ^ ordinary application
    muExp (HsNegApp e) = Application (Variable "-") (muExp e)
    muExp (HsLambda _ args exp) = Lambda (map muPat args) (muExp exp)
    --muExp HsLet = Let [Declaration] Expression          -- ^ local declarations with @let@
    muExp (HsIf e1 e2 e3) = If (muExp e1) (muExp e2) (muExp e3)
    --muExp HsMatch = Match Expression [Alternative]          -- ^ @case@ /exp/ @of@ /alts/
    muExp (HsTuple elements) = MuTuple (map muExp elements)               -- ^ tuple Expression
    muExp (HsList elements) = MuList (map muExp elements)
    muExp (HsParen e) = (muExp e)
    muExp (HsEnumFrom from)              = Application (Variable "enumFrom") (muExp from)
    muExp (HsEnumFromTo from to)         = Application (Application (Variable "enumFromTo") (muExp from)) (muExp to)
    muExp (HsEnumFromThen from thn)      = Application (Application (Variable "enumFromThen") (muExp from)) (muExp thn)
    muExp (HsEnumFromThenTo from thn to) = Application (Application (Application (Variable "enumFromThenTo") (muExp from)) (muExp thn)) (muExp to)
    muExp (HsListComp exp stmts)         = Comprehension (muExp exp) (map muStmt stmts)
    muExp (HsDo stmts) | (HsQualifier exp) <- last stmts  = Comprehension (muExp exp) (map muStmt stmts)
    muExp _ = ExpressionOther

    muLit (HsChar        v) = MuString [v]
    muLit (HsString      v) = MuString v
    muLit (HsInt         v) = MuInteger v
    muLit (HsFrac        v) = MuFloat . fromRational $ v
    muLit (HsCharPrim    v) = MuString [v]
    muLit (HsStringPrim  v) = MuString v
    muLit (HsIntPrim     v) = MuInteger v
    muLit (HsFloatPrim   v) = MuFloat . fromRational $ v
    muLit (HsDoublePrim  v) = MuFloat . fromRational $ v

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
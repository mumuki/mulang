{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.C (c, parseC) where

import Language.Mulang.Ast
import Language.Mulang.Parsers
import Language.Mulang.Builder (compactMap, normalize)

import qualified Language.C.Parser as C
import Language.C.Syntax
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.InputStream

import Control.Fallible

import Data.List(intercalate)
import Data.Maybe(catMaybes, maybe, fromJust)

c :: Parser
c = orFail . parseC'

parseC :: EitherParser
parseC = parseC'

parseC' :: EitherParser
parseC' = fmap (normalize . muTranslationUnit) . toEitherParser . flip (C.parseC) nopos . inputStreamFromString

toEitherParser (Left (C.ParseError (msgs, _))) = Left $ concat msgs
toEitherParser (Right right) = Right right

muTranslationUnit (CTranslUnit externalDeclatarions _) = compactMap muExternalDeclaration $ externalDeclatarions

muExternalDeclaration (CDeclExt declaration) = muDeclaration declaration
muExternalDeclaration (CFDefExt function)    = muFunction function
muExternalDeclaration e = debug e

muDeclaration (CDecl declarationSpecifiers declarations _) = Sequence $ concatMap (muDeclarationSpecifier declarationSpecifiers) declarations

muDeclarationSpecifier :: [CDeclSpec] -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> [Expression]
muDeclarationSpecifier declarationSpecifiers (maybeDeclarator, maybeInitializer, _) = [
      TypeSignature (muMaybeTypeDeclarator maybeDeclarator) (SimpleType (intercalateMaybes " " muDecl declarationSpecifiers) []),
      Variable (muMaybeDeclaratorId maybeDeclarator) (fmapOrNone muInitializer maybeInitializer)]

muDecl :: CDeclSpec -> Maybe String
muDecl (CTypeSpec decl) = muTypeSpecifier decl
muDecl _ = Nothing

muTypeSpecifier :: CTypeSpec -> Maybe String
muTypeSpecifier (CVoidType   _) = Just "void"
muTypeSpecifier (CCharType   _) = Just "char"
muTypeSpecifier (CShortType  _) = Just "short"
muTypeSpecifier (CIntType    _) = Just "int"
muTypeSpecifier (CLongType   _) = Just "long"
muTypeSpecifier (CFloatType  _) = Just "float"
muTypeSpecifier (CDoubleType _) = Just "double"
muTypeSpecifier (CSignedType _) = Just "signed"
muTypeSpecifier (CUnsigType  _) = Just "unsigned"
muTypeSpecifier (CBoolType   _) = Just "bool"
muTypeSpecifier (CEnumType _ _) = Just "enum"
muTypeSpecifier (CTypeDef id _) = Just $ muIdent id
muTypeSpecifier (CSUType sou _) = Just $ muStructureUnionTag sou
muTypeSpecifier _ = Nothing

-- muStorageSpecifier (CAuto     _) = Just "auto"
-- muStorageSpecifier (CRegister _) = Just "register"
-- muStorageSpecifier (CStatic   _) = Just "static"
-- muStorageSpecifier (CExtern   _) = Just "extern"
-- muStorageSpecifier (CTypedef  _) = Just "typedef"
-- muStorageSpecifier _ = Nothing

-- muTypeQualifier (CConstQual _) = Just "const"
-- muTypeQualifier (CVolatQual _) = Just "volatile"
-- muTypeQualifier (CRestrQual _) = Just "restrict"
-- muTypeQualifier _ = Nothing



muStructureUnionTag (CStruct structTag _ _ _ _) = muStructTag structTag

muStructTag CStructTag = "struct"
muStructTag CUnionTag  = "union"

muInitializer (CInitExpr initExpr _) = muExpression initExpr
muInitializer (CInitList initList _) = muInitList initList

muExpression :: CExpr -> Expression
muExpression (CConst constant) = muConst constant
muExpression (CVar i _) = Reference $ muIdent i
--muExpression (CComma [CExpression _] _   ) = undefined
--muExpression (CAssign CAssignOp (CExpression _) (CExpression _) _  ) = undefined
--muExpression (CCond (CExpression _) (Maybe (CExpression _)) (CExpression _) _  ) = undefined
--muExpression (CBinary CBinaryOp (CExpression _) (CExpression _) _  ) = undefined
--muExpression (CCast (CDeclaration _) (CExpression _) _   ) = undefined
--muExpression (CUnary CUnaryOp (CExpression _) _  ) = undefined
--muExpression (CSizeofExpr (CExpression _) _  ) = undefined
--muExpression (CSizeofType (CDeclaration _) _   ) = undefined
--muExpression (CAlignofExpr (CExpression _) _   ) = undefined
--muExpression (CAlignofType (CDeclaration _) _  ) = undefined
--muExpression (CComplexReal (CExpression _) _   ) = undefined
--muExpression (CComplexImag (CExpression _) _   ) = undefined
--muExpression (CIndex (CExpression _) (CExpression _) _   ) = undefined
--muExpression (CCall (CExpression _) [CExpression _] _  ) = undefined
--muExpression (CMember (CExpression _) Ident Bool _   ) = undefined
--muExpression (CCompoundLit (CDeclaration _) (CInitializerList _) _  ) = undefined
--muExpression (CGenericSelection (CExpression _) [(Maybe (CDeclaration _), CExpression _)] _ ) = undefined
--muExpression (CStatExpr (CStatement _) _  ) = undefined
--muExpression (CLabAddrExpr Ident _  ) = undefined
--muExpression (CBuiltinExpr (CBuiltinThing _)) = undefined

muFunction :: CFunDef -> Expression
muFunction (CFunDef declarationSpecifiers declarator params body _) = Sequence [
  SubroutineSignature (muTypeDeclarator declarator) (muParamTypes params) (intercalateMaybes " " muDecl declarationSpecifiers) [],
  SimpleFunction (muDeclaratorId declarator) (muParams params) (muStatement body)]

muParams :: [CDecl] -> [Pattern]
muParams = map muParam

muParamTypes :: [CDecl] -> [String]
muParamTypes = map muParamType

muParamType :: CDecl -> String
muParamType (CDecl declarationSpecifiers _ _) = intercalateMaybes " " muDecl declarationSpecifiers

muParam :: CDecl -> Pattern
muParam declaration = VariablePattern (muCDecl declaration)

muCDecl :: CDecl -> String
muCDecl (CDecl _ [(maybeDeclarator, _, _)] _) = muMaybeTypeDeclarator maybeDeclarator

muStatement :: CStat -> Expression
muStatement (CCompound _ sentences _) = Sequence $ map muCompoundBlockItem sentences
muStatement (CExpr maybeExpression _) = fmapOrNone muExpression maybeExpression
muStatement a = debug a
--muStatement CLabel Ident (CStatement a) [CAttribute a] a
--muStatement CCase (CExpression a) (CStatement a) a
--muStatement CCases (CExpression a) (CExpression a) (CStatement a) a
--muStatement CDefault (CStatement a) a
--muStatement CIf (CExpression a) (CStatement a) (Maybe (CStatement a)) a
--muStatement CSwitch (CExpression a) (CStatement a) a
--muStatement CWhile (CExpression a) (CStatement a) Bool a
--muStatement CFor (Either (Maybe (CExpression a)) (CDeclaration a)) (Maybe (CExpression a)) (Maybe (CExpression a)) (CStatement a) a
--muStatement CGoto Ident a
--muStatement CGotoPtr (CExpression a) a
--muStatement CCont a
--muStatement CBreak a
--muStatement CReturn (Maybe (CExpression a)) a
--muStatement CAsm (CAssemblyStatement a) a

muCompoundBlockItem :: CBlockItem -> Expression
muCompoundBlockItem (CBlockStmt statement) = muStatement statement
muCompoundBlockItem (CBlockDecl declaration) = muDeclaration declaration
muCompoundBlockItem (CNestedFunDef function) = muFunction function

muInitList initList = MuList $ map (\(_, initializer) -> muInitializer initializer) initList

muConst (CIntConst   cInt   _)            = MuNumber $ fromIntegral $ getCInteger cInt
muConst (CStrConst   cStr   _)            = MuString $ getCString cStr
muConst (CFloatConst (CFloat strF) _)     = MuNumber $ (read strF :: Double)
muConst (CCharConst  (CChar  char _) _)   = MuChar char
muConst (CCharConst  (CChars string _) _) = MuString string

-- -- Combinators

fmapOrNone = maybe None

mapMaybes f = catMaybes . map f
intercalateMaybes sep f = intercalate sep . mapMaybes f

-- -- Helpers


muMaybeDeclaratorId = fromJust . fmap muDeclaratorId

muDeclaratorId :: CDeclr -> String
muDeclaratorId (CDeclr ident _ _ _ _) = muMaybeIdent ident

muMaybeIdent (Just ident) = muIdent ident
muIdent (Ident name _ _) = name

muMaybeTypeDeclarator :: Maybe CDeclr -> String
muMaybeTypeDeclarator = fromJust . fmap muTypeDeclarator

muTypeDeclarator :: CDeclr -> String
muTypeDeclarator (CDeclr ident ds _ _ _) = ptrDerivedDeclarators ds ++ muMaybeIdent ident ++ arrDerivedDeclarators ds

ptrDerivedDeclarators ds = intercalateMaybes "" ptrs ds
arrDerivedDeclarators ds = intercalateMaybes "" arrs ds

ptrs (CPtrDeclr _ _) = Just "*"
ptrs _ = Nothing

arrs (CArrDeclr [] (CNoArrSize False) _) = Just "[]"
arrs (CArrDeclr [] (CArrSize False (CConst (CIntConst n _))) _) = Just $ '[': show n ++ "]"
arrs _ = Nothing

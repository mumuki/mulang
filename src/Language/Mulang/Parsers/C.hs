{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.C (c, parseC) where

import Language.Mulang.Ast hiding (Primitive, While, Return, Equal, Lambda, Try, Switch, Assert)
import Language.Mulang.Parsers
import Language.Mulang.Builder (compactMap, normalize)

import qualified Language.C.Parser as C
import Language.C.Syntax
--import Language.C.Pretty (prettyPrint)
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.InputStream


import Control.Fallible

import Data.List(intercalate)
import Data.Maybe(catMaybes, fromMaybe)

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
muExternalDeclaration e = debug e

muDeclaration (CDecl declarationSpecifiers declarations _) = Sequence $ concatMap (muDeclarationSpecifier declarationSpecifiers) declarations

muDeclarationSpecifier declarationSpecifiers (declarator, initializer, expression) = [
      TypeSignature (typeDeclrId declarator) (SimpleType (intercalateMaybes " " muDecl declarationSpecifiers) []),
      Variable (declrId declarator) (fmapOrNone muVarInit initializer)]

muDecl (CTypeSpec decl) = muTypeSpecifier decl
muDecl _ = Nothing

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
muTypeSpecifier (CTypeDef id _) = Just $ i id
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

muVarInit e = debug e

-- -- Combinators

fmapOrNone f = fromMaybe None . fmap f

mapMaybes f = catMaybes . map f
intercalateMaybes sep f = intercalate sep . mapMaybes f

-- -- Helpers

declrId (Just (CDeclr ident _ _ _ _)) = mi ident

mi (Just ident) = i ident
i (Ident name _ _) = name

typeDeclrId (Just (CDeclr ident ds _ _ _)) = ptrDerivedDeclarators ds ++ mi ident ++ arrDerivedDeclarators ds


ptrDerivedDeclarators ds = intercalateMaybes "" ptrs ds
arrDerivedDeclarators ds = intercalateMaybes "" arrs ds

ptrs (CPtrDeclr _ _) = Just "*"
ptrs _ = Nothing

arrs (CArrDeclr [] (CNoArrSize False) _) = Just "[]"
arrs (CArrDeclr [] (CArrSize False (CConst (CIntConst n _))) _) = Just $ '[': show n ++ "]"
arrs _ = Nothing

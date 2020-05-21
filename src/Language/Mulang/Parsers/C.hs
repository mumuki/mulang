{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.C (c, parseC) where

import Language.Mulang.Ast
import qualified Language.Mulang.Ast.Operator as O
import Language.Mulang.Parsers
import Language.Mulang.Builder (compactMap, compactConcatMap, normalize)

import qualified Language.C.Parser as C
import Language.C.Syntax
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.InputStream

import Control.Fallible

import Data.List(intercalate, partition)
import Data.Maybe(catMaybes, maybe, fromJust)

c :: Parser
c = orFail . parseC'

parseC :: EitherParser
parseC = parseC'

parseC' :: EitherParser
parseC' = fmap (normalize . muTranslationUnit) . toEitherParser . flip (C.parseC) nopos . inputStreamFromString

toEitherParser :: Either C.ParseError CTranslUnit -> Either String CTranslUnit
toEitherParser (Left (C.ParseError (msgs, _))) = Left $ concat msgs
toEitherParser (Right right)                   = Right right

muTranslationUnit :: CTranslUnit -> Expression
muTranslationUnit (CTranslUnit externalDeclatarions _) = compactMap muExternalDeclaration $ externalDeclatarions

muExternalDeclaration :: CExtDecl -> Expression
muExternalDeclaration (CDeclExt declaration) = muDeclaration declaration
muExternalDeclaration (CFDefExt function)    = muFunction function
muExternalDeclaration e                      = debug e

muDeclaration :: CDecl -> Expression
muDeclaration (CDecl declarationSpecifiers declarations _) = compactConcatMap (muDeclarationSpecifier declarationSpecifiers) declarations
muDeclaration e                                            = debug e

muDeclarationSpecifier :: [CDeclSpec] -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> [Expression]
muDeclarationSpecifier declarationSpecifiers (maybeDeclarator, maybeInitializer, _) = [
      TypeSignature (muMaybeTypeDeclarator maybeDeclarator) (SimpleType (intercalateMaybes " " muDecl declarationSpecifiers) []),
      Variable (muMaybeDeclaratorId maybeDeclarator) (fmapOrNone muInitializer maybeInitializer)]

muDecl :: CDeclSpec -> Maybe String
muDecl (CTypeSpec decl) = muTypeSpecifier decl
muDecl _                = Nothing

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
muTypeSpecifier _               = Nothing

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

muStructureUnionTag :: CStructUnion -> String
muStructureUnionTag (CStruct structTag _ _ _ _) = muStructTag structTag

muStructTag :: CStructTag -> String
muStructTag CStructTag = "struct"
muStructTag CUnionTag  = "union"

muInitializer :: CInit -> Expression
muInitializer (CInitExpr initExpr _) = muExpression initExpr
muInitializer (CInitList initList _) = muInitList initList

muExpression :: CExpr -> Expression
muExpression (CConst constant) = muConst constant
muExpression (CVar i _)                                       = Reference $ muIdent i
muExpression (CBinary operator leftArgument rightArgument _ ) = Application (muBinaryOp operator) [muExpression leftArgument, muExpression rightArgument]
muExpression (CUnary operator argument _ )                    = muUnaryOp operator (muExpression argument)
muExpression (CAssign operator leftArgument rightArgument _ ) = muAssignmentExpression operator leftArgument rightArgument
muExpression (CCall callee arguments _  )                     = Application (muExpression callee) (map muExpression arguments)
muExpression (CIndex callee argument _   )                    = Application (Reference "[]") [muExpression callee, muExpression argument]
muExpression (CMember expr ident False _)                     = FieldReference (muExpression expr) (muIdent ident)
muExpression a                                                = debug a
--muExpression (CComma [CExpression _] _   ) = undefined
--muExpression (CCond (CExpression _) (Maybe (CExpression _)) (CExpression _) _  ) = undefined
--muExpression (CCast (CDeclaration _) (CExpression _) _   ) = undefined
--muExpression (CSizeofExpr (CExpression _) _  ) = undefined
--muExpression (CSizeofType (CDeclaration _) _   ) = undefined
--muExpression (CAlignofExpr (CExpression _) _   ) = undefined
--muExpression (CAlignofType (CDeclaration _) _  ) = undefined
--muExpression (CComplexReal (CExpression _) _   ) = undefined
--muExpression (CComplexImag (CExpression _) _   ) = undefined
--muExpression (CCompoundLit (CDeclaration _) (CInitializerList _) _  ) = undefined
--muExpression (CGenericSelection (CExpression _) [(Maybe (CDeclaration _), CExpression _)] _ ) = undefined
--muExpression (CStatExpr (CStatement _) _  ) = undefined
--muExpression (CLabAddrExpr Ident _  ) = undefined
--muExpression (CBuiltinExpr (CBuiltinThing _)) = undefined

muAssignmentExpression :: CAssignOp -> CExpr -> CExpr -> Expression
muAssignmentExpression CAssignOp (CVar i _)              argument = Assignment (muIdent i) $ muExpression argument
muAssignmentExpression CAssignOp (CMember exp i False _) argument = FieldAssignment (muExpression exp) (muIdent i) $ muExpression argument
muAssignmentExpression operator  l@(CVar i _)            argument = Assignment (muIdent i) $ Application (muAssignOp operator) [muExpression l, muExpression argument]
muAssignmentExpression operator  (CMember exp i False _) argument = FieldAssignment (muExpression exp) (muIdent i) $ Application (muAssignOp operator) [Reference (muIdent i), muExpression argument]
muAssignmentExpression _         e                   _            = debug e

muBinaryOp :: CBinaryOp -> Expression
muBinaryOp CMulOp = Primitive O.Multiply
muBinaryOp CDivOp = Primitive O.Divide
muBinaryOp CAddOp = Primitive O.Plus
muBinaryOp CSubOp = Primitive O.Minus
muBinaryOp CLeOp  = Primitive O.LessThan
muBinaryOp CGrOp  = Primitive O.GreatherThan
muBinaryOp CLeqOp = Primitive O.LessOrEqualThan
muBinaryOp CGeqOp = Primitive O.GreatherOrEqualThan
muBinaryOp CEqOp  = Primitive O.Equal
muBinaryOp CNeqOp = Primitive O.NotEqual
muBinaryOp CLndOp = Primitive O.And
muBinaryOp CLorOp = Primitive O.Or
muBinaryOp CAndOp = Primitive O.BitwiseAnd
muBinaryOp COrOp  = Primitive O.BitwiseOr
muBinaryOp CRmdOp = Primitive O.Modulo
muBinaryOp CShlOp = Primitive O.BitwiseLeftShift
muBinaryOp CShrOp = Primitive O.BitwiseRightShift
muBinaryOp CXorOp = Primitive O.BitwiseXor

muUnaryOp :: CUnaryOp -> Expression -> Expression
muUnaryOp CPreIncOp  argument@(Reference arg) = Assignment arg $ Application (Primitive O.Plus)     [argument, MuNumber 1]
muUnaryOp CPreDecOp  argument@(Reference arg) = Assignment arg $ Application (Primitive O.Minus)    [argument, MuNumber 1]
muUnaryOp CPostIncOp argument@(Reference arg) = Assignment arg $ Application (Primitive O.Plus)     [argument, MuNumber 1]
muUnaryOp CPostDecOp argument@(Reference arg) = Assignment arg $ Application (Primitive O.Minus)    [argument, MuNumber 1]
muUnaryOp CNegOp     argument                 = Application (Primitive O.Negation) [argument]
muUnaryOp CPlusOp    argument                 = Application (Primitive O.Plus)     [argument]
muUnaryOp CMinOp     argument                 = Application (Primitive O.Minus)    [argument]
muUnaryOp CAdrOp     argument                 = Application (Reference "&")        [argument]
muUnaryOp CIndOp     argument                 = Application (Reference "*")        [argument]
muUnaryOp CCompOp    argument                 = Application (Reference "~")        [argument]
muUnaryOp a b                                 = Other (Just $ show a) $ Just b

muAssignOp :: CAssignOp -> Expression
muAssignOp CMulAssOp = Primitive O.Multiply
muAssignOp CDivAssOp = Primitive O.Divide
muAssignOp CAddAssOp = Primitive O.Plus
muAssignOp CSubAssOp = Primitive O.Minus
muAssignOp CAndAssOp = Primitive O.And
muAssignOp COrAssOp  = Primitive O.Or
muAssignOp CRmdAssOp = Primitive O.Modulo
muAssignOp CShlAssOp = Primitive O.BitwiseLeftShift
muAssignOp CShrAssOp = Primitive O.BitwiseRightShift
muAssignOp CXorAssOp = Primitive O.BitwiseXor

muFunction :: CFunDef -> Expression
muFunction (CFunDef declarationSpecifiers declarator _ body _) = Sequence [
  SubroutineSignature (muTypeDeclarator declarator) (muParamTypes declarator) (intercalateMaybes " " muDecl declarationSpecifiers) [],
  SimpleFunction (muDeclaratorId declarator) (muParams declarator) (muStatement body)]

muParams :: CDeclr -> [Pattern]
muParams = mapMaybes muParam . muFunctionParams

muParamTypes :: CDeclr -> [String]
muParamTypes = mapMaybes muParamType . muFunctionParams

muParamType :: CDecl -> Maybe String
muParamType (CDecl declarationSpecifiers _ _) = Just $ intercalateMaybes " " muDecl declarationSpecifiers
muParamType _                                 = Nothing

muFunctionParams :: CDeclr -> [CDecl]
muFunctionParams (CDeclr _ [CFunDeclr (Right (params, False)) [] _] _ _ _) = params
muFunctionParams _                                                         = []

muParam :: CDecl -> Maybe Pattern
muParam declaration = fmap VariablePattern (muCDecl declaration)

muCDecl :: CDecl -> Maybe String
muCDecl (CDecl _ [(maybeDeclarator, _, _)] _) = Just $ muMaybeTypeDeclarator maybeDeclarator
muCDecl _                                     = Nothing

muStatement :: CStat -> Expression
muStatement (CCompound _ sentences _)                           = compactMap muCompoundBlockItem sentences
muStatement (CExpr maybeExpression _)                           = fmapOrNone muExpression maybeExpression
muStatement (CIf condition trueBranch maybeFalseBranch _)       = If (muExpression condition) (muStatement trueBranch) (fmapOrNone muStatement maybeFalseBranch)
muStatement (CFor forInitValue maybeCondition maybeAcum body _) = ForLoop (muForInitValue forInitValue) (fmapOrNone muExpression maybeCondition) (fmapOrNone muExpression maybeAcum) (muStatement body)
muStatement (CReturn maybeExpression _)                         = Return $ fmapOrNone muExpression maybeExpression
muStatement (CWhile condition body _ _)                         = While (muExpression condition) (muStatement body)
muStatement (CSwitch value cases _)                             = muSwitch (muExpression value) $ muCases cases
muStatement (CDefault statement _)                              = muStatement statement
muStatement (CBreak _)                                          = Break None
muStatement (CCont _)                                           = Continue None
muStatement a                                                   = debug a
--muStatement CCases (CExpression a) (CExpression a) (CStatement a) a
--muStatement CGoto Ident a
--muStatement CGotoPtr (CExpression a) a
--muStatement CLabel Ident (CStatement a) [CAttribute a] a
--muStatement CAsm (CAssemblyStatement a) a

muSwitch :: Expression -> ([(Expression, Expression)], Expression) -> Expression
muSwitch value (cases, def) = Switch value cases def

muCases :: CStat -> ([(Expression, Expression)], Expression)
muCases (CCompound [] statements _) = muMapCases . partition isDefault . mapMaybes muBlockStmt $ statements
muCases a                           = ([], debug a)

muMapCases :: ([CStat], [CStat]) -> ([(Expression, Expression)], Expression)
muMapCases (def, cases) = (map muCase cases, compactMap muStatement def)

muCase :: CStat -> (Expression, Expression)
muCase (CCase pattern statement _) = (muExpression pattern, muStatement statement)
muCase e                           = (debug e, None)

isDefault :: CStat -> Bool
isDefault (CDefault _ _) = True
isDefault _              = False

muBlockStmt :: CBlockItem -> Maybe CStat
muBlockStmt (CBlockStmt statement) = Just statement
muBlockStmt _                      = Nothing

muForInitValue :: Either (Maybe CExpr) CDecl -> Expression
muForInitValue (Left maybeExpression) = fmapOrNone muExpression maybeExpression
muForInitValue (Right declaration)    = muDeclaration declaration

muCompoundBlockItem :: CBlockItem -> Expression
muCompoundBlockItem (CBlockStmt statement)   = muStatement statement
muCompoundBlockItem (CBlockDecl declaration) = muDeclaration declaration
muCompoundBlockItem (CNestedFunDef function) = muFunction function

muInitList :: CInitList -> Expression
muInitList initList = MuList $ map (\(_, initializer) -> muInitializer initializer) initList

muConst :: CConst -> Expression
muConst (CIntConst   cInt   _)            = MuNumber . fromIntegral $ getCInteger cInt
muConst (CStrConst   cStr   _)            = MuString $ getCString cStr
muConst (CFloatConst (CFloat strF) _)     = MuNumber $ (read strF :: Double)
muConst (CCharConst  (CChar  char _) _)   = MuChar char
muConst (CCharConst  (CChars string _) _) = MuString string

-- -- Combinators

fmapOrNone :: (a -> Expression) -> Maybe a -> Expression
fmapOrNone = maybe None

mapMaybes :: (a -> Maybe b) -> [a] -> [b]
mapMaybes f = catMaybes . map f

intercalateMaybes :: [a] -> (b -> Maybe [a]) -> [b] -> [a]
intercalateMaybes sep f = intercalate sep . mapMaybes f

-- -- Helpers

muMaybeDeclaratorId :: Maybe CDeclr -> String
muMaybeDeclaratorId = fromJust . fmap muDeclaratorId

muDeclaratorId :: CDeclr -> String
muDeclaratorId (CDeclr ident _ _ _ _) = muMaybeIdent ident

muMaybeIdent :: Maybe Ident -> String
muMaybeIdent (Just ident) = muIdent ident

muIdent :: Ident -> String
muIdent (Ident name _ _) = name

muMaybeTypeDeclarator :: Maybe CDeclr -> String
muMaybeTypeDeclarator = fromJust . fmap muTypeDeclarator

muTypeDeclarator :: CDeclr -> String
muTypeDeclarator (CDeclr ident ds _ _ _) = ptrDerivedDeclarators ds ++ muMaybeIdent ident ++ arrDerivedDeclarators ds

ptrDerivedDeclarators :: [CDerivedDeclr] -> String
ptrDerivedDeclarators ds = intercalateMaybes "" ptrs ds

arrDerivedDeclarators :: [CDerivedDeclr] -> String
arrDerivedDeclarators ds = intercalateMaybes "" arrs ds

ptrs :: CDerivedDeclr -> Maybe String
ptrs (CPtrDeclr _ _) = Just "*"
ptrs _               = Nothing

arrs :: CDerivedDeclr -> Maybe String
arrs (CArrDeclr [] (CNoArrSize False) _)                        = Just "[]"
arrs (CArrDeclr [] (CArrSize False (CConst (CIntConst n _))) _) = Just $ '[': show n ++ "]"
arrs _                                                          = Nothing

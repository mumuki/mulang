module Language.Mulang.Analyzer.SignatureStyleCompiler (
  compileSignatureStyle) where

import Language.Mulang.Signature as Signature (SignatureStyle,
                                               mulangStyle, untypedCStyle,
                                               haskellStyle, prologStyle)
import Language.Mulang.Analyzer.Analysis as Analysis (SignatureStyle(..))

compileSignatureStyle :: Analysis.SignatureStyle -> Signature.SignatureStyle
compileSignatureStyle MulangStyle = mulangStyle
compileSignatureStyle UnTypedCStyle = untypedCStyle
compileSignatureStyle HaskellStyle = haskellStyle
compileSignatureStyle PrologStyle = prologStyle

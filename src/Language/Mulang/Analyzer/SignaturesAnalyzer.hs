module Language.Mulang.Analyzer.SignaturesAnalyzer (
  analyseSignatures) where

import Language.Mulang
import Language.Mulang.Signature
import Language.Mulang.Analyzer.Analysis (SignatureAnalysisType(..))
import Language.Mulang.Analyzer.SignatureStyleCompiler (compileSignatureStyle)
import Data.Maybe (fromMaybe)

analyseSignatures :: Expression -> Maybe SignatureAnalysisType -> [Code]
analyseSignatures e analysis = analyseSignatures' e (fromMaybe NoSignatures analysis)

analyseSignatures' _ NoSignatures             = []
analyseSignatures' e DefaultSignatures        = codeSignaturesOf e
analyseSignatures' e (StyledSignatures style) = styledCodeSignaturesOf (compileSignatureStyle style) e

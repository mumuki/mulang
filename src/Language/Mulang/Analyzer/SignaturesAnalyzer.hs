module Language.Mulang.Analyzer.SignaturesAnalyzer (
  analyseSignatures) where

import Language.Mulang
import Language.Mulang.Signature
import Control.Monad (MonadPlus (..), mzero)

analyseSignatures :: Expression -> Bool -> [Code]
analyseSignatures e flag = onlyIf flag (codeSignaturesOf e)

onlyIf :: MonadPlus m => Bool -> m a -> m a
onlyIf True x = x
onlyIf _    _ = mzero


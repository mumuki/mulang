module Language.Mulang.Inspector.Extras (hasConditional) where

import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner

hasConditional :: Inspection
hasConditional = alternative usesIf usesGuards
module Language.Mulang.Inspector.Extras (
  declaresConditional,
  declaresPredicate) where

import Language.Mulang.Explorer
import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner

declaresConditional :: Inspection
declaresConditional = alternative usesIf usesGuards

declaresPredicate :: BindingPredicate -> Inspection
declaresPredicate pred = alternative (declaresFact pred) (declaresRule pred)
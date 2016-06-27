module Language.Mulang.Inspector.Extras (
  usesConditional,
  declaresPredicate) where

import Language.Mulang.Binding
import Language.Mulang.Explorer
import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner

usesConditional :: Inspection
usesConditional = alternative usesIf usesGuards

declaresPredicate :: BindingPredicate -> Inspection
declaresPredicate pred = alternative (declaresFact pred) (declaresRule pred)
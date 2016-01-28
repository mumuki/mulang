module Language.Mulang.Inspector.Combiner (
  detect,
  negative,
  transitive) where

import Language.Mulang
import Language.Mulang.Inspector
import Language.Mulang.Explorer

detect :: Inspection -> Expression -> [Binding]
detect inspection code = filter (`inspection` code) $ topLevelBindings code

negative :: Inspection -> Inspection
negative f code = not . f code

transitive :: Inspection -> Inspection
transitive inspection binding code = inspection binding code || inUsage
  where inUsage = any (`inspection` code) . transitiveBindingsOf binding $ code

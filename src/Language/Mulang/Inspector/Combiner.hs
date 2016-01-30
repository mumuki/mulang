module Language.Mulang.Inspector.Combiner (
  detect,
  negative,
  alternative,
  transitive) where

import Language.Mulang
import Language.Mulang.Inspector
import Language.Mulang.Explorer


detect :: Inspection -> Expression -> [Binding]
detect inspection code = filter (`inspection` code) $ topLevelBindings code

alternative :: Inspection -> Inspection -> Inspection
alternative i1 i2 b code = i1 b code || i2 b code

negative :: Inspection -> Inspection
negative f b = not . f b

transitive :: Inspection -> Inspection
transitive inspection binding code = inspection binding code || inUsage
  where inUsage = any (`inspection` code) . transitiveBindingsOf binding $ code

module Language.Mulang.Inspector (
  Inspection,
  IdentifierInspection,
  module Language.Mulang.Inspector.Generic,
  module Language.Mulang.Inspector.Combiner,
  module Language.Mulang.Inspector.Generalized,
  module Language.Mulang.Inspector.Query,
  module Language.Mulang.Inspector.Typed,
  module Language.Mulang.Inspector.ObjectOriented,
  module Language.Mulang.Inspector.ObjectOriented.Polymorphism,
  module Language.Mulang.Inspector.Functional,
  module Language.Mulang.Inspector.Logic,
  module Language.Mulang.Inspector.Procedural) where

import Language.Mulang.Inspector.Combiner
import Language.Mulang.Inspector.Functional
import Language.Mulang.Inspector.Generalized
import Language.Mulang.Inspector.Generic
import Language.Mulang.Inspector.Logic
import Language.Mulang.Inspector.ObjectOriented
import Language.Mulang.Inspector.ObjectOriented.Polymorphism
import Language.Mulang.Inspector.Primitive (Inspection, IdentifierInspection)
import Language.Mulang.Inspector.Procedural
import Language.Mulang.Inspector.Query
import Language.Mulang.Inspector.Typed

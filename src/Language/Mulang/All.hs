module Language.Mulang.All(
  js,
  hs,
  module Language.Mulang.Inspector,
  module Language.Mulang.Inspector.Combiner,
  module Language.Mulang.Inspector.Smell) where

import Language.Mulang.Parsers.JavaScript (js)
import Language.Mulang.Parsers.Haskell (hs)
import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner
import Language.Mulang.Inspector.Smell

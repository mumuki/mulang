module Language.Mulang.Inspector.Generalized (
  specify,
  generalize,
  generalized,
  generalizedScoped,
  generalizedScopedList,
  generalizedTransitive,
  generalizedTransitiveList,
  delegates,
  GeneralizedInspection,
  GeneralizedScope,
  GeneralizedIdentifierInspection) where

import Language.Mulang.Ast
import Language.Mulang.Generator (expressions, declarations)
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Combiner (scoped, scopedList, transitive, transitiveList, Scope)
import Language.Mulang.Inspector.Primitive (Inspection, IdentifierInspection)
import Language.Mulang.Inspector.Query (inspect, select)

type GeneralizedInspection = Expression -> Inspection
type GeneralizedScope = GeneralizedInspection -> GeneralizedInspection
type GeneralizedIdentifierInspection = IdentifierPredicate -> GeneralizedInspection

specify :: GeneralizedInspection -> Inspection
specify inspection = \expression -> inspection expression expression

generalize :: Inspection -> GeneralizedInspection
generalize inspection = \_ expression -> inspection expression

generalized :: Scope -> GeneralizedScope
generalized f inspection = \root expression -> (f (inspection root)) expression

generalizedScoped :: Identifier -> GeneralizedScope
generalizedScoped scope = generalized (scoped scope)

generalizedScopedList :: [Identifier] -> GeneralizedScope
generalizedScopedList scope = generalized (scopedList scope)

generalizedTransitive :: Identifier -> GeneralizedScope
generalizedTransitive scope = generalized (transitive scope)

generalizedTransitiveList :: [Identifier] -> GeneralizedScope
generalizedTransitiveList scope = generalized (transitiveList scope)

delegates :: IdentifierInspection
delegates p = specify (delegates' p)

delegates' :: GeneralizedIdentifierInspection
delegates' p root expression = inspect $ do
  (Subroutine name1 _)       <- declarations root
  (Call (Reference name2) _) <- expressions expression
  select (name1 == name2)
  select (p name1)

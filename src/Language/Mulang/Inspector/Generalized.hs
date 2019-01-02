module Language.Mulang.Inspector.Generalized (
  generalize,
  generalized,
  generalizedScoped,
  generalizedScopedList,
  generalizedTransitive,
  generalizedTransitiveList,
  GeneralizedInspection) where

import Language.Mulang.Ast
import Language.Mulang.Generator (expressions, declarations)
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Combiner (scoped, scopedList, transitive, transitiveList, Scope)
import Language.Mulang.Inspector.Primitive (Inspection)
import Language.Mulang.Inspector.Query (inspect, select)

type GeneralizedInspection = Expression -> Inspection
type GeneralizedIdentifierInspection = IdentifierPredicate -> GeneralizedInspection

generalize :: Inspection -> GeneralizedInspection
generalize inspection = \_ expression -> inspection expression

generalized :: Scope -> GeneralizedInspection -> GeneralizedInspection
generalized f inspection = \root expression -> (f (inspection root)) expression

generalizedScoped :: Identifier -> GeneralizedInspection -> GeneralizedInspection
generalizedScoped scope = generalized (scoped scope)

generalizedScopedList :: [Identifier] -> GeneralizedInspection -> GeneralizedInspection
generalizedScopedList scope = generalized (scopedList scope)

generalizedTransitive :: Identifier -> GeneralizedInspection -> GeneralizedInspection
generalizedTransitive scope = generalized (transitive scope)

generalizedTransitiveList :: [Identifier] -> GeneralizedInspection -> GeneralizedInspection
generalizedTransitiveList scope = generalized (transitiveList scope)

delegates :: GeneralizedIdentifierInspection
delegates p root expression = inspect $ do
  (Subroutine name1 _)       <- declarations root
  (Call (Reference name2) _) <- expressions expression
  select (name1 == name2)
  select (p name1)

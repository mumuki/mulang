module Compiler where (compileExpectation)

import Data.Char (isLower)

compileExpectation (Expectation bindings inspection) = compile binding inspection

  compile :: [String] -> Inspection
  compile scope ("Not":xs)                       = negative $ compile scope xs
  compile scope ("not":xs)                       = negative $ compile scope xs
  compile [binding] ["HasUsage", target]         = transitive (hasUsage target) binding
  compile [binding] ["HasLambda"]                = transitive hasLambda binding
  compile [binding] ["HasGuards"]                = transitive hasGuards binding
  compile [binding] ["HasComposition"]           = transitive hasComposition binding
  compile [binding] ["HasBinding"]               = transitive hasBinding binding
  compile [binding] ["HasDirectRecursion"]       = transitive hasDirectRecursion binding
  compile [binding] ["HasComprehension"]         = transitive hasComprehension binding
  compile [binding] ["HasIf"]                    = transitive hasIf binding
  compile [binding] ["HasConditional"]           = transitive hasConditional binding
  compile [binding] ["HasTypeDeclaration"]       = hasTypeDeclaration
  compile [binding] ["HasTypeSignature"]         = hasTypeSignature
  compile scope ("transitive":inspection)        = (compileTransitiveScope scope) (compileAdvanced is)
  compile scope is  | isLower.head $ is          = (compileScoped scope) (compileAdvanced is)
  compile _ _                                    = \_-> True

  compileScope scope = (`scopedList` scope)
  compileTransitiveScope = (`transitiveList` scope)

  compileAdvanced ["declaresObject", name]      = declaresObject name
  compileAdvanced ["declaresAtribute", name]    = declaresAtribute name
  compileAdvanced ["declaresMethod", name]      = declaresMethod name



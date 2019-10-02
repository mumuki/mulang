module Mulang::Expectation
  SMELLS = %w(DiscardsExceptions DoesConsolePrint DoesNilTest DoesNullTest DoesTypeTest
              HasAssignmentReturn HasCodeDuplication HasEmptyIfBranches HasRedundantRepeat HasLongParameterList
              HasMisspelledBindings HasMisspelledIdentifiers
              HasRedundantBooleanComparison HasRedundantGuards HasRedundantIf
              HasRedundantLambda HasRedundantLocalVariableReturn HasRedundantParameter
              HasRedundantReduction HasTooManyMethods HasTooShortBindings HasTooShortIdentifiers HasUnreachableCode
              HasWrongCaseBinding HasWrongCaseIdentifiers IsLongCode OverridesEqualOrHashButNotBoth
              ReturnsNil ReturnsNull UsesCut UsesFail UsesUnificationOperator)

  def self.guess_type(expectation)
    if expectation[:binding] == '<<custom>>'
      Custom
    elsif expectation[:inspection] =~ /(Not\:)?Has.*/ && !has_smell?(expectation[:inspection])
      V0
    else
      V2
    end
  end

  def self.has_smell?(smell)
    SMELLS.include? smell
  end

  def self.parse(expectation)
    guess_type(expectation).parse(expectation)
  end

  def self.valid?(expectation)
    guess_type(expectation).valid?(expectation)
  end
end

require_relative './expectation/i18n'
require_relative './expectation/custom'
require_relative './expectation/standard'

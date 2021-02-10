module Mulang::Expectation
  LOGIC_SMELLS = %w(
    HasRedundantReduction
    UsesCut
    UsesFail
    UsesUnificationOperator
  )

  FUNCTIONAL_SMELLS = %w(
    HasRedundantGuards
    HasRedundantLambda
    HasRedundantParameter
    ShouldUseOtherwise
  )

  OBJECT_ORIENTED_SMELLS = %w(
    DoesNilTest
    DoesNullTest
    DoesTypeTest
    HasTooManyMethods
    OverridesEqualOrHashButNotBoth
    ReturnsNil
    ReturnsNull
    UsesNamedSelfReference
  )

  IMPERATIVE_SMELLS = %w(
    HasAssignmentCondition
    HasAssignmentReturn
    HasEmptyRepeat
    HasRedundantRepeat
  )

  EXPRESSIVENESS_SMELLS = %w(
    HasMisspelledBindings
    HasMisspelledIdentifiers
    HasTooShortBindings
    HasTooShortIdentifiers
    HasWrongCaseBinding
    HasWrongCaseIdentifiers
  )

  GENERIC_SMELLS = %w(
    DiscardsExceptions
    DoesConsolePrint
    HasCodeDuplication
    HasDeclarationTypos
    HasEmptyIfBranches
    HasEqualIfBranches
    HasLongParameterList
    HasRedundantBooleanComparison
    HasRedundantIf
    HasRedundantLocalVariableReturn
    HasUnreachableCode
    HasUsageTypos
    IsLongCode
    ShouldInvertIfCondition
    ShouldUseStrictComparators
  )

  JAVA_SCRIPT_SMELLS = %w(
    JavaScript#UsesVarInsteadOfLet
  )

  SMELLS = GENERIC_SMELLS +
           EXPRESSIVENESS_SMELLS +
           IMPERATIVE_SMELLS +
           OBJECT_ORIENTED_SMELLS +
           FUNCTIONAL_SMELLS +
           LOGIC_SMELLS +
           JAVA_SCRIPT_SMELLS

  def self.guess_type(expectation)
    if expectation[:binding] == '<<custom>>'
      Custom
    elsif expectation[:inspection] =~ /^(Not\:)?Has.*/ && !has_smell?(expectation[:inspection])
      V0
    else
      V2
    end
  end

  def self.has_smell?(smell)
    SMELLS.include? smell.split(':').first
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

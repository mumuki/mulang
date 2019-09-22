class Mulang::Expectation

  SMELLS = %w(DiscardsExceptions DoesConsolePrint DoesNilTest DoesNullTest DoesTypeTest
              HasAssignmentReturn HasCodeDuplication HasEmptyIfBranches HasLongParameterList
              HasMisspelledBindings HasMisspelledIdentifiers
              HasRedundantBooleanComparison HasRedundantGuards HasRedundantIf
              HasRedundantLambda HasRedundantLocalVariableReturn HasRedundantParameter
              HasRedundantReduction HasTooManyMethods HasTooShortBindings HasTooShortIdentifiers HasUnreachableCode
              HasWrongCaseBinding HasWrongCaseIdentifiers IsLongCode OverridesEqualOrHashButNotBoth
              ReturnsNil ReturnsNull UsesCut UsesFail UsesUnificationOperator)

  attr_accessor :binding, :inspection

  def initialize(binding, inspection)
    @binding = binding
    @inspection = inspection
  end

  def check!
    raise "Wrong binding in #{to_h}" unless binding?
    raise "Wrong inspection #{to_h}" unless inspection?
  end

  def translate(keywords = nil)
    Mulang::Expectation::I18n.translate self, keywords
  end

  def to_h
    {binding: binding, inspection: inspection.to_s}
  end

  def self.guess_type(expectation)
    if expectation[:inspection] =~ /(Not\:)?Has.*/ && !has_smell?(expectation[:inspection])
      V0
    else
      V2
    end
  end

  def self.has_smell?(smell)
    SMELLS.include? smell
  end

  def self.parse(expectation)
    guess_type(expectation).new(
      expectation[:binding],
      Mulang::Inspection.parse(expectation[:inspection]))
  end

  def self.valid?(expectation)
    !!Mulang::Inspection.parse(expectation['inspection']) rescue false
  end

  class V0 < Mulang::Expectation
    INSPECTIONS = %w(HasBinding HasTypeDeclaration HasTypeSignature HasVariable HasArity HasDirectRecursion
                     HasComposition HasComprehension HasForeach HasIf HasGuards HasConditional HasLambda HasRepeat HasWhile
                     HasUsage HasAnonymousVariable HasNot HasForall HasFindall)


    def binding?
      binding.present?
    end

    def inspection?
      inspection.present? && INSPECTIONS.include?(inspection.type)
    end

    def as_v2
      if has? 'Binding' then as_v2_declare ''
      elsif has? 'TypeDeclaration' then as_v2_declare 'TypeAlias'
      elsif has? 'TypeSignature' then as_v2_declare 'TypeSignature'
      elsif has? 'Variable' then as_v2_declare 'Variable'
      elsif has? 'Arity' then as_v2_declare "ComputationWithArity#{inspection.target.value}"
      elsif has? 'DirectRecursion' then as_v2_declare "Recursively"
      elsif has? 'Usage'
        V2.new binding, new_inspection('Uses', Mulang::Inspection::Target.named(inspection.target.value))
      else as_v2_use
      end
    end

    def has?(simple_type)
      inspection.type == "Has#{simple_type}"
    end

    def as_v2_use
      V2.new binding, new_inspection(inspection.type.gsub('Has', 'Uses'), nil)
    end

    def as_v2_declare(simple_type)
      V2.new '*', new_inspection("Declares#{simple_type}", Mulang::Inspection::Target.named(binding))
    end

    def new_inspection(type, target)
      Mulang::Inspection.new(type, target, negated: inspection.negated?)
    end
  end

  class V2 < Mulang::Expectation
    def binding?
      true
    end

    def inspection?
      true
    end

    def as_v2
      self
    end
  end
end

require_relative './expectation/i18n'

class Mulang::Expectation::Standard
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

  def translate!(keywords = nil)
    Mulang::Expectation::I18n.translate! self, keywords
  end

  def to_h
    {binding: binding, inspection: inspection.to_s}
  end

  def custom?
    false
  end

  def standard?
    true
  end

  def self.parse(expectation)
    new expectation[:binding], Mulang::Inspection.parse(expectation[:inspection])
  end

  def self.valid?(expectation)
    !!Mulang::Inspection.parse(expectation['inspection']) rescue false
  end
end

class Mulang::Expectation::V0 < Mulang::Expectation::Standard
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
      Mulang::Expectation::V2.new binding, new_inspection('Uses', Mulang::Inspection::Target.named(inspection.target.value))
    else as_v2_use
    end
  end

  def has?(simple_type)
    inspection.type == "Has#{simple_type}"
  end

  def as_v2_use
    Mulang::Expectation::V2.new binding, new_inspection(inspection.type.gsub(/^Has/, 'Uses'), nil)
  end

  def as_v2_declare(simple_type)
    Mulang::Expectation::V2.new '*', new_inspection("Declares#{simple_type}", Mulang::Inspection::Target.named(binding))
  end

  def new_inspection(type, target)
    Mulang::Inspection.new(type, target, negated: inspection.negated?)
  end
end

class Mulang::Expectation::V2 < Mulang::Expectation::Standard
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

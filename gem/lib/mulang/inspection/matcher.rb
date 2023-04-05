class Mulang::Inspection::Matcher
  include Mulang::Inspection::Compacted

  TYPES = %w(
    WithAnything WithChar WithFalse WithLiteral
    WithLogic WithMath WithNil WithNonliteral
    WithNumber WithReference WithString
    WithSymbol WithTrue
    WithAnyString WithAnyNumber)

  attr_accessor :type, :value

  def initialize(type, value=nil)
    @type = type
    @value = value
  end

  def to_s
    "#{type.to_s.camelcase}#{value ? ":#{value}" : nil}"
  end

  def self.parse(type, value)
    return nil unless type
    raise "Invalid matcher #{type}" unless type.in? TYPES
    new type.underscore.to_sym, value
  end
end

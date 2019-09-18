class Mulang::Inspection::Matcher
  include Mulang::Inspection::Compacted

  TYPES = %w(WithLiteral WithNonliteral WithLogic WithMath WithFalse WithNil WithTrue WithChar WithNumber WithString WithSymbol)

  attr_accessor :type, :value

  def initialize(type, value=nil)
    @type = type
    @value = value
  end

  def self.parse(type, value)
    return nil unless type
    raise "Invalid matcher #{type}" unless type.in? TYPES
    new type.underscore.to_sym, value
  end
end

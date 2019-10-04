module Mulang
  class Inspection
    module Compacted
      def as_json(*args)
        super(*args).compact
      end
    end

    include Mulang::Inspection::Compacted

    REGEXP = Regexp.new %q{
      ^(?<negation>Not:)?
      (?<type>[^:]+)
      (
        :(?<matcher>WithLiteral|WithNonliteral|WithLogic|WithMath|WithFalse|WithNil|WithTrue) |
        :(?<matcher>WithChar|WithNumber|WithString|WithSymbol):(?<value>[^:]+) |
        :(?<target>[^:]+)(:(?<matcher>[^:]+)(:(?<value>[^:]+))?)?
      )?$}.gsub(/\s/, '')

    attr_accessor :type, :target, :matcher, :negated
    alias negated? negated

    def initialize(type, target, negated: false, matcher: nil)
      @type = type
      @target = target
      @negated = negated
      @matcher = matcher
    end

    def to_s
      "#{negated_to_s}#{type}#{target_to_s}#{matcher_to_s}"
    end

    def negated_to_s
      negated ? 'Not:' : nil
    end

    def target_to_s
      target ? ":#{target.to_s}" : nil
    end

    def matcher_to_s
      matcher ? ":#{matcher.to_s}" : nil
    end

    def self.parse_binding_name(binding_s)
      if binding_s.start_with? 'Intransitive:'
        binding_s[13..-1]
      else
        binding_s
      end
    end

    def self.parse(inspection_s)
      parse_extension(inspection_s).try { |it| return it }
      match = REGEXP.match inspection_s
      raise "Invalid inspection #{inspection_s}" unless match
      Mulang::Inspection.new(
        match['type'],
        Mulang::Inspection::Target.parse(match['target']),
        matcher: Mulang::Inspection::Matcher.parse(match['matcher'], match['value']),
        negated: match['negation'].present?)
    end

    def self.parse_extension(inspection_s)
      extensions.each do |extension|
        extension.parse(inspection_s).try { |it| return it }
      end
      nil
    end

    def self.extensions
      @extensions ||= []
    end

    def self.register_extension!(extension)
      extensions << extension
    end

    def self.unregister_extension!(extension)
      extensions.delete extension
    end
  end
end

module Mulang::Inspection::Css
  def self.parse(inspection_s)
    match = /^(?<negation>Not:)?DeclaresStyle:(?<target>.*)$/.match(inspection_s)
    Mulang::Inspection.new 'DeclaresStyle', Mulang::Inspection::Target.new(:unknown, match['target']), negated: match['negation'].present? if match
  end
end

require_relative './inspection/target'
require_relative './inspection/matcher'

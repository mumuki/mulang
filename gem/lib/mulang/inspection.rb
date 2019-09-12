module Mulang
  class Inspection
    attr_accessor :type, :target, :negated
    alias negated? negated

    def initialize(type, target, negated=false)
      @type = type
      @target = target
      @negated = negated
    end

    def to_s
      "#{negated_to_s}#{type}#{target_to_s}"
    end

    def negated_to_s
      negated ? 'Not:' : nil
    end

    def target_to_s
      target ? ":#{target.to_s}" : nil
    end

    def self.parse_binding_name(binding_s)
      if binding_s.start_with? 'Intransitive:'
        binding_s[13..-1]
      else
        binding_s
      end
    end

    def self.parse(insepection_s)
      raise "Invalid inspection #{insepection_s}" unless insepection_s =~ /^(Not\:)?([^\:]+)\:?(.+)?$/
      Inspection.new($2, Mulang::Inspection::Target.parse($3), $1.present?)
    end
  end
end

require_relative './inspection/target'

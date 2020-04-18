module Mulang::Expectation::I18n
  class << self
    DEFAULT_TOKENS = {
      keyword_EntryPoint: 'program',
      keyword_Fail: 'fail',
      keyword_False: 'false',
      keyword_findall: 'findall',
      keyword_For: 'for',
      keyword_Forall: 'forall',
      keyword_Foreach: 'foreach',
      keyword_If: 'if',
      keyword_Is: 'is',
      keyword_Not: 'not',
      keyword_Null: 'null',
      keyword_Repeat: 'repeat',
      keyword_Switch: 'switch',
      keyword_True: 'true',
      keyword_While: 'while',
      keyword_Yield: 'yield',
      operator_And: '&&',
      operator_BackwardComposition: '.',
      operator_Divide: '/',
      operator_Equal: '==',
      operator_ForwardComposition: '>>',
      operator_GreatherOrEqualThan: '>=',
      operator_GreatherThan: '>',
      operator_Hash: 'hash',
      operator_LessOrEqualThan: '<=',
      operator_LessThan: '<',
      operator_Minus: '-',
      operator_Multiply: '*',
      operator_Negation: '!',
      operator_NotEqual: '!=',
      operator_Or: '||',
      operator_Otherwise: 'otherwise',
      operator_Plus: '+',
      operator_Modulo: '%',
      operator_BitwiseOr: '|',
      operator_BitwiseAnd: '&',
      operator_BitwiseXor: '^',
      operator_BitwiseLeftShift: '<<',
      operator_BitwiseRightShift: '>>'
    }.transform_values { |v| CGI::escapeHTML(v) }.freeze

    def translate(e, tokens = nil)
      translate!(e, tokens)
    rescue
      '<unknown expectation>'
    end

    def translate!(e, tokens = nil)
      e = e.as_v2
      key = key_for e.binding, e.inspection
      ::I18n.t key, translation_params(e, tokens)
    end

    alias t translate

    private

    def translation_params(e, tokens)
      with_tokens tokens,
                    binding: t_binding(e.binding),
                    target: t_target(e.inspection),
                    must: t_must(e.inspection),
                    matching: t_matching(tokens, e.inspection)
    end

    def key_for(binding, inspection)
      "#{inspection.i18n_namespace}.#{inspection.type}#{inspection.target ? inspection.target.i18n_suffix : nil}"
    end

    def t_binding(binding)
      binding == '*' ? ::I18n.t("mulang.expectation.solution") : "<code>#{Mulang::Inspection.parse_binding_name binding}</code>"
    end

    def t_must(parsed)
      ::I18n.t("mulang.expectation.#{parsed.negated? ? 'must_not' : 'must' }")
    end

    def t_target(parsed)
      "<code>#{parsed.target.value}</code>" if parsed.target
    end

    def t_matching(tokens, parsed)
      ::I18n.t("mulang.expectation.#{parsed.matcher.type}", with_tokens(tokens, value: parsed.matcher.value)) if parsed.matcher
    end

    def with_tokens(tokens, params)
      params.merge(DEFAULT_TOKENS).merge(tokens || {})
    end
  end
end

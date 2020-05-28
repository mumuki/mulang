module Mulang::Expectation::I18n
  class << self
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
      hash = if tokens.nil?
        {}
      elsif tokens.is_a?(Hash)
        tokens
      else
        params.merge(Mulang::Tokens::TOKENS.indifferent_get(tokens))
      end

      params.merge(Mulang::Tokens::DEFAULT_TOKENS.merge(hash))
    end
  end
end

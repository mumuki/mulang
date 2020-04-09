module Mulang::Expectation::I18n
  class << self
    DEFAULT_KEYWORDS = {
      keyword_entry_point: :program,
      keyword_fail: :fail,
      keyword_false: :false,
      keyword_findall: :findall,
      keyword_for: :for,
      keyword_forall: :forall,
      keyword_foreach: :foreach,
      keyword_if: :if,
      keyword_is: :is,
      keyword_not: :not,
      keyword_null: :null,
      keyword_repeat: :repeat,
      keyword_switch: :switch,
      keyword_true: :true,
      keyword_while: :while,
      keyword_yield: :yield
    }

    def translate(e, keywords = nil)
      translate!(e, keywords)
    rescue
      '<unknown expectation>'
    end

    def translate!(e, keywords = nil)
      e = e.as_v2
      key = key_for e.binding, e.inspection
      ::I18n.t key, translation_params(e, keywords)
    end

    alias t translate

    private

    def translation_params(e, keywords)
      with_keywords keywords,
                    binding: t_binding(e.binding),
                    target: t_target(e.inspection),
                    must: t_must(e.inspection),
                    matching: t_matching(keywords, e.inspection)
    end

    def key_for(binding, inspection)
      "#{inspection.i18n_namespace}.#{inspection.type}#{inspection.target ? inspection.target.i18n_suffix : nil}"
    end

    def t_binding(binding)
      binding == '*' ? ::I18n.t("mulang.expectation.solution") : "<strong>#{Mulang::Inspection.parse_binding_name binding}</strong>"
    end

    def t_must(parsed)
      ::I18n.t("mulang.expectation.#{parsed.negated? ? 'must_not' : 'must' }")
    end

    def t_target(parsed)
      "<strong>#{parsed.target.value}</strong>" if parsed.target
    end

    def t_matching(keywords, parsed)
      ::I18n.t("mulang.expectation.#{parsed.matcher.type}", with_keywords(keywords, value: parsed.matcher.value)) if parsed.matcher
    end

    def with_keywords(keywords, params)
      params.merge(DEFAULT_KEYWORDS).merge(keywords || {})
    end
  end
end

module Mulang::Expectation::I18n
  class << self
    DEFAULT_KEYWORDS = {
      keyword_null: :null,
      keyword_if: :if,
      keyword_is: :is,
      keyword_fail: :fail,
      keyword_findall: :findall,
      keyword_forall: :forall,
      keyword_foreach: :foreach,
      keyword_not: :not,
      keyword_repeat: :repeat,
      keyword_switch: :switch,
      keyword_while: :while,
    }

    def translate(e, keywords = nil)
      e = e.as_v2
      key = key_for e.binding, e.inspection
      ::I18n.t key, translation_params(e, keywords)
    rescue
      '<unknown expectation>'
    end

    alias t translate

    private

    def translation_params(e, keywords)
      {
        binding: t_binding(e.binding),
        target: t_target(e.inspection),
        must: t_must(e.inspection)
      }.merge(DEFAULT_KEYWORDS).merge(keywords || {})
    end

    def key_for(binding, inspection)
      "mulang.inspection.#{inspection.type}#{inspection.target ? inspection.target.i18n_suffix : nil}"
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
  end
end

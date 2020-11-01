module Mulang::Language
  class Native
    def initialize(language)
      @language = language
    end

    def ast(content)
      Mulang.analyse(ast_analysis(content))['intermediateLanguage'] rescue nil
    end

    def ast_analysis(content)
      {
        sample: { tag: 'CodeSample', language: @language, content: content },
        spec: { expectations: [], smellsSet: { tag: 'NoSmells' }, includeIntermediateLanguage: true }
      }
    end

    def sample(content)
      {
        tag: 'CodeSample',
        language: @language,
        content: content
      }
    end
  end

  class External
    def initialize(normalization_options = {}, &tool)
      @normalization_options = normalization_options
      @tool = block_given? ? tool : proc { |it| it }
    end

    def ast(content)
      @tool.call(content) rescue nil
    end

    def sample(content)
      {
        tag: 'MulangSample',
        normalizationOptions: @normalization_options,
        ast: ast(content)
      }
    end
  end
end

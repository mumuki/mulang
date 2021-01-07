module Mulang::Language
  class Native
    def initialize(language)
      @language = language
    end

    def ast(content, **options)
      Mulang.analyse(ast_analysis(content, **options))['outputAst'] rescue nil
    end

    def ast_analysis(content, **options)
      {
        sample: { tag: 'CodeSample', language: @language, content: content },
        spec: {
          expectations: [],
          smellsSet: { tag: 'NoSmells' },
          includeOutputAst: true,
          normalizationOptions: options
        }.compact
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
    def initialize(&tool)
      @tool = block_given? ? tool : proc { |it| it }
    end

    def ast(content, **args)
      @tool.call(content) rescue nil
    end

    def sample(content)
      {
        tag: 'MulangSample',
        ast: ast(content)
      }
    end
  end
end

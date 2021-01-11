module Mulang::Language
  class Base
    def transformed_asts(content, operations, **options)
      Mulang.analyse(transformed_asts_analysis(content, operations, **options), **options)['transformedAsts'] rescue nil
    end

    def transformed_asts_analysis(content, operations, **options)
      {
        sample: sample(content),
        spec: {
          expectations: [],
          smellsSet: { tag: 'NoSmells' },
          includeOutputAst: false,
          transformationSpecs: operations,
          normalizationOptions: normalization_options(options)
        }.compact
      }
    end

    def normalization_options(**options)
      options.except(:serialization).presence
    end
  end

  class Native < Base
    def initialize(language)
      @language = language
    end

    def ast(content, **options)
      Mulang.analyse(ast_analysis(content, **options), **options)['outputAst'] rescue nil
    end

    def ast_analysis(content, **options)
      {
        sample: sample(content),
        spec: {
          expectations: [],
          smellsSet: { tag: 'NoSmells' },
          includeOutputAst: true,
          normalizationOptions: normalization_options(options)
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

  class External < Base
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

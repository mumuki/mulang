module Mulang::Language
  class Base
    def identifiers(content, **options)
      Mulang.analyse(identifiers_analysis(content, **options), **options)['outputIdentifiers'] rescue nil
    end

    def identifiers_analysis(content, **options)
      base_analysis content, {includeOutputIdentifiers: true}, **options
    end

    def transformed_asts(content, operations, **options)
      Mulang.analyse(transformed_asts_analysis(content, operations, **options), **options)['transformedAsts'] rescue nil
    end

    def transformed_asts_analysis(content, operations, **options)
      base_analysis content, {transformationSpecs: operations}, **options
    end

    def normalization_options(**options)
      options.except(:serialization).presence
    end

    private

    def base_analysis(content, spec, **options)
      {
        sample: sample(content),
        spec: {
          expectations: [],
          smellsSet: { tag: 'NoSmells' },
          includeOutputAst: false,
          normalizationOptions: normalization_options(options)
        }.merge(spec).compact
      }
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
      base_analysis content, {includeOutputAst: true}, **options
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

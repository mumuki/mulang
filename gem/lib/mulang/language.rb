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

    def ast(content, **options)
      Mulang.analyse(ast_analysis(content, **options), **options)['outputAst'] rescue nil
    end

    def ast_analysis(content, **options)
      base_analysis content, {includeOutputAst: true}, **options
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
    attr_accessor :name

    def initialize(language_name)
      @name = language_name
    end

    def sample(content)
      {
        tag: 'CodeSample',
        language: @name,
        content: content
      }
    end
  end

  class External < Base
    attr_accessor :name

    def initialize(language_name = nil, &tool)
      @name = language_name
      @tool = block_given? ? tool : proc { |it| it }
    end

    def ast(content, **args)
      if args[:serialization]
        super
      else
        call_tool content
      end
    end

    def sample(content)
      {
        tag: 'MulangSample',
        ast: call_tool(content)
      }
    end

    def base_analysis(*)
      super.deep_merge(spec: {originalLanguage: @name}.compact)
    end

    private

    def call_tool(content)
      @tool.call(content) rescue nil
    end
  end
end

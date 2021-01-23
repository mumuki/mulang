module Mulang
  class Code
    attr_accessor :language, :content
    def initialize(language, content)
      @language = language
      @content  = content
    end

    def identifiers(**options)
      @language.identifiers @content, **options
    end

    def ast(**options)
      @language.ast @content, **options
    end

    def ast_analysis(**options)
      @language.ast_analysis @content, **options
    end

    def transformed_asts(operations, **options)
      @language.transformed_asts @content, operations, **options
    end

    def transformed_asts_analysis(operations, **options)
      @language.transformed_asts_analysis @content, operations, **options
    end

    def sample
      @language.sample @content
    end

    def analysis(spec)
      { sample: sample, spec: spec }
    end

    def analyse(spec, **options)
      Mulang.analyse analysis(spec), **options
    end

    def expect(binding='*', inspection)
      expectation = Mulang::Expectation.parse(binding: binding, inspection: inspection).as_v2.to_h
      expectation_results_for(analyse(expectations: [expectation])).first['result']
    end

    def custom_expect(edl)
      expectation_results_for(analyse(customExpectations: edl))
        .map { |e| [e['expectation']['inspection'], e['result']] }
        .to_h
    end

    alias query expect
    alias custom_query custom_expect

    def self.native(language_name, content)
      new Mulang::Language::Native.new(language_name), content
    end

    def self.native!(*args)
      native(*args).tap { |it| it.expect('Parses') }
    end

    def self.external(language_name = nil, content, &tool)
      new Mulang::Language::External.new(language_name, &tool), content
    end

    def self.analyse_many(codes, spec, **options)
      run_many(codes, **options) { |it| it.analysis(spec)  }
    end

    def self.ast_many(codes, **options)
      run_many(codes, key: 'outputAst', **options) { |it| it.ast_analysis(**options) }
    end

    def self.transformed_asts_many(codes, operations, **options)
      run_many(codes, key: 'transformedAsts', **options) { |it| it.transformed_asts_analysis(operations, **options) }
    end

    private

    def self.run_many(codes, key: nil, **options)
      result = Mulang.analyse(codes.map { |it| yield it }, **options)
      key ? result.map { |it| it[key] } : result
    end

    def expectation_results_for(result)
      raise result['reason'] if result['tag'] == 'AnalysisFailed'
      result['expectationResults']
    end
  end
end

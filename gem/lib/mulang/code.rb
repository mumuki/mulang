module Mulang
  class Code
    attr_accessor :language, :content
    def initialize(language, content)
      @language = language
      @content  = content
    end

    def ast
      @language.ast @content
    end

    def ast_analysis
      @language.ast_analysis @content
    end

    def sample
      @language.sample @content
    end

    def analysis(spec)
      { sample: sample, spec: spec }
    end

    def analyse(spec)
      Mulang.analyse analysis(spec)
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

    def self.external(content, &tool)
      new Mulang::Language::External.new(&tool), content
    end

    def self.ast(ast)
      new Mulang::Language::External.new, ast
    end

    def self.analyse_many(codes, spec)
      Mulang.analyse codes.map { |it| it.analysis(spec)  }
    end

    def self.ast_many(codes)
      Mulang.analyse(codes.map { |it| it.ast_analysis  }).map { |it| it['intermediateLanguage'] }
    end

    private

    def expectation_results_for(result)
      raise result['reason'] if result['tag'] == 'AnalysisFailed'
      result['expectationResults']
    end
  end
end

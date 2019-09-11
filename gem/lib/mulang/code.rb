module Mulang
  class Code
    def initialize(language, content)
      @language = language
      @content  = content
    end

    def ast
      @language.ast @content
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

    def expect(binding='*', inspection)
      result = analyse(expectations: [{binding: binding, inspection: inspection}])

      raise result['reason'] if result['tag'] == 'AnalysisFailed'

      result['expectationResults'].first['result']
    end

    alias query expect
  end
end

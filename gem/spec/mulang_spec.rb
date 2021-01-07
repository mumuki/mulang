require "spec_helper"

describe Mulang::Code do
  context 'when language is javascript' do
    let(:code) { Mulang::Code.native('JavaScript', 'let x = 1') }

    it { expect(code.ast).to eq "tag"=>"Variable", "contents"=>["x", {"tag"=>"MuNumber", "contents"=>1}] }
    it { expect(code.analyse expectations: [], smellsSet: { tag: 'NoSmells' }). to eq 'tag'=>'AnalysisCompleted',
                                                                                      'intermediateLanguage'=>nil,
                                                                                      'signatures'=>[],
                                                                                      'smells'=>[],
                                                                                      'expectationResults'=>[],
                                                                                      'testResults' => [] }

    it { expect(code.expect 'x', 'HasBinding').to be true }
    it { expect(code.expect 'y', 'HasBinding').to be false }

    it { expect(code.expect 'Assigns:x').to be true }
    it { expect(code.expect 'Assigns:y').to be false }

    it { expect(code.expect '*', 'Assigns:x:WithLiteral').to be true }
    it { expect(code.expect '*', 'Assigns:x:WithNumber:1').to be true }
    it { expect(code.expect '*', 'Assigns:x:WithNumber:2').to be false }

    it { expect(code.custom_expect %q{
                  expectation "assigns 1":
                    assigns with 1;
                  expectation "assigns 2":
                    assigns with 2}).to eq 'assigns 1' => true,
                                           'assigns 2' => false }

    it { expect { code.expect ':foo' }.to raise_error 'Invalid inspection :foo' }
    pending { expect { code.custom_expect 'dfsdfsdf dsfsd sdfsdf' }.to raise_error 'unexpected token dfsdfsdf' }
  end

  context 'when language is lowercase' do
    let(:code) { Mulang::Code.native('Haskell', 'x = 1') }

    it { expect(code.ast).to eq 'tag'=>'Variable', 'contents'=>['x', {'tag'=>'MuNumber', 'contents'=>1}] }
  end

  context 'when code is well-formed' do
    let(:code) { Mulang::Code.native('Haskell', 'x = 1') }

    it { expect(code.ast).to eq 'tag'=>'Variable', 'contents'=>['x', {'tag'=>'MuNumber', 'contents'=>1}] }
  end

  context 'when code is expressed as an ast' do
    let(:ast) { {'tag'=>'Variable', 'contents'=>['x', {'tag'=>'MuNumber', 'contents'=>1}]} }
    let(:code) { Mulang::Code.ast(ast) }

    it { expect(code.ast).to eq ast }
  end

  context 'when code is ill-formed' do
    let(:source) {  '= 1' }
    describe '.native' do
      let(:code) { Mulang::Code.native('Haskell', source) }

      it { expect(code.ast).to be nil }
    end

    describe '.native!' do
      it { expect { Mulang::Code.native!('Haskell', source) }.to raise_error 'Parse error' }
    end

    let(:code) { Mulang::Code.native('Haskell', source) }

    it { expect(code.ast).to be nil }
  end


  context 'when code is well-formed but mulang does not support it' do
    before { allow(JSON).to receive(:parse).and_raise(JSON::ParserError) }
    let(:code) { Mulang::Code.native('Java', 'something that causes a non exhaustive pattern match error in mulang') }

    it { expect(code.ast).to be nil }
  end

  context 'when input is really long' do
    let(:input) { { tag: :Other, contents: ['something' * 100000, nil] } }
    let(:code) { Mulang::Code.external(input) }
    let(:spec) { { expectations: [], smellsSet: { tag: 'NoSmells' } } }

    it { expect { code.analyse(spec) }.not_to raise_error }
  end

  context 'when language is external with normalization options' do
    let(:input) do
      {tag: 'Procedure', contents: ['x', [[[], {tag: 'UnguardedBody', contents: {tag: 'MuNumber', contents: 1}}]]]}
    end
    let(:code) { Mulang::Code.external(input) }

    it do
      expect(code.sample).to eq tag: 'MulangSample', ast: input

    end

    it do
      expect(code.analyse(includeIntermediateLanguage: true,
                          normalizationOptions: {insertImplicitReturn: true})).to eq 'expectationResults' => [],
                                                                                    'intermediateLanguage' => {
                                                                                      'tag'=>'Procedure',
                                                                                      'contents'=>['x', [[[], {
                                                                                        'tag'=>'UnguardedBody',
                                                                                        'contents'=>{'tag'=>'Return', 'contents'=>{'tag'=>'MuNumber', 'contents'=>1}}}]]]},
                                                                                    'signatures' => [],
                                                                                    'smells' => [],
                                                                                    'tag' => 'AnalysisCompleted',
                                                                                    'testResults' => []

    end
  end

  context 'when language is native with normalization options' do
    let(:input) do
      'function x() { 1 }'
    end
    let(:code) { Mulang::Code.native('JavaScript', input) }

    it do
      expect(code.sample).to eq tag: 'CodeSample', language: 'JavaScript', content: input

    end

    it do
      expect(code.analyse(includeIntermediateLanguage: true,
                          normalizationOptions: {insertImplicitReturn: true})).to eq 'expectationResults' => [],
                                                                                    'intermediateLanguage' => {
                                                                                      'tag'=>'Procedure',
                                                                                      'contents'=>['x', [[[], {
                                                                                        'tag'=>'UnguardedBody',
                                                                                        'contents'=>{'tag'=>'Return', 'contents'=>{'tag'=>'MuNumber', 'contents'=>1}}}]]]},
                                                                                    'signatures' => [],
                                                                                    'smells' => [],
                                                                                    'tag' => 'AnalysisCompleted',
                                                                                    'testResults' => []

    end
  end

  context 'when batch mode is used' do
    let(:codes) { 3.times.map { |it| Mulang::Code.native("JavaScript", "let x = #{it}") } }

    it "results are functionally equivalent to standard mode" do
      expect(Mulang::Code.ast_many codes).to eq codes.map(&:ast)
    end
  end
end

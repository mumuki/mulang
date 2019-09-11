require "spec_helper"

describe Mulang::Code do
  context 'when language is javascript' do
    let(:code) { Mulang::Code.native('JavaScript', 'x = 1') }

    it { expect(code.ast).to eq "tag"=>"Assignment", "contents"=>["x", {"tag"=>"MuNumber", "contents"=>1}] }
    it { expect(code.analyse expectations: [], smellsSet: { tag: 'NoSmells' }). to eq 'tag'=>'AnalysisCompleted',
                                                                                      'intermediateLanguage'=>nil,
                                                                                      'signatures'=>[],
                                                                                      'smells'=>[],
                                                                                      'expectationResults'=>[],
                                                                                      'testResults' => [] }

    it { expect(code.query 'Assigns:x').to be true }
    it { expect(code.query 'Assigns:y').to be false }

    it { expect(code.query '*', 'Assigns:x:WithLiteral').to be true }
    it { expect(code.query '*', 'Assigns:x:WithNumber:1').to be true }
    it { expect(code.query '*', 'Assigns:x:WithNumber:2').to be false }

    it { expect(code.custom_query %q{
                  expectation "assigns 1":
                    assigns with 1;
                  expectation "assigns 2":
                    assigns with 2}).to eq 'assigns 1' => true,
                                           'assigns 2' => false }

    pending { expect { code.query 'sadad d sa dasdpsads a' }.to raise_error 'invalid expectation' }
    pending { expect { code.custom_query 'dfsdfsdf dsfsd sdfsdf' }.to raise_error 'unexpected token dfsdfsdf' }
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
end

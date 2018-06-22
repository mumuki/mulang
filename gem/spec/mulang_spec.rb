require "spec_helper"

describe Mulang::Code do
  context 'when language is javascript' do
    let(:code) { Mulang::Code.native('JavaScript', 'x = 1') }

    it { expect(code.ast).to eq "tag"=>"Assignment", "contents"=>["x", {"tag"=>"MuNumber", "contents"=>1}] }
    it { expect(code.analyse expectations: [], smellsSet: { tag: 'NoSmells' }). to eq 'tag'=>'AnalysisCompleted',
                                                                                      'generatedAsts'=>[],
                                                                                      'signatures'=>[],
                                                                                      'smells'=>[],
                                                                                      'expectationResults'=>[] }

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
    let(:code) { Mulang::Code.native('Haskell', '= 1') }

    it { expect(code.ast).to be nil }
  end

  context 'when code is well-formed but mulang does not support it' do
    before { allow(JSON).to receive(:parse).and_raise(JSON::ParserError) }
    let(:code) { Mulang::Code.native('Java', 'something that causes a non exhaustive pattern match error in mulang') }

    it { expect(code.ast).to be nil }
  end
end

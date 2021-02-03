require "spec_helper"

describe Mulang::Code do
  context 'when language is javascript' do
    let(:code) { Mulang::Code.native('JavaScript', 'let x = 1') }
    let(:code_with_function) { Mulang::Code.native('JavaScript', 'let x = 1; function m(a, b) { return a + b + x }') }

    it { expect(code_with_function.identifiers).to eq [["x", "m"], ["a", "b", "x"]] }


    it { expect(code.ast).to eq "tag"=>"Variable", "contents"=>["x", {"tag"=>"MuNumber", "contents"=>1}] }
    it { expect(code.ast serialization: :bracket).to eq "[Variable[x][MuNumber[1.0]]]" }
    it { expect(code.analyse expectations: [], smellsSet: { tag: 'NoSmells' }). to eq 'tag'=>'AnalysisCompleted',
                                                                                      'outputAst'=>nil,
                                                                                      'outputIdentifiers' => nil,
                                                                                      'transformedAsts' => nil,
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

    describe "transformed_asts" do
      let(:code) { Mulang::Code.native('JavaScript', 'let x = {}; let y = 2; console.log(x)') }

      it "can produce one transformed asts" do
        expect(code.transformed_asts [
                      [{tag: :Replace, contents: ["IsLiteral", {tag: :None}]}]
                    ]).to eq [
                      {"contents"=>
                       [{"contents"=>["x", {"tag"=>"None"}], "tag"=>"Object"},
                        {"contents"=>["y", {"tag"=>"None"}], "tag"=>"Variable"},
                        {"contents"=>{"contents"=>"x", "tag"=>"Reference"}, "tag"=>"Print"}],
                      "tag"=>"Sequence"}]
      end

      it "can produce multiple transformed asts with options" do
        expect(code.transformed_asts [
                      [{tag: :Normalize, contents: { convertObjectIntoDict: true }}],
                      [{tag: :Crop, contents: "IsVariable:y"}, {tag: :RenameVariables}]
                    ], convertObjectVariableIntoObject: false).to eq [
                      {"contents"=>
                       [{"contents"=>["x", {"contents"=>{"tag"=>"None"}, "tag"=>"MuDict"}],
                         "tag"=>"Variable"},
                        {"contents"=>["y", {"contents"=>2, "tag"=>"MuNumber"}], "tag"=>"Variable"},
                        {"contents"=>{"contents"=>"x", "tag"=>"Reference"}, "tag"=>"Print"}],
                      "tag"=>"Sequence"},
                     {"contents"=>
                       [{"contents"=>
                          ["mulang_var_n0", {"contents"=>{"tag"=>"None"}, "tag"=>"MuObject"}],
                         "tag"=>"Variable"},
                        {"contents"=>{"contents"=>"mulang_var_n0", "tag"=>"Reference"},
                         "tag"=>"Print"}],
                      "tag"=>"Sequence"}]
      end

      it "can produce multiple transformed asts with serialization options" do
        expect(code.transformed_asts [
                      [{tag: :Normalize, contents: { convertObjectIntoDict: true }}],
                      [{tag: :Crop, contents: "IsVariable:y"}, {tag: :RenameVariables}]
                    ], convertObjectVariableIntoObject: false, serialization: :brace).to eq [
                      "{Sequence{Variable{x}{MuDict{None}}}{Variable{y}{MuNumber{2.0}}}{Print{Reference{x}}}}",
                      "{Sequence{Variable{mulang_var_n0}{MuObject{None}}}{Print{Reference{mulang_var_n0}}}}"
                    ]
      end
    end

    describe 'normalization' do
      let(:code) { Mulang::Code.native('JavaScript', 'let x = {}') }

      it { expect(code.ast convertObjectIntoDict: true).to eq "tag"=>"Variable", "contents"=>["x", {"tag"=>"MuDict", "contents"=>{"tag"=>"None"}}] }
    end

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
    let(:code) { Mulang::Code.external(ast) }

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

    it { expect(code.ast).to eq input }
    it { expect(code.ast serialization: :bracket).to eq "[Procedure[x][Equation[UnguardedBody[MuNumber[1.0]]]]]" }

    it do
      expect(code.sample).to eq tag: 'MulangSample', ast: input
    end

    it do
      expect(code.analyse(includeOutputAst: true,
                          normalizationOptions: {insertImplicitReturn: true})).to eq 'expectationResults' => [],
                                                                                    'outputAst' => {
                                                                                      'tag'=>'Procedure',
                                                                                      'contents'=>['x', [[[], {
                                                                                        'tag'=>'UnguardedBody',
                                                                                        'contents'=>{'tag'=>'Return', 'contents'=>{'tag'=>'MuNumber', 'contents'=>1}}}]]]},
                                                                                    'outputIdentifiers' => nil,
                                                                                    'transformedAsts' => nil,
                                                                                    'signatures' => [],
                                                                                    'smells' => [],
                                                                                    'tag' => 'AnalysisCompleted',
                                                                                    'testResults' => []

    end
  end
  describe 'original language' do
    let(:ast) { {:tag=>:Method, :contents=>[:drive!, [[[], {:tag=>:UnguardedBody, :contents=>{:tag=>:MuNil}}]]]} }
    context 'when language is external with original language name' do
      let(:code) { Mulang::Code.external('Ruby', ast) }
      it { expect(code.language.name).to eq 'Ruby' }
      it { expect(code.ast_analysis[:spec][:originalLanguage]).to eq 'Ruby' }
      it { expect(code.analyse(smellsSet: {tag: :NoSmells, include: ['HasWrongCaseIdentifiers']})['smells']).to eq [] }
      it { expect(code.ast serialization: :bracket).to eq '[Method[drive!][Equation[UnguardedBody[MuNil]]]]' }
    end

    context 'when language is external with no original language name' do
      let(:code) { Mulang::Code.external(ast) }
      it { expect(code.language.name).to be nil }
      it { expect(code.ast_analysis[:spec][:originalLanguage]).to be nil }
      it { expect(code.analyse(smellsSet: {tag: :NoSmells, include: ['HasWrongCaseIdentifiers']})['smells']).to eq [{"binding"=>"drive!", "inspection"=>"HasWrongCaseIdentifiers"}] }
      it { expect(code.ast serialization: :bracket).to eq '[Method[drive!][Equation[UnguardedBody[MuNil]]]]' }
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
      expect(code.ast(insertImplicitReturn: true)).to eq 'tag'=>'Procedure',
                                                          'contents'=>['x', [[[], {
                                                            'tag'=>'UnguardedBody',
                                                            'contents'=>{'tag'=>'Return', 'contents'=>{'tag'=>'MuNumber', 'contents'=>1}}}]]]

    end

    it do
      expect(code.analyse(includeOutputAst: true,
                          normalizationOptions: {insertImplicitReturn: true})).to eq 'expectationResults' => [],
                                                                                      'outputAst' => {
                                                                                        'tag'=>'Procedure',
                                                                                        'contents'=>['x', [[[], {
                                                                                          'tag'=>'UnguardedBody',
                                                                                          'contents'=>{'tag'=>'Return', 'contents'=>{'tag'=>'MuNumber', 'contents'=>1}}}]]]},
                                                                                      'outputIdentifiers' => nil,
                                                                                      'transformedAsts' => nil,
                                                                                      'signatures' => [],
                                                                                      'smells' => [],
                                                                                      'tag' => 'AnalysisCompleted',
                                                                                      'testResults' => []

    end
  end

  context 'when batch mode is used' do
    let(:codes) { 3.times.map { |it| Mulang::Code.native("JavaScript", "let x#{it} = {}") } }


    it "results are functionally equivalent to standard mode" do
      expect(Mulang::Code.ast_many codes).to eq codes.map(&:ast)
    end

    it "results are functionally equivalent to standard mode with normalization options are used" do
      expect(Mulang::Code.ast_many codes, convertObjectIntoDict: true).to eq codes.map { |it| it.ast convertObjectIntoDict: true }
    end

    it "results are functionally equivalent to standard mode with transformations" do
      expect(Mulang::Code.transformed_asts_many codes, [[{tag: :RenameVariables}]]).to eq codes.map { |it| it.transformed_asts [[{tag: :RenameVariables}]] }
    end
  end
end

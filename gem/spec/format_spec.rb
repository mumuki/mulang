require 'spec_helper'

describe Mulang::Expectation do
  subject { Mulang::Expectation }

  describe 'it can check old format' do
    it { expect { subject.parse(binding: 'foo', inspection: 'HasBinding').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasUsage:bar').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasWhile').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasTypeDeclaration').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasTypeSignature').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasAnonymousVariable').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasRepeat').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasNot').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasLambda').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasIf').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasForall').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasFindall').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasComprehension').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasDirectRecursion').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasComposition').check! }.to_not raise_error }
    it { expect { subject.parse(binding: 'foo', inspection: 'HasArity').check! }.to_not raise_error }

    it { expect { subject.parse(binding: 'foo', inspection: 'HasBindin').check! }.to raise_error }
    it { expect { subject.parse(binding: '', inspection: 'HasBinding').check! }.to raise_error }
  end

  describe 'it can guess format' do
    it { expect(subject.guess_type binding: 'foo', inspection: 'HasBinding').to be Mulang::Expectation::V0 }
    it { expect(subject.guess_type binding: 'foo', inspection: 'Uses:*').to be Mulang::Expectation::V2 }
  end

  describe 'it can convert back to hash' do
    context 'after adaptation' do
      it { expect(subject.parse(binding: 'foo', inspection: 'HasWhile').as_v2.to_h).to eq binding: 'foo', inspection: 'UsesWhile' }
    end

    context 'no adaptation' do
      it { expect(subject.parse(binding: 'foo', inspection: 'HasBinding').to_h).to eq binding: 'foo', inspection: 'HasBinding' }
      it { expect(subject.parse(binding: 'foo', inspection: 'HasArity:1').to_h).to eq binding: 'foo', inspection: 'HasArity:1' }
      it { expect(subject.parse(binding: 'foo', inspection: 'DeclaresClass:Golondrina').to_h).to eq binding: 'foo', inspection: 'DeclaresClass:Golondrina' }
      it { expect(subject.parse(binding: 'Intransitive:foo', inspection: 'Uses:*').to_h).to eq binding: 'Intransitive:foo', inspection: 'Uses:*' }
      it { expect(subject.parse(binding: 'foo', inspection: 'DeclaresClass').to_h).to eq binding: 'foo', inspection: 'DeclaresClass' }
      it { expect(subject.parse(binding: 'foo', inspection: 'Assigns:bar').to_h).to eq binding: 'foo', inspection: 'Assigns:bar' }
    end
  end

  describe 'it can adapt to latest format' do
    it { expect(subject.parse(binding: '*', inspection: 'Declares:foo').as_v2)
          .to json_like binding: '*', inspection: {type: 'Declares', target: {type: :unknown, value: 'foo'}, negated: false   }  }
    it { expect(subject.parse(binding: 'foo', inspection: 'HasBinding').as_v2)
          .to json_like binding: '*', inspection: {type: 'Declares', target: {type: :named, value: 'foo'}, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasBinding').as_v2)
          .to json_like binding: '*', inspection: {type: 'Declares', target: {type: :named, value: 'foo'}, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasTypeDeclaration').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresTypeAlias', target: {type: :named, value: 'foo'}, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasTypeDeclaration').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresTypeAlias', target: {type: :named, value: 'foo'}, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasTypeSignature').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresTypeSignature', target: {type: :named, value: 'foo'}, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasTypeSignature').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresTypeSignature', target: {type: :named, value: 'foo'}, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasVariable').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresVariable', target: {type: :named, value: 'foo'}, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasVariable').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresVariable', target: {type: :named, value: 'foo'}, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasArity:1').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresComputationWithArity1', target: {type: :named, value: 'foo'}, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasArity:3').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresComputationWithArity3', target: {type: :named, value: 'foo'}, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasDirectRecursion').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresRecursively', target: {type: :named, value: 'foo'}, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasDirectRecursion').as_v2)
          .to json_like binding: '*', inspection: {type: 'DeclaresRecursively', target: {type: :named, value: 'foo'}, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasComposition').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesComposition', target: nil, negated: false}   }
    it { expect(subject.parse(binding: 'foo', inspection: 'Not:HasComposition').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesComposition', target: nil, negated: true}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasComprehension').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesComprehension', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasForeach').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesForeach', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasIf').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesIf', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasGuards').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesGuards', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasConditional').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesConditional', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasLambda').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesLambda', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasRepeat').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesRepeat', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasWhile').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'UsesWhile', target: nil, negated: false}   }

    it { expect(subject.parse(binding: 'foo', inspection: 'HasUsage:bar').as_v2)
          .to json_like binding: 'foo', inspection: {type: 'Uses', target: {type: :named, value: 'bar'}, negated: false}   }

  end
end

require_relative './spec_helper'


describe Mulang::Inspection do
  it { expect(Mulang::Inspection.parse('Declares')).to json_like(type: 'Declares',
                                                                negated: false,
                                                                target: nil) }

  it { expect(Mulang::Inspection.parse('Not:Declares')).to json_like(type: 'Declares',
                                                                    negated: true,
                                                                    target: nil) }

  it { expect(Mulang::Inspection.parse('Uses:m')).to json_like(type: 'Uses',
                                                              negated: false,
                                                              target: { type: :unknown, value: 'm' }) }

  it { expect(Mulang::Inspection.parse('Not:Uses:m')).to json_like(type: 'Uses',
                                                                  negated: true,
                                                                  target: { type: :unknown, value: 'm' }) }

  it { expect(Mulang::Inspection.parse('Uses:^foo')).to json_like(type: 'Uses',
                                                                  negated: false,
                                                                  target: { type: :except, value: 'foo' }) }

  it { expect(Mulang::Inspection.parse('Uses:=m')).to json_like(type: 'Uses',
                                                               negated: false,
                                                               target: { type: :named, value: 'm' }) }
  it { expect(Mulang::Inspection.parse('Uses:~m')).to json_like(type: 'Uses',
                                                               negated: false,
                                                               target: { type: :like, value: 'm' }) }
  it { expect(Mulang::Inspection.parse('Not:Uses:~m')).to json_like(type: 'Uses',
                                                                   negated: true,
                                                                   target: { type: :like, value: 'm' }) }
  it { expect(Mulang::Inspection.parse('Uses:*')).to json_like(type: 'Uses',
                                                              negated: false,
                                                              target: {type: 'anyone', value: nil}) }

  it { expect(Mulang::Inspection.parse_binding_name('foo')).to eq 'foo' }
  it { expect(Mulang::Inspection.parse_binding_name('Intransitive:foo')).to eq 'foo' }
end

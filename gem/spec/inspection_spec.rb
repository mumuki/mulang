require_relative './spec_helper'


describe Mulang::Inspection do
  it { expect(Mulang::Inspection.parse('Assigns').to_s).to eq 'Assigns' }
  it { expect(Mulang::Inspection.parse('Assigns:x').to_s).to eq 'Assigns:x' }
  it { expect(Mulang::Inspection.parse('Assigns:WithNumber:2').to_s).to eq 'Assigns:WithNumber:2' }
  it { expect(Mulang::Inspection.parse('Assigns:x:WithNumber:2').to_s).to eq 'Assigns:x:WithNumber:2' }

  it { expect(Mulang::Inspection.parse('Declares')).to json_like(type: 'Declares',
                                                                negated: false) }

  it { expect(Mulang::Inspection.parse('Not:Declares')).to json_like(type: 'Declares',
                                                                    negated: true) }

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
                                                              target: {type: 'anyone'}) }

  it { expect(Mulang::Inspection.parse_binding_name('foo')).to eq 'foo' }
  it { expect(Mulang::Inspection.parse_binding_name('Intransitive:foo')).to eq 'foo' }

  it { expect(Mulang::Inspection.parse('Assigns:x:WithNumber:2')).to json_like(type: 'Assigns',
                                                                              negated: false,
                                                                              target: {type: :unknown, value: 'x'},
                                                                              matcher: {type: :with_number, value: "2"}) }
  it { expect(Mulang::Inspection.parse('Not:Assigns:x:WithNumber:2')).to json_like(type: 'Assigns',
                                                                              negated: true,
                                                                              target: {type: :unknown, value: 'x'},
                                                                              matcher: {type: :with_number, value: "2"}) }

  it { expect(Mulang::Inspection.parse("Assigns:x")).to json_like(type: 'Assigns', negated: false, target: {type: :unknown, value: 'x'}) }
  it { expect(Mulang::Inspection.parse("Assigns:WithFalse")).to json_like(type: 'Assigns', negated: false, matcher: {type: :with_false }) }
  it { expect(Mulang::Inspection.parse("Assigns:WithNumber:2")).to json_like(type: 'Assigns', negated: false, matcher: {type: :with_number, value: "2" }) }

  it { expect(Mulang::Inspection.parse("Returns:WithFalse")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_false }) }
  it { expect(Mulang::Inspection.parse("Returns:WithNil")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_nil }) }
  it { expect(Mulang::Inspection.parse("Returns:WithTrue")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_true }) }
  it { expect(Mulang::Inspection.parse("Returns:WithChar:'c'")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_char, value: "'c'" }) }
  it { expect(Mulang::Inspection.parse("Returns:WithNumber:2")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_number, value: "2" }) }
  it { expect(Mulang::Inspection.parse("Returns:WithString:'hooper'")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_string, value: "'hooper'" }) }
  it { expect(Mulang::Inspection.parse("Returns:WithSymbol:grace")).to json_like(type: 'Returns', negated: false, matcher: {type: :with_symbol, value: 'grace' }) }
end

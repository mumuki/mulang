var should = require('should');
let mulang = require('../build/mulang');

describe('es', () => {
  before(() => mulang.I18n.locale = 'es');

  it('Declares', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('la solución debe declarar <strong>foo</strong>'));
  it('DeclaresClass', () => should(mulang.I18n.translate('*', 'DeclaresClass:foo')).eql('la solución debe declarar una clase <strong>foo</strong>'));

  it('DeclaresClass', () => should(mulang.I18n.translate('Mulang', 'DeclaresClass:Inspection')).eql('<strong>Mulang</strong> debe declarar una clase <strong>Inspection</strong>'));

  it('DeclaresObject', () => should(mulang.I18n.translate('*', 'DeclaresObject:foo')).eql('la solución debe declarar un objeto <strong>foo</strong>'));
  it('DeclaresMethod', () => should(mulang.I18n.translate('foo', 'DeclaresMethod:bar')).eql('<strong>foo</strong> debe declarar un método <strong>bar</strong>'));
  it('Declares', () => should(mulang.I18n.translate('foo', 'Declares')).eql('<strong>foo</strong> debe contener declaraciones'));

  it('DeclaresMethod', () => should(mulang.I18n.translate('foo.bar', 'DeclaresMethod')).eql('<strong>foo.bar</strong> debe declarar métodos'));
  it('UsesIf', () => should(mulang.I18n.translate('foo.bar', 'UsesIf')).eql('<strong>foo.bar</strong> debe usar <i>if</i>'));

  it('Not', () => should(mulang.I18n.translate('Intransitive:foo', 'Not:UsesLambda')).eql('<strong>foo</strong> no debe emplear expresiones lambda'));

  it('Uses', () => should(mulang.I18n.translate('foo', 'Uses:*')).eql('<strong>foo</strong> debe delegar'));
  it('DeclaresMethod', () => should(mulang.I18n.translate('foo', 'DeclaresMethod:*')).eql('<strong>foo</strong> debe declarar métodos'));
  it('Uses', () => should(mulang.I18n.translate('foo', 'Uses:baz')).eql('<strong>foo</strong> debe utilizar <strong>baz</strong>'));
  it('Uses', () => should(mulang.I18n.translate('foo', 'Uses')).eql('<strong>foo</strong> debe delegar'));
  it('UsesForall', () => should(mulang.I18n.translate('foo', 'UsesForall')).eql('<strong>foo</strong> debe utilizar <i>forall</i>'));

  it('Not', () => should(mulang.I18n.translate('foo', 'Not:Uses:*')).eql('<strong>foo</strong> no debe delegar'));
  it('Not', () => should(mulang.I18n.translate('foo', 'Not:Uses')).eql('<strong>foo</strong> no debe delegar'));
  it('Not', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<strong>foo</strong> no debe utilizar <strong>baz</strong>'));
  it('Not', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<strong>foo</strong> no debe emplear expresiones lambda'));

  it('DeclaresClass', () => should(mulang.I18n.translate('*', 'DeclaresClass')).eql('la solución debe declarar clases'));
  it('Not', () => should(mulang.I18n.translate('*', 'Not:DeclaresMethod')).eql('la solución no debe declarar métodos'));
  it('Not', () => should(mulang.I18n.translate('*', 'Not:DeclaresClass')).eql('la solución no debe declarar clases'));

  it('DeclaresObject', () => should(mulang.I18n.translate('foo', 'DeclaresObject')).eql('<strong>foo</strong> debe declarar objetos'));
  it('Not', () => should(mulang.I18n.translate('*', 'Not:DeclaresClass')).eql('la solución no debe declarar clases'));
  it('UsesAnonymousVariable', () => should(mulang.I18n.translate('foo', 'UsesAnonymousVariable')).eql('<strong>foo</strong> debe utilizar una variable anónima'));
  it('UsesStaticPolymorphism', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('la solución debe usar polimorfismo'));

  it('HasRedundantIf', () => should(mulang.I18n.translate('foo', 'HasRedundantIf')).eql('<strong>foo</strong> tiene ifs innecesarios'));
  it('HasRedundantBooleanComparison', () => should(mulang.I18n.translate('foo', 'HasRedundantBooleanComparison')).eql('<strong>foo</strong> hace comparaciones booleanas innecesarias'));
  it('UsesFail', () => should(mulang.I18n.translate('bar', 'UsesFail')).eql('<strong>bar</strong> usa <i>fail</li>, lo cual es una mala práctica'));
  it('HasEmptyIfBranches', () => should(mulang.I18n.translate('foo', 'HasEmptyIfBranches')).eql('<strong>foo</strong> tiene ramas de <i>if</i> vacías'));
  it('HasEmptyRepeat', () => should(mulang.I18n.translate('foo', 'HasEmptyRepeat')).eql('<strong>foo</strong> tiene un <i>repeat</i> vacío'));
  it('ShouldInvertIfCondition', () => should(mulang.I18n.translate('foo', 'ShouldInvertIfCondition')).eql('<strong>foo</strong> debería invertir la condición del <i>if</i> e intercambiar las ramas'));
  it('HasRedundantRepeat', () => should(mulang.I18n.translate('foo', 'HasRedundantRepeat')).eql('<strong>foo</strong> tiene un <i>repeat</i> innecesario'));
  it('HasUnreachableCode', () => should(mulang.I18n.translate('foo', 'HasUnreachableCode')).eql('<strong>foo</strong> tiene código inalcanzable'));
  it('HasLongParameterList', () => should(mulang.I18n.translate('foo', 'HasLongParameterList')).eql('<strong>foo</strong> tiene demasiados parámetros. Te podría estar faltando una abstracción'));
  it('OverridesEqualOrHashButNotBoth', () => should(mulang.I18n.translate('foo', 'OverridesEqualOrHashButNotBoth')).eql('<strong>foo</strong> redefine los métodos <i>equals</i> o <i>hash</i>, pero no ambos'));
});

describe('pt', () => {
  before(() => { mulang.I18n.locale = 'pt' });

  it('Declares:foo', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('a solução deve declarar <strong>foo</strong>'));
  it('Not:Uses:baz', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<strong>foo</strong> não deve usar <strong>baz</strong>'));
  it('Not:UsesLambda', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<strong>foo</strong> não deve usar expressões lambda'));
  it('UsesStaticPolymorphism', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('a solução deve usar polimorfismo'));
});


describe('en', () => {
  before(() => { mulang.I18n.locale = 'en' });

  it('Declares:foo', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('solution must declare <strong>foo</strong>'));
  it('Not:Uses:baz', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<strong>foo</strong> must not use <strong>baz</strong>'));
  it('Not:UsesLambda', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<strong>foo</strong> must not use lambda expressions'));
  it('UsesStaticPolymorphism', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('solution must use polymorphism'));

  it('HasDeclarationTypos:foo', () =>
    should(mulang.I18n.translate('Foo', 'HasDeclarationTypos:foo')).eql('Solution must declare <strong>foo</strong>, but declares <strong>Foo</strong>. May you have made a typo?'));
  it('HasUsageTypos:foo', () =>
    should(mulang.I18n.translate('Foo', 'HasUsageTypos:foo')).eql('Solution must use <strong>foo</strong>, but it uses <strong>Foo</strong>. May you have made a typo?'));
});

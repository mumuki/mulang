var should = require('should');
let mulang = require('../build/mulang');

describe('es', () => {
  before(() => mulang.I18n.locale = 'es');

  it('Declares', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('la solución debe declarar <code>foo</code>'));
  it('DeclaresClass', () => should(mulang.I18n.translate('*', 'DeclaresClass:foo')).eql('la solución debe declarar una clase <code>foo</code>'));

  it('DeclaresClass', () => should(mulang.I18n.translate('Mulang', 'DeclaresClass:Inspection')).eql('<code>Mulang</code> debe declarar una clase <code>Inspection</code>'));

  it('DeclaresObject', () => should(mulang.I18n.translate('*', 'DeclaresObject:foo')).eql('la solución debe declarar un objeto <code>foo</code>'));
  it('DeclaresMethod', () => should(mulang.I18n.translate('foo', 'DeclaresMethod:bar')).eql('<code>foo</code> debe declarar un método <code>bar</code>'));
  it('Declares', () => should(mulang.I18n.translate('foo', 'Declares')).eql('<code>foo</code> debe contener declaraciones'));

  it('DeclaresMethod', () => should(mulang.I18n.translate('foo.bar', 'DeclaresMethod')).eql('<code>foo.bar</code> debe declarar métodos'));
  it('UsesIf', () => should(mulang.I18n.translate('foo.bar', 'UsesIf')).eql('<code>foo.bar</code> debe usar <code>if</code>'));

  it('Not', () => should(mulang.I18n.translate('Intransitive:foo', 'Not:UsesLambda')).eql('<code>foo</code> no debe emplear expresiones lambda'));

  it('Uses', () => should(mulang.I18n.translate('foo', 'Uses:*')).eql('<code>foo</code> debe delegar'));
  it('DeclaresMethod', () => should(mulang.I18n.translate('foo', 'DeclaresMethod:*')).eql('<code>foo</code> debe declarar métodos'));
  it('Uses', () => should(mulang.I18n.translate('foo', 'Uses:baz')).eql('<code>foo</code> debe utilizar <code>baz</code>'));
  it('Uses', () => should(mulang.I18n.translate('foo', 'Uses')).eql('<code>foo</code> debe delegar'));
  it('UsesForall', () => should(mulang.I18n.translate('foo', 'UsesForall', 'Prolog')).eql('<code>foo</code> debe utilizar <code>forall</code>'));

  it('Not', () => should(mulang.I18n.translate('foo', 'Not:Uses:*')).eql('<code>foo</code> no debe delegar'));
  it('Not', () => should(mulang.I18n.translate('foo', 'Not:Uses')).eql('<code>foo</code> no debe delegar'));
  it('Not', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<code>foo</code> no debe utilizar <code>baz</code>'));
  it('Not', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<code>foo</code> no debe emplear expresiones lambda'));

  it('DeclaresClass', () => should(mulang.I18n.translate('*', 'DeclaresClass')).eql('la solución debe declarar clases'));
  it('Not', () => should(mulang.I18n.translate('*', 'Not:DeclaresMethod')).eql('la solución no debe declarar métodos'));
  it('Not', () => should(mulang.I18n.translate('*', 'Not:DeclaresClass')).eql('la solución no debe declarar clases'));

  it('DeclaresObject', () => should(mulang.I18n.translate('foo', 'DeclaresObject')).eql('<code>foo</code> debe declarar objetos'));
  it('Not', () => should(mulang.I18n.translate('*', 'Not:DeclaresClass')).eql('la solución no debe declarar clases'));
  it('UsesAnonymousVariable', () => should(mulang.I18n.translate('foo', 'UsesAnonymousVariable')).eql('<code>foo</code> debe utilizar una variable anónima'));
  it('UsesStaticPolymorphism', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('la solución debe usar polimorfismo'));

  it('HasRedundantIf', () => should(mulang.I18n.translate('foo', 'HasRedundantIf')).eql('<code>foo</code> tiene ifs innecesarios'));
  it('HasRedundantBooleanComparison', () => should(mulang.I18n.translate('foo', 'HasRedundantBooleanComparison')).eql('<code>foo</code> hace comparaciones booleanas innecesarias'));
  it('UsesFail', () => should(mulang.I18n.translate('bar', 'UsesFail', 'Prolog')).eql('<code>bar</code> usa <code>fail</li>, lo cual es una mala práctica'));
  it('HasEmptyIfBranches', () => should(mulang.I18n.translate('foo', 'HasEmptyIfBranches')).eql('<code>foo</code> tiene ramas de <code>if</code> vacías'));
  it('HasEmptyRepeat', () => should(mulang.I18n.translate('foo', 'HasEmptyRepeat')).eql('<code>foo</code> tiene un <code>repeat</code> vacío'));
  it('ShouldInvertIfCondition', () => should(mulang.I18n.translate('foo', 'ShouldInvertIfCondition')).eql('<code>foo</code> debería invertir la condición del <code>if</code> e intercambiar las ramas'));
  it('HasRedundantRepeat', () => should(mulang.I18n.translate('foo', 'HasRedundantRepeat')).eql('<code>foo</code> tiene un <code>repeat</code> innecesario'));
  it('HasUnreachableCode', () => should(mulang.I18n.translate('foo', 'HasUnreachableCode')).eql('<code>foo</code> tiene código inalcanzable'));
  it('HasLongParameterList', () => should(mulang.I18n.translate('foo', 'HasLongParameterList')).eql('<code>foo</code> tiene demasiados parámetros. Te podría estar faltando una abstracción'));
  it('OverridesEqualOrHashButNotBoth', () => should(mulang.I18n.translate('foo', 'OverridesEqualOrHashButNotBoth')).eql('<code>foo</code> redefine los métodos <code>equals</code> o <code>hash</code>, pero no ambos'));
});

describe('pt', () => {
  before(() => { mulang.I18n.locale = 'pt' });

  it('Declares:foo', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('a solução deve declarar <code>foo</code>'));
  it('Not:Uses:baz', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<code>foo</code> não deve usar <code>baz</code>'));
  it('Not:UsesLambda', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<code>foo</code> não deve usar expressões lambda'));
  it('UsesStaticPolymorphism', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('a solução deve usar polimorfismo'));
});


describe('en', () => {
  before(() => { mulang.I18n.locale = 'en' });

  it('Declares:foo', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('solution must declare <code>foo</code>'));
  it('Not:Uses:baz', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<code>foo</code> must not use <code>baz</code>'));
  it('Not:UsesLambda', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<code>foo</code> must not use lambda expressions'));
  it('UsesStaticPolymorphism', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('solution must use polymorphism'));

  it('HasDeclarationTypos:foo', () =>
    should(mulang.I18n.translate('Foo', 'HasDeclarationTypos:foo')).eql('Solution must declare <code>foo</code>, but declares <code>Foo</code>. Perhaps you meant <code>foo</code>?'));
  it('HasUsageTypos:foo', () =>
    should(mulang.I18n.translate('Foo', 'HasUsageTypos:foo')).eql('Solution must use <code>foo</code>, but it uses <code>Foo</code>. Perhaps you meant <code>foo</code>?'));
});

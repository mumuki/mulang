require 'spec_helper'

describe Mulang::Inspection::I18n do

  def expectation(binding, inspection)
    Mulang::Inspection::Expectation.parse(binding: binding, inspection: inspection)
  end

  context 'en locale' do
    before { I18n.locale = :en }

    it { expect(expectation('*', 'Declares:foo').translate).to eq('solution must declare <strong>foo</strong>') }
    it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<strong>foo</strong> must not use <strong>baz</strong>') }
    it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<strong>foo</strong> must not use lambda expressions') }
    it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('solution must use polymorphism') }
  end

  context 'pt locale' do
    before { I18n.locale = :pt }

    it { expect(expectation('*', 'Declares:foo').translate).to eq('a solução deve declarar <strong>foo</strong>') }
    it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<strong>foo</strong> não deve usar <strong>baz</strong>') }
    it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<strong>foo</strong> não deve usar expressões lambda') }
    it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('a solução deve usar polimorfismo') }

  end

  context 'es locale' do
    before { I18n.locale = :es }

    describe 'source exectations' do
      it { expect(expectation('*', 'SourceRepeats:foo(X)').translate).to eq('la solución debe usar <strong>foo(X)</strong> más de una vez') }
      it { expect(expectation('*', 'SourceContains:foo(X)').translate).to eq('la solución debe usar <strong>foo(X)</strong>') }
      it { expect(expectation('*', 'SourceEquals:foo(X)').translate).to eq('la solución debe ser igual a <strong>foo(X)</strong>') }
      it { expect(expectation('*', 'SourceEqualsIgnoreSpaces:foo(X)').translate).to eq('la solución debe ser igual a <strong>foo(X)</strong>') }
    end

    describe 'v0 exectations' do
      it { expect(expectation('foo', 'HasBinding').translate).to eq('la solución debe declarar <strong>foo</strong>') }
      it { expect(expectation('foo', 'HasUsage:bar').translate).to eq('<strong>foo</strong> debe utilizar <strong>bar</strong>') }
      it { expect(expectation('foo', 'HasWhile').translate).to eq('<strong>foo</strong> debe utilizar un <i>while</i>') }
      it { expect(expectation('foo', 'HasTypeDeclaration').translate).to eq('la solución debe declarar un sinónimo de tipo <strong>foo</strong>') }
      it { expect(expectation('foo', 'HasTypeSignature').translate).to eq('la solución debe declarar una firma <strong>foo</strong>') }
      it { expect(expectation('foo', 'HasRepeat').translate).to eq('<strong>foo</strong> debe usar <i>repeat</i>') }
      it { expect(expectation('foo', 'HasNot').translate).to eq('<strong>foo</strong> debe utilizar <i>not</i>') }
      it { expect(expectation('foo', 'HasLambda').translate).to eq('<strong>foo</strong> debe emplear expresiones lambda') }
      it { expect(expectation('foo', 'HasIf').translate).to eq('<strong>foo</strong> debe usar <i>if</i>') }
      it { expect(expectation('foo', 'HasForall').translate).to eq('<strong>foo</strong> debe utilizar <i>forall</i>') }
      it { expect(expectation('foo', 'HasFindall').translate).to eq('<strong>foo</strong> debe utilizar <i>findall</i>') }
      it { expect(expectation('foo', 'HasComprehension').translate).to eq('<strong>foo</strong> debe emplear listas por comprensión') }
      it { expect(expectation('foo', 'HasDirectRecursion').translate).to eq('<strong>foo</strong> debe estar declarado recursivamente') }
      it { expect(expectation('foo', 'HasComposition').translate).to eq('<strong>foo</strong> debe usar composición') }
      it { expect(expectation('foo', 'UsesLoop').translate).to eq('<strong>foo</strong> debe usar un bucle') }

      it { expect(expectation('foo', 'HasIf').translate(keyword_if: 'si')).to eq('<strong>foo</strong> debe usar <i>si</i>') }
      it { expect(expectation('foo', 'HasIf').translate(keyword_repeat: 'repetir')).to eq('<strong>foo</strong> debe usar <i>if</i>') }
    end

    describe 'v2 expectations' do
      it { expect(expectation('*', 'Declares:foo').translate).to eq('la solución debe declarar <strong>foo</strong>') }
      it { expect(expectation('*', 'DeclaresClass:foo').translate).to eq('la solución debe declarar una clase <strong>foo</strong>') }

      it { expect(expectation('Mulang', 'DeclaresClass:Inspection').translate).to eq('<strong>Mulang</strong> debe declarar una clase <strong>Inspection</strong>') }

      it { expect(expectation('*', 'DeclaresObject:foo').translate).to eq('la solución debe declarar un objeto <strong>foo</strong>') }
      it { expect(expectation('foo', 'DeclaresMethod:bar').translate).to eq('<strong>foo</strong> debe declarar un método <strong>bar</strong>') }
      it { expect(expectation('foo', 'Declares').translate).to eq('<strong>foo</strong> debe contener declaraciones') }

      it { expect(expectation('foo.bar', 'DeclaresMethod').translate).to eq('<strong>foo.bar</strong> debe declarar métodos') }
      it { expect(expectation('foo.bar', 'UsesIf').translate).to eq('<strong>foo.bar</strong> debe usar <i>if</i>') }

      it { expect(expectation('Intransitive:foo', 'Not:UsesLambda').translate).to eq('<strong>foo</strong> no debe emplear expresiones lambda') }

      it { expect(expectation('foo', 'Uses:=baz').translate).to eq('<strong>foo</strong> debe utilizar <strong>baz</strong>') }
      it { expect(expectation('foo', 'Uses:~baz').translate).to eq('<strong>foo</strong> debe delegar en algo parecido a <strong>baz</strong>') }
      it { expect(expectation('foo', 'DeclaresMethod:~baz').translate).to eq('<strong>foo</strong> debe declarar un método parecido a <strong>baz</strong>') }
      it { expect(expectation('foo', 'Uses:*').translate).to eq('<strong>foo</strong> debe delegar') }
      it { expect(expectation('foo', 'DeclaresMethod:*').translate).to eq('<strong>foo</strong> debe declarar métodos') }
      it { expect(expectation('foo', 'Uses:baz').translate).to eq('<strong>foo</strong> debe utilizar <strong>baz</strong>') }
      it { expect(expectation('foo', 'Uses').translate).to eq('<strong>foo</strong> debe delegar') }
      it { expect(expectation('foo', 'UsesForall').translate).to eq('<strong>foo</strong> debe utilizar <i>forall</i>') }

      it { expect(expectation('foo', 'Not:Uses:=baz').translate).to eq('<strong>foo</strong> no debe utilizar <strong>baz</strong>') }
      it { expect(expectation('foo', 'Not:Uses:~baz').translate).to eq('<strong>foo</strong> no debe delegar en algo parecido a <strong>baz</strong>') }
      it { expect(expectation('foo', 'Not:Uses:*').translate).to eq('<strong>foo</strong> no debe delegar') }
      it { expect(expectation('foo', 'Not:Uses').translate).to eq('<strong>foo</strong> no debe delegar') }
      it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<strong>foo</strong> no debe utilizar <strong>baz</strong>') }
      it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<strong>foo</strong> no debe emplear expresiones lambda') }

      it { expect(expectation('*', 'DeclaresClass').translate).to eq('la solución debe declarar clases') }
      it { expect(expectation('*', 'Not:DeclaresMethod').translate).to eq('la solución no debe declarar métodos') }
      it { expect(expectation('*', 'Not:DeclaresClass').translate).to eq('la solución no debe declarar clases') }

      it { expect(expectation('foo', 'DeclaresObject').translate).to eq('<strong>foo</strong> debe declarar objetos') }
      it { expect(expectation('*', 'Not:DeclaresClass').translate).to eq('la solución no debe declarar clases') }
      it { expect(expectation('foo', 'HasAnonymousVariable').translate).to eq('<strong>foo</strong> debe utilizar una variable anónima') }
      it { expect(expectation('foo', 'UsesAnonymousVariable').translate).to eq('<strong>foo</strong> debe utilizar una variable anónima') }
      it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('la solución debe usar polimorfismo') }

      it { expect(expectation('foo', 'UsesIf').translate(keyword_if: 'si')).to eq('<strong>foo</strong> debe usar <i>si</i>') }
      it { expect(expectation('foo', 'UsesIf').translate(keyword_repeat: 'repetir')).to eq('<strong>foo</strong> debe usar <i>if</i>') }
    end

    describe 'smells' do
      it { expect(expectation('foo', 'HasRedundantIf').translate).to eq('<strong>foo</strong> tiene ifs innecesarios') }
      it { expect(expectation('foo', 'HasRedundantBooleanComparison').translate).to eq('<strong>foo</strong> hace comparaciones booleanas innecesarias') }
      it { expect(expectation('bar', 'UsesFail').translate).to eq('<strong>bar</strong> usa <i>fail</li>, lo cual es una mala práctica') }
      it { expect(expectation('foo', 'HasEmptyIfBranches').translate).to eq('<strong>foo</strong> tiene ramas de <i>if</i> vacías') }
      it { expect(expectation('foo', 'HasUnreachableCode').translate).to eq('<strong>foo</strong> tiene código inalcanzable') }
      it { expect(expectation('foo', 'HasLongParameterList').translate).to eq('<strong>foo</strong> tiene demasiados parámetros. Te podría estar faltando una abstracción') }
      it { expect(expectation('foo', 'OverridesEqualOrHashButNotBoth').translate).to eq('<strong>foo</strong> redefine los métodos <i>equals</i> o <i>hash</i>, pero no ambos') }
    end
  end
end

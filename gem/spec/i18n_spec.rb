require 'spec_helper'

describe Mulang::Expectation::I18n do

  def expectation(binding, inspection)
    Mulang::Expectation.parse(binding: binding, inspection: inspection)
  end

  context 'en locale' do
    before { I18n.locale = :en }

    it { expect(expectation('*', 'Declares:foo').translate).to eq('solution must declare <strong>foo</strong>') }
    it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<strong>foo</strong> must not use <strong>baz</strong>') }
    it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<strong>foo</strong> must not use lambda expressions') }
    it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('solution must use polymorphism') }

    it { expect(expectation('*', "Calls:x:WithString:'foo'").translate).to eq("solution must use <strong>x</strong> with string <i>'foo'</i>") }
    it { expect(expectation('*', "Returns:WithNumber:3").translate).to eq('solution must return with number <i>3</i>') }
    it { expect(expectation('*', "Calls:g:WithNumber:1").translate).to eq('solution must use <strong>g</strong> with number <i>1</i>') }
    it { expect(expectation('*', "Calls:x:WithTrue").translate).to eq('solution must use <strong>x</strong> with value <i>true</i>') }
    it { expect(expectation('*', "Assigns:WithTrue").translate).to eq('solution must perform assignments with value <i>true</i>') }
    it { expect(expectation('*', "DeclaresAttribute:WithTrue").translate).to eq('solution must declare attributes with value <i>true</i>') }
    it { expect(expectation('*', "Returns:WithFalse").translate).to eq('solution must return with value <i>false</i>') }
    it { expect(expectation('*', "UsesRepeat:WithMath").translate).to eq('solution must use <i>repeat</i> with a math expression') }
    it { expect(expectation('*', "Calls:g:WithLiteral").translate).to eq('solution must use <strong>g</strong> with a literal value') }
    it { expect(expectation('*', "Calls:g:WithNonliteral").translate).to eq('solution must use <strong>g</strong> with a non-literal expresson') }
    it { expect(expectation('*', "Calls:g:WithLogic").translate).to eq('solution must use <strong>g</strong> with a boolean expression') }
    it { expect(expectation('*', "DeclaresVariable:x:WithNumber:4").translate).to eq('solution must declare a variable <strong>x</strong> with number <i>4</i>') }
    it { expect(expectation('*', "Assigns:x:WithSymbol:bar").translate).to eq('solution must assign <strong>x</strong> with symbol <i>bar</i>') }
    it { expect(expectation('*', "Assigns:x:WithChar:'a'").translate).to eq("solution must assign <strong>x</strong> with character <i>'a'</i>") }
    it { expect(expectation('*', "Assigns:*:WithString:\"hello\"").translate).to eq("solution must perform assignments with string <i>\"hello\"</i>") }
    it { expect(expectation('*', "Returns:WithNumber:9").translate).to eq('solution must return with number <i>9</i>') }

    it { expect(expectation('foo', 'HasEmptyRepeat').translate).to eq('<strong>foo</strong> has empty <i>repeat</i>') }

    it { expect(expectation('Foo', 'HasDeclarationTypos:foo').translate).to eq('Solution must declare <strong>foo</strong>, but declares <strong>Foo</strong>. Perhaps you meant <strong>foo</strong>?') }
    it { expect(expectation('Foo', 'HasUsageTypos:foo').translate).to eq('Solution must use <strong>foo</strong>, but it uses <strong>Foo</strong>. Perhaps you meant <strong>foo</strong>?') }

    it { expect(expectation('*', 'UsesEqual').translate).to eq('solution must use <i>==</i>') }
    it { expect(expectation('*', 'UsesNotEqual').translate).to eq('solution must use <i>!=</i>') }
    it { expect(expectation('*', 'UsesNegation').translate).to eq('solution must use <i>!</i>') }
    it { expect(expectation('*', 'UsesAnd').translate).to eq('solution must use <i>&amp;&amp;</i>') }
    it { expect(expectation('*', 'UsesOr').translate).to eq('solution must use <i>||</i>') }
    it { expect(expectation('*', 'UsesHash').translate).to eq('solution must use <i>hash</i>') }
    it { expect(expectation('*', 'UsesGreatherOrEqualThan').translate).to eq('solution must use <i>&gt;=</i>') }
    it { expect(expectation('*', 'UsesGreatherThan').translate).to eq('solution must use <i>&gt;</i>') }
    it { expect(expectation('*', 'UsesLessOrEqualThan').translate).to eq('solution must use <i>&lt;=</i>') }
    it { expect(expectation('*', 'UsesLessThan').translate).to eq('solution must use <i>&lt;</i>') }
    it { expect(expectation('*', 'UsesOtherwise').translate).to eq('solution must use <i>otherwise</i>') }
    it { expect(expectation('*', 'UsesPlus').translate).to eq('solution must use <i>+</i>') }
    it { expect(expectation('*', 'UsesMinus').translate).to eq('solution must use <i>-</i>') }
    it { expect(expectation('*', 'UsesMultiply').translate).to eq('solution must use <i>*</i>') }
    it { expect(expectation('*', 'UsesDivide').translate).to eq('solution must use <i>/</i>') }
    it { expect(expectation('*', 'UsesForwardComposition').translate).to eq('solution must use <i>&gt;&gt;</i>') }
    it { expect(expectation('*', 'UsesBackwardComposition').translate).to eq('solution must use <i>.</i>') }
    it { expect(expectation('*', 'UsesModulo').translate).to eq('solution must use <i>%</i>') }
    it { expect(expectation('*', 'UsesBitwiseOr').translate).to eq('solution must use <i>|</i>') }
    it { expect(expectation('*', 'UsesBitwiseAnd').translate).to eq('solution must use <i>&amp;</i>') }
    it { expect(expectation('*', 'UsesBitwiseXor').translate).to eq('solution must use <i>^</i>') }
    it { expect(expectation('*', 'UsesBitwiseLeftShift').translate).to eq('solution must use <i>&lt;&lt;</i>') }
    it { expect(expectation('*', 'UsesBitwiseRightShift').translate).to eq('solution must use <i>&gt;&gt;</i>') }
  end

  context 'pt locale' do
    before { I18n.locale = :pt }

    it { expect(expectation('*', 'Declares:foo').translate).to eq('a solução deve declarar <strong>foo</strong>') }
    it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<strong>foo</strong> não deve usar <strong>baz</strong>') }
    it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<strong>foo</strong> não deve usar expressões lambda') }
    it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('a solução deve usar polimorfismo') }

    it { expect(expectation('*', "Returns:WithFalse").translate).to eq('a solução deve retornar com o valor <i>false</i>') }
    it { expect(expectation('*', "UsesRepeat:WithMath").translate).to eq('a solução deve usar <i>repeat</i> com uma expressão matemática') }
    it { expect(expectation('*', "Calls:g:WithLiteral").translate).to eq('a solução deve usar <strong>g</strong> com um valor literal') }

    it { expect(expectation('foo', 'HasEmptyRepeat').translate).to eq('<strong>foo</strong> tem um <i>repeat</i> vazio') }
  end

  context 'es locale' do
    before { I18n.locale = :es }

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

      it { expect(expectation('foo', 'HasIf').translate(keyword_If: 'si')).to eq('<strong>foo</strong> debe usar <i>si</i>') }
      it { expect(expectation('foo', 'HasIf').translate(keyword_Repeat: 'repetir')).to eq('<strong>foo</strong> debe usar <i>if</i>') }
    end

    describe 'custom expectations' do
      it { expect(expectation('<<custom>>', 'La solución debe declarar `foo`').translate).to eq('La solución debe declarar `foo`') }
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
      it { expect(expectation('foo', 'Delegates').translate).to eq('<strong>foo</strong> debe delegar') }
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

      it { expect(expectation('foo', 'UsesIf').translate(keyword_If: 'si')).to eq('<strong>foo</strong> debe usar <i>si</i>') }
      it { expect(expectation('foo', 'UsesIf').translate(keyword_Repeat: 'repetir')).to eq('<strong>foo</strong> debe usar <i>if</i>') }

      it { expect(expectation('*', "Calls:x:WithString:'foo'").translate).to eq("la solución debe utilizar <strong>x</strong> con la cadena <i>'foo'</i>") }
      it { expect(expectation('*', "Returns:WithNumber:3").translate).to eq('la solución debe retornar con el número <i>3</i>') }
      it { expect(expectation('*', "Calls:g:WithNumber:1").translate).to eq('la solución debe utilizar <strong>g</strong> con el número <i>1</i>') }
      it { expect(expectation('*', "Calls:x:WithTrue").translate).to eq('la solución debe utilizar <strong>x</strong> con el valor <i>true</i>') }
      it { expect(expectation('*', "Assigns:WithTrue").translate).to eq('la solución debe realizar asignaciones con el valor <i>true</i>') }
      it { expect(expectation('*', "DeclaresAttribute:WithTrue").translate).to eq('la solución debe declarar atributos con el valor <i>true</i>') }
      it { expect(expectation('*', "Returns:WithFalse").translate).to eq('la solución debe retornar con el valor <i>false</i>') }
      it { expect(expectation('*', "UsesRepeat:WithMath").translate).to eq('la solución debe usar <i>repeat</i>con una expresión matemática') }
      it { expect(expectation('*', "Calls:g:WithLiteral").translate).to eq('la solución debe utilizar <strong>g</strong> con un valor literal') }
      it { expect(expectation('*', "Calls:g:WithNonliteral").translate).to eq("la solución debe utilizar <strong>g</strong> con una expresión no literal") }
      it { expect(expectation('*', "Calls:g:WithLogic").translate).to eq('la solución debe utilizar <strong>g</strong> con una expresión booleana') }
      it { expect(expectation('*', "DeclaresVariable:x:WithNumber:4").translate).to eq('la solución debe declarar una variable <strong>x</strong> con el número <i>4</i>') }
      it { expect(expectation('*', "Assigns:x:WithSymbol:bar").translate).to eq('la solución debe asignar <strong>x</strong> con el símbolo <i>bar</i>') }
      it { expect(expectation('*', "Assigns:x:WithChar:'a'").translate).to eq("la solución debe asignar <strong>x</strong> con el carácter <i>'a'</i>") }
      it { expect(expectation('*', "Assigns:*:WithString:\"hello\"").translate).to eq("la solución debe realizar asignaciones con la cadena <i>\"hello\"</i>") }
      it { expect(expectation('*', "Returns:WithNumber:9").translate).to eq('la solución debe retornar con el número <i>9</i>') }
    end

    describe 'smells' do
      it { expect(expectation('foo', 'HasRedundantIf').translate).to eq('<strong>foo</strong> tiene ifs innecesarios') }
      it { expect(expectation('foo', 'HasRedundantBooleanComparison').translate).to eq('<strong>foo</strong> hace comparaciones booleanas innecesarias') }
      it { expect(expectation('bar', 'UsesFail').translate).to eq('<strong>bar</strong> usa <i>fail</li>, lo cual es una mala práctica') }
      it { expect(expectation('foo', 'HasEmptyIfBranches').translate).to eq('<strong>foo</strong> tiene ramas de <i>if</i> vacías') }
      it { expect(expectation('foo', 'HasEmptyRepeat').translate).to eq('<strong>foo</strong> tiene un <i>repeat</i> vacío') }
      it { expect(expectation('foo', 'ShouldInvertIfCondition').translate).to eq('<strong>foo</strong> debería invertir la condición del <i>if</i> e intercambiar las ramas') }
      it { expect(expectation('foo', 'HasRedundantRepeat').translate).to eq('<strong>foo</strong> tiene un <i>repeat</i> innecesario') }
      it { expect(expectation('foo', 'HasUnreachableCode').translate).to eq('<strong>foo</strong> tiene código inalcanzable') }
      it { expect(expectation('foo', 'HasLongParameterList').translate).to eq('<strong>foo</strong> tiene demasiados parámetros. Te podría estar faltando una abstracción') }
      it { expect(expectation('foo', 'OverridesEqualOrHashButNotBoth').translate).to eq('<strong>foo</strong> redefine los métodos <i>equals</i> o <i>hash</i>, pero no ambos') }

      it { expect(expectation('Foo', 'HasDeclarationTypos:foo').translate).to eq('La solución parece tener un error de tipeo: debe declarar <strong>foo</strong>, pero declara <strong>Foo</strong>. ¿Quizás quisiste decir <strong>foo</strong>?') }
      it { expect(expectation('Foo', 'HasUsageTypos:foo').translate).to eq('La solución parece tener un error de tipeo: debe usar <strong>foo</strong>, pero usa <strong>Foo</strong>. ¿Quizás quisiste decir <strong>foo</strong>?') }
    end
  end
end

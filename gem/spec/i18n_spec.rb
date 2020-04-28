require 'spec_helper'

describe Mulang::Expectation::I18n do

  def expectation(binding, inspection)
    Mulang::Expectation.parse(binding: binding, inspection: inspection)
  end

  context 'en locale' do
    before { I18n.locale = :en }

    it { expect(expectation('*', 'Declares:foo').translate).to eq('solution must declare <code>foo</code>') }
    it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<code>foo</code> must not use <code>baz</code>') }
    it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<code>foo</code> must not use lambda expressions') }
    it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('solution must use polymorphism') }

    it { expect(expectation('*', "Calls:x:WithString:'foo'").translate).to eq("solution must use <code>x</code> with string <code>'foo'</code>") }
    it { expect(expectation('*', "Returns:WithNumber:3").translate).to eq('solution must return with number <code>3</code>') }
    it { expect(expectation('*', "Calls:g:WithNumber:1").translate).to eq('solution must use <code>g</code> with number <code>1</code>') }
    it { expect(expectation('*', "Calls:x:WithTrue").translate).to eq('solution must use <code>x</code> with value <code>true</code>') }
    it { expect(expectation('*', "Assigns:WithTrue").translate).to eq('solution must perform assignments with value <code>true</code>') }
    it { expect(expectation('*', "DeclaresAttribute:WithTrue").translate).to eq('solution must declare attributes with value <code>true</code>') }
    it { expect(expectation('*', "Returns:WithFalse").translate).to eq('solution must return with value <code>false</code>') }
    it { expect(expectation('*', "UsesRepeat:WithMath").translate).to eq('solution must use <code>repeat</code> with a math expression') }
    it { expect(expectation('*', "Calls:g:WithLiteral").translate).to eq('solution must use <code>g</code> with a literal value') }
    it { expect(expectation('*', "Calls:g:WithNonliteral").translate).to eq('solution must use <code>g</code> with a non-literal expresson') }
    it { expect(expectation('*', "Calls:g:WithLogic").translate).to eq('solution must use <code>g</code> with a boolean expression') }
    it { expect(expectation('*', "DeclaresVariable:x:WithNumber:4").translate).to eq('solution must declare a variable <code>x</code> with number <code>4</code>') }
    it { expect(expectation('*', "Assigns:x:WithSymbol:bar").translate).to eq('solution must assign <code>x</code> with symbol <code>bar</code>') }
    it { expect(expectation('*', "Assigns:x:WithChar:'a'").translate).to eq("solution must assign <code>x</code> with character <code>'a'</code>") }
    it { expect(expectation('*', "Assigns:*:WithString:\"hello\"").translate).to eq("solution must perform assignments with string <code>\"hello\"</code>") }
    it { expect(expectation('*', "Returns:WithNumber:9").translate).to eq('solution must return with number <code>9</code>') }

    it { expect(expectation('foo', 'HasEmptyRepeat').translate).to eq('<code>foo</code> has empty <code>repeat</code>') }

    it { expect(expectation('Foo', 'HasDeclarationTypos:foo').translate).to eq('Solution must declare <code>foo</code>, but declares <code>Foo</code>. Perhaps you meant <code>foo</code>?') }
    it { expect(expectation('Foo', 'HasUsageTypos:foo').translate).to eq('Solution must use <code>foo</code>, but it uses <code>Foo</code>. Perhaps you meant <code>foo</code>?') }

    it { expect(expectation('*', 'UsesEqual').translate).to eq('solution must use <code>==</code>') }
    it { expect(expectation('*', 'UsesNotEqual').translate).to eq('solution must use <code>!=</code>') }
    it { expect(expectation('*', 'UsesNegation').translate).to eq('solution must use <code>!</code>') }
    it { expect(expectation('*', 'UsesAnd').translate).to eq('solution must use <code>&amp;&amp;</code>') }
    it { expect(expectation('*', 'UsesOr').translate).to eq('solution must use <code>||</code>') }
    it { expect(expectation('*', 'UsesHash').translate).to eq('solution must use <code>hash</code>') }
    it { expect(expectation('*', 'UsesGreatherOrEqualThan').translate).to eq('solution must use <code>&gt;=</code>') }
    it { expect(expectation('*', 'UsesGreatherThan').translate).to eq('solution must use <code>&gt;</code>') }
    it { expect(expectation('*', 'UsesLessOrEqualThan').translate).to eq('solution must use <code>&lt;=</code>') }
    it { expect(expectation('*', 'UsesLessThan').translate).to eq('solution must use <code>&lt;</code>') }
    it { expect(expectation('*', 'UsesOtherwise').translate :Haskell).to eq('solution must use <code>otherwise</code>') }
    it { expect(expectation('*', 'UsesPlus').translate).to eq('solution must use <code>+</code>') }
    it { expect(expectation('*', 'UsesMinus').translate).to eq('solution must use <code>-</code>') }
    it { expect(expectation('*', 'UsesMultiply').translate).to eq('solution must use <code>*</code>') }
    it { expect(expectation('*', 'UsesDivide').translate).to eq('solution must use <code>/</code>') }
    it { expect(expectation('*', 'UsesForwardComposition').translate).to eq('solution must use <code>&gt;&gt;</code>') }
    it { expect(expectation('*', 'UsesBackwardComposition').translate).to eq('solution must use <code>.</code>') }
    it { expect(expectation('*', 'UsesModulo').translate).to eq('solution must use <code>%</code>') }
    it { expect(expectation('*', 'UsesBitwiseOr').translate).to eq('solution must use <code>|</code>') }
    it { expect(expectation('*', 'UsesBitwiseAnd').translate).to eq('solution must use <code>&amp;</code>') }
    it { expect(expectation('*', 'UsesBitwiseXor').translate).to eq('solution must use <code>^</code>') }
    it { expect(expectation('*', 'UsesBitwiseLeftShift').translate).to eq('solution must use <code>&lt;&lt;</code>') }
    it { expect(expectation('*', 'UsesBitwiseRightShift').translate).to eq('solution must use <code>&gt;&gt;</code>') }
  end

  context 'pt locale' do
    before { I18n.locale = :pt }

    it { expect(expectation('*', 'Declares:foo').translate).to eq('a solução deve declarar <code>foo</code>') }
    it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<code>foo</code> não deve usar <code>baz</code>') }
    it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<code>foo</code> não deve usar expressões lambda') }
    it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('a solução deve usar polimorfismo') }

    it { expect(expectation('*', "Returns:WithFalse").translate).to eq('a solução deve retornar com o valor <code>false</code>') }
    it { expect(expectation('*', "UsesRepeat:WithMath").translate).to eq('a solução deve usar <code>repeat</code> com uma expressão matemática') }
    it { expect(expectation('*', "Calls:g:WithLiteral").translate).to eq('a solução deve usar <code>g</code> com um valor literal') }

    it { expect(expectation('foo', 'HasEmptyRepeat').translate).to eq('<code>foo</code> tem um <code>repeat</code> vazio') }
  end

  context 'es locale' do
    before { I18n.locale = :es }

    describe 'v0 exectations' do
      it { expect(expectation('foo', 'HasBinding').translate).to eq('la solución debe declarar <code>foo</code>') }
      it { expect(expectation('foo', 'HasUsage:bar').translate).to eq('<code>foo</code> debe utilizar <code>bar</code>') }
      it { expect(expectation('foo', 'HasWhile').translate).to eq('<code>foo</code> debe utilizar un <code>while</code>') }
      it { expect(expectation('foo', 'HasTypeDeclaration').translate).to eq('la solución debe declarar un sinónimo de tipo <code>foo</code>') }
      it { expect(expectation('foo', 'HasTypeSignature').translate).to eq('la solución debe declarar una firma <code>foo</code>') }
      it { expect(expectation('foo', 'HasRepeat').translate).to eq('<code>foo</code> debe usar <code>repeat</code>') }
      it { expect(expectation('foo', 'HasNot').translate :Prolog).to eq('<code>foo</code> debe utilizar <code>not</code>') }
      it { expect(expectation('foo', 'HasLambda').translate).to eq('<code>foo</code> debe emplear expresiones lambda') }
      it { expect(expectation('foo', 'HasIf').translate).to eq('<code>foo</code> debe usar <code>if</code>') }
      it { expect(expectation('foo', 'HasForall').translate :Prolog).to eq('<code>foo</code> debe utilizar <code>forall</code>') }
      it { expect(expectation('foo', 'HasFindall').translate :Prolog).to eq('<code>foo</code> debe utilizar <code>findall</code>') }
      it { expect(expectation('foo', 'HasComprehension').translate).to eq('<code>foo</code> debe emplear listas por comprensión') }
      it { expect(expectation('foo', 'HasDirectRecursion').translate).to eq('<code>foo</code> debe estar declarado recursivamente') }
      it { expect(expectation('foo', 'HasComposition').translate).to eq('<code>foo</code> debe usar composición') }
      it { expect(expectation('foo', 'UsesLoop').translate).to eq('<code>foo</code> debe usar un bucle') }

      it { expect(expectation('foo', 'HasIf').translate(keyword_If: 'si')).to eq('<code>foo</code> debe usar <code>si</code>') }
      it { expect(expectation('foo', 'HasIf').translate(keyword_Repeat: 'repetir')).to eq('<code>foo</code> debe usar <code>if</code>') }
    end

    describe 'custom expectations' do
      it { expect(expectation('<<custom>>', 'La solución debe declarar `foo`').translate).to eq('La solución debe declarar `foo`') }
    end

    describe 'v2 expectations' do
      it { expect(expectation('*', 'Declares:foo').translate).to eq('la solución debe declarar <code>foo</code>') }
      it { expect(expectation('*', 'DeclaresClass:foo').translate).to eq('la solución debe declarar una clase <code>foo</code>') }

      it { expect(expectation('Mulang', 'DeclaresClass:Inspection').translate).to eq('<code>Mulang</code> debe declarar una clase <code>Inspection</code>') }

      it { expect(expectation('*', 'DeclaresObject:foo').translate).to eq('la solución debe declarar un objeto <code>foo</code>') }
      it { expect(expectation('foo', 'DeclaresMethod:bar').translate).to eq('<code>foo</code> debe declarar un método <code>bar</code>') }
      it { expect(expectation('foo', 'Declares').translate).to eq('<code>foo</code> debe contener declaraciones') }

      it { expect(expectation('foo.bar', 'DeclaresMethod').translate).to eq('<code>foo.bar</code> debe declarar métodos') }
      it { expect(expectation('foo.bar', 'UsesIf').translate).to eq('<code>foo.bar</code> debe usar <code>if</code>') }
      it { expect(expectation('foo', 'Returns').translate).to eq('<code>foo</code> debe retornar') }

      it { expect(expectation('Intransitive:foo', 'Not:UsesLambda').translate).to eq('<code>foo</code> no debe emplear expresiones lambda') }

      it { expect(expectation('foo', 'Uses:=baz').translate).to eq('<code>foo</code> debe utilizar <code>baz</code>') }
      it { expect(expectation('foo', 'Uses:~baz').translate).to eq('<code>foo</code> debe delegar en algo parecido a <code>baz</code>') }
      it { expect(expectation('foo', 'DeclaresMethod:~baz').translate).to eq('<code>foo</code> debe declarar un método parecido a <code>baz</code>') }
      it { expect(expectation('foo', 'Uses:*').translate).to eq('<code>foo</code> debe delegar') }
      it { expect(expectation('foo', 'DeclaresMethod:*').translate).to eq('<code>foo</code> debe declarar métodos') }
      it { expect(expectation('foo', 'Uses:baz').translate).to eq('<code>foo</code> debe utilizar <code>baz</code>') }
      it { expect(expectation('foo', 'Uses').translate).to eq('<code>foo</code> debe delegar') }
      it { expect(expectation('foo', 'Delegates').translate).to eq('<code>foo</code> debe delegar') }
      it { expect(expectation('foo', 'UsesForall').translate :Prolog).to eq('<code>foo</code> debe utilizar <code>forall</code>') }

      it { expect(expectation('foo', 'Not:Uses:=baz').translate).to eq('<code>foo</code> no debe utilizar <code>baz</code>') }
      it { expect(expectation('foo', 'Not:Uses:~baz').translate).to eq('<code>foo</code> no debe delegar en algo parecido a <code>baz</code>') }
      it { expect(expectation('foo', 'Not:Uses:*').translate).to eq('<code>foo</code> no debe delegar') }
      it { expect(expectation('foo', 'Not:Uses').translate).to eq('<code>foo</code> no debe delegar') }
      it { expect(expectation('foo', 'Not:Uses:baz').translate).to eq('<code>foo</code> no debe utilizar <code>baz</code>') }
      it { expect(expectation('foo', 'Not:UsesLambda').translate).to eq('<code>foo</code> no debe emplear expresiones lambda') }

      it { expect(expectation('*', 'DeclaresClass').translate).to eq('la solución debe declarar clases') }
      it { expect(expectation('*', 'Not:DeclaresMethod').translate).to eq('la solución no debe declarar métodos') }
      it { expect(expectation('*', 'Not:DeclaresClass').translate).to eq('la solución no debe declarar clases') }

      it { expect(expectation('foo', 'DeclaresObject').translate).to eq('<code>foo</code> debe declarar objetos') }
      it { expect(expectation('*', 'Not:DeclaresClass').translate).to eq('la solución no debe declarar clases') }
      it { expect(expectation('foo', 'HasAnonymousVariable').translate).to eq('<code>foo</code> debe utilizar una variable anónima') }
      it { expect(expectation('foo', 'UsesAnonymousVariable').translate).to eq('<code>foo</code> debe utilizar una variable anónima') }
      it { expect(expectation('*', 'UsesStaticPolymorphism').translate).to eq('la solución debe usar polimorfismo') }

      it { expect(expectation('foo', 'UsesIf').translate(keyword_If: 'si')).to eq('<code>foo</code> debe usar <code>si</code>') }
      it { expect(expectation('foo', 'UsesIf').translate(keyword_Repeat: 'repetir')).to eq('<code>foo</code> debe usar <code>if</code>') }

      it { expect(expectation('*', "Calls:x:WithString:'foo'").translate).to eq("la solución debe utilizar <code>x</code> con la cadena <code>'foo'</code>") }
      it { expect(expectation('*', "Returns:WithNumber:3").translate).to eq('la solución debe retornar con el número <code>3</code>') }
      it { expect(expectation('*', "Calls:g:WithNumber:1").translate).to eq('la solución debe utilizar <code>g</code> con el número <code>1</code>') }
      it { expect(expectation('*', "Calls:x:WithTrue").translate).to eq('la solución debe utilizar <code>x</code> con el valor <code>true</code>') }
      it { expect(expectation('*', "Assigns:WithTrue").translate).to eq('la solución debe realizar asignaciones con el valor <code>true</code>') }
      it { expect(expectation('*', "DeclaresAttribute:WithTrue").translate).to eq('la solución debe declarar atributos con el valor <code>true</code>') }
      it { expect(expectation('*', "Returns:WithFalse").translate).to eq('la solución debe retornar con el valor <code>false</code>') }
      it { expect(expectation('*', "UsesRepeat:WithMath").translate).to eq('la solución debe usar <code>repeat</code>con una expresión matemática') }
      it { expect(expectation('*', "Calls:g:WithLiteral").translate).to eq('la solución debe utilizar <code>g</code> con un valor literal') }
      it { expect(expectation('*', "Calls:g:WithNonliteral").translate).to eq("la solución debe utilizar <code>g</code> con una expresión no literal") }
      it { expect(expectation('*', "Calls:g:WithLogic").translate).to eq('la solución debe utilizar <code>g</code> con una expresión booleana') }
      it { expect(expectation('*', "DeclaresVariable:x:WithNumber:4").translate).to eq('la solución debe declarar una variable <code>x</code> con el número <code>4</code>') }
      it { expect(expectation('*', "Assigns:x:WithSymbol:bar").translate).to eq('la solución debe asignar <code>x</code> con el símbolo <code>bar</code>') }
      it { expect(expectation('*', "Assigns:x:WithChar:'a'").translate).to eq("la solución debe asignar <code>x</code> con el carácter <code>'a'</code>") }
      it { expect(expectation('*', "Assigns:*:WithString:\"hello\"").translate).to eq("la solución debe realizar asignaciones con la cadena <code>\"hello\"</code>") }
      it { expect(expectation('*', "Returns:WithNumber:9").translate).to eq('la solución debe retornar con el número <code>9</code>') }
    end

    describe 'smells' do
      it { expect(expectation('foo', 'HasRedundantIf').translate).to eq('<code>foo</code> tiene ifs innecesarios') }
      it { expect(expectation('foo', 'HasRedundantBooleanComparison').translate).to eq('<code>foo</code> hace comparaciones booleanas innecesarias') }
      it { expect(expectation('bar', 'UsesFail').translate :Prolog).to eq('<code>bar</code> usa <code>fail</li>, lo cual es una mala práctica') }
      it { expect(expectation('foo', 'HasEmptyIfBranches').translate).to eq('<code>foo</code> tiene ramas de <code>if</code> vacías') }
      it { expect(expectation('foo', 'HasEmptyRepeat').translate).to eq('<code>foo</code> tiene un <code>repeat</code> vacío') }
      it { expect(expectation('foo', 'ShouldInvertIfCondition').translate).to eq('<code>foo</code> debería invertir la condición del <code>if</code> e intercambiar las ramas') }
      it { expect(expectation('foo', 'HasRedundantRepeat').translate).to eq('<code>foo</code> tiene un <code>repeat</code> innecesario') }
      it { expect(expectation('foo', 'HasUnreachableCode').translate).to eq('<code>foo</code> tiene código inalcanzable') }
      it { expect(expectation('foo', 'HasLongParameterList').translate).to eq('<code>foo</code> tiene demasiados parámetros. Te podría estar faltando una abstracción') }
      it { expect(expectation('foo', 'OverridesEqualOrHashButNotBoth').translate).to eq('<code>foo</code> redefine los métodos <code>equals</code> o <code>hash</code>, pero no ambos') }

      it { expect(expectation('Foo', 'HasDeclarationTypos:foo').translate).to eq('La solución parece tener un error de tipeo: debe declarar <code>foo</code>, pero declara <code>Foo</code>. ¿Quizás quisiste decir <code>foo</code>?') }
      it { expect(expectation('Foo', 'HasUsageTypos:foo').translate).to eq('La solución parece tener un error de tipeo: debe usar <code>foo</code>, pero usa <code>Foo</code>. ¿Quizás quisiste decir <code>foo</code>?') }

      it { expect(expectation('Foo', 'HasAssignmentReturn').translate).to eq('<code>Foo</code> devuelve el resultado de una asignación. ¿Quizás quisiste usar el operador <code>==</code>?') }
      it { expect(expectation('Foo', 'HasAssignmentCondition').translate).to eq('<code>Foo</code> evalúa el resultado de una asignación en donde se esperaba una expresión booleana. ¿Quizás quisiste usar el operador <code>==</code>?') }
    end
  end
end

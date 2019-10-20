var should = require('should');
let mulang = require('../build/mulang');

(() => {

  const DEFAULT_KEYWORDS = {
    'fail': 'fail',
    'false': 'false',
    'findall': 'findall',
    'for': 'for',
    'forall': 'forall',
    'foreach': 'foreach',
    'if': 'if',
    'is': 'is',
    'not': 'not',
    'null': 'null',
    'repeat': 'repeat',
    'switch': 'switch',
    'true': 'true',
    'while': 'while',
    'yield': 'yield'
  }


  const LOCALES = {
    es: {
      must_not: 'no debe',
      must: 'debe',
      solution: 'la solución',
      Assigns_named: (binding, must, target, keyword) => `${binding} ${must} asignar ${target}`,
      Assigns: (binding, must, target, keyword) => `${binding} ${must} realizar asignaciones`,
      Calls_named: (binding, must, target, keyword) => `${binding} ${must} utilizar ${target}`,
      Calls: (binding, must, target, keyword) => `${binding} ${must} delegar`,
      Declares_named: (binding, must, target, keyword) => `${binding} ${must} declarar ${target}`,
      Declares: (binding, must, target, keyword) => `${binding} ${must} contener declaraciones`,
      DeclaresAttribute_named: (binding, must, target, keyword) => `${binding} ${must} declarar un atributo ${target}`,
      DeclaresAttribute: (binding, must, target, keyword) => `${binding} ${must} declarar atributos`,
      DeclaresClass_named: (binding, must, target, keyword) => `${binding} ${must} declarar una clase ${target}`,
      DeclaresClass: (binding, must, target, keyword) => `${binding} ${must} declarar clases`,
      DeclaresComputation_named: (binding, must, target, keyword) => `${binding} ${must} declarar una computación ${target}`,
      DeclaresComputation: (binding, must, target, keyword) => `${binding} ${must} declarar computaciones`,
      DeclaresComputationWithArity0_named: (binding, must, target, keyword) => `${target} ${must} declarar cero parametros`,
      DeclaresComputationWithArity1_named: (binding, must, target, keyword) => `${target} ${must} tener un parámetro`,
      DeclaresComputationWithArity2_named: (binding, must, target, keyword) => `${target} ${must} tener dos parámetros`,
      DeclaresComputationWithArity3_named: (binding, must, target, keyword) => `${target} ${must} tener tres parámetros`,
      DeclaresComputationWithArity4_named: (binding, must, target, keyword) => `${target} ${must} tener cuatro parámetros`,
      DeclaresComputationWithArity5_named: (binding, must, target, keyword) => `${target} ${must} tener cinco parámetros`,
      DeclaresEntryPoint: (binding, must, target, keyword) => `${binding} ${must} declarar un punto de entrada`,
      DeclaresEnumeration_named: (binding, must, target, keyword) => `${binding} ${must} declarar una enumeracion ${target}`,
      DeclaresEnumeration: (binding, must, target, keyword) => `${binding} ${must} declarar enumeraciones`,
      DeclaresFact_named: (binding, must, target, keyword) => `${binding} ${must} declarar un hecho ${target}`,
      DeclaresFact: (binding, must, target, keyword) => `${binding} ${must} declarar hechos`,
      DeclaresFunction_named: (binding, must, target, keyword) => `${binding} ${must} declarar una función ${target}`,
      DeclaresFunction: (binding, must, target, keyword) => `${binding} ${must} debe declarar funciones`,
      DeclaresInterface_named: (binding, must, target, keyword) => `${binding} ${must} declarar una interfaz ${target}`,
      DeclaresInterface: (binding, must, target, keyword) => `${binding} ${must} declarar interfaces`,
      DeclaresMethod_named: (binding, must, target, keyword) => `${binding} ${must} declarar un método ${target}`,
      DeclaresMethod: (binding, must, target, keyword) => `${binding} ${must} declarar métodos`,
      DeclaresObject_named: (binding, must, target, keyword) => `${binding} ${must} declarar un objeto ${target}`,
      DeclaresObject: (binding, must, target, keyword) => `${binding} ${must} declarar objetos`,
      DeclaresPredicate_named: (binding, must, target, keyword) => `${binding} ${must} declarar un predicados ${target}`,
      DeclaresPredicate: (binding, must, target, keyword) => `${binding} ${must} declarar predicados`,
      DeclaresProcedure_named: (binding, must, target, keyword) => `${binding} ${must} declarar un procedimiento ${target}`,
      DeclaresProcedure: (binding, must, target, keyword) => `${binding} ${must} declarar procedimientos`,
      DeclaresRecursively_named: (binding, must, target, keyword) => `${target} ${must} estar declarado recursivamente`,
      DeclaresRule_named: (binding, must, target, keyword) => `${binding} ${must} declarar una regla ${target}`,
      DeclaresRule: (binding, must, target, keyword) => `${binding} ${must} debe declarar reglas`,
      DeclaresSuperclass_named: (binding, must, target, keyword) => `${binding} ${must} declarar una superclase ${target}`,
      DeclaresSuperclass: (binding, must, target, keyword) => `${binding} ${must} declarar una superclase`,
      DeclaresTypeAlias_named: (binding, must, target, keyword) => `${binding} ${must} declarar un sinónimo de tipo ${target}`,
      DeclaresTypeAlias: (binding, must, target, keyword) => `${binding} ${must} declarar sinónimos de tipo`,
      DeclaresTypeSignature_named: (binding, must, target, keyword) => `${binding} ${must} declarar una firma ${target}`,
      DeclaresTypeSignature: (binding, must, target, keyword) => `${binding} ${must} declarar firmas de tipos`,
      DeclaresVariable_named: (binding, must, target, keyword) => `${binding} ${must} declarar una variable ${target}`,
      DeclaresVariable: (binding, must, target, keyword) => `${binding} ${must} debe declarar variables`,
      DiscardsExceptions: (binding, must, target, keyword) => `${binding} está ignorando excepciones silenciosamiente`,
      DoesConsolePrint: (binding, must, target, keyword) => `${binding} está realizando impresiones por pantalla`,
      DoesNilTest: (binding, must, target, keyword) => `${binding} hace comparaciones contra <i>${keyword['null']}</i>`,
      DoesNullTest: (binding, must, target, keyword) => `${binding} hace comparaciones contra <i>${keyword['null']}</i>`,
      DoesTypeTest: (binding, must, target, keyword) => `${binding} hace comparaciones contra strings`,
      HasAssignmentReturn: (binding, must, target, keyword) => `${binding} devuelve el resultado de una asignación`,
      HasCodeDuplication: (binding, must, target, keyword) => `${binding} tiene código repetido`,
      HasEmptyIfBranches: (binding, must, target, keyword) => `${binding} tiene ramas de <i>${keyword['if']}</i> vacías`,
      HasLongParameterList: (binding, must, target, keyword) => `${binding} tiene demasiados parámetros. Te podría estar faltando una abstracción`,
      HasMisspelledBindings: (binding, must, target, keyword) => `${binding} está mal escrito. Revisá la ortografía y procurá no usar abreviaturas`,
      HasMisspelledIdentifiers: (binding, must, target, keyword) => `${binding} está mal escrito. Revisá la ortografía y procurá no usar abreviaturas`,
      HasRedundantBooleanComparison: (binding, must, target, keyword) => `${binding} hace comparaciones booleanas innecesarias`,
      HasRedundantGuards: (binding, must, target, keyword) => `${binding} tiene guardas innecesarias`,
      HasRedundantIf: (binding, must, target, keyword) => `${binding} tiene ifs innecesarios`,
      HasRedundantLambda: (binding, must, target, keyword) => `${binding} tiene lambdas innecesarias`,
      HasRedundantLocalVariableReturn: (binding, must, target, keyword) => `${binding} usa variables locales innecesarias; podés retornar directamente la expresión`,
      HasRedundantParameter: (binding, must, target, keyword) => `${binding} tiene parámetros innecesarios (se pueden eliminar mediante point-free)`,
      HasRedundantReduction: (binding, must, target, keyword) => `${binding} usa <i>${keyword['is']}</i>, pero no realiza cálculos`,
      HasRedundantRepeat: (binding, must, target, keyword) => `${binding} tiene un <i>${keyword['repeat']}</i> innecesario`,
      HasTooManyMethods: (binding, must, target, keyword) => `${binding} tiene demasiados métodos`,
      HasTooShortBindings: (binding, must, target, keyword) => `${binding} es un identificador muy corto`,
      HasTooShortIdentifiers: (binding, must, target, keyword) => `${binding} es un identificador muy corto`,
      HasUnreachableCode: (binding, must, target, keyword) => `${binding} tiene código inalcanzable`,
      HasWrongCaseBindings: (binding, must, target, keyword) => `${binding} no respeta la convención de nombres`,
      HasWrongCaseIdentifiers: (binding, must, target, keyword) => `${binding} no respeta la convención de nombres`,
      Implements_named: (binding, must, target, keyword) => `${binding} ${must} implementar ${target}`,
      Implements: (binding, must, target, keyword) => `${binding} ${must} implementar intefaces`,
      Includes_named: (binding, must, target, keyword) => `${binding} ${must} incluir el mixin ${target}`,
      Includes: (binding, must, target, keyword) => `${binding} ${must} incluir mixins`,
      Inherits_named: (binding, must, target, keyword) => `${binding} ${must} declarar una superclase ${target}`,
      Inherits: (binding, must, target, keyword) => `${binding} ${must} declarar una superclase`,
      Instantiates_named: (binding, must, target, keyword) => `${binding} ${must} instanciar ${target}`,
      Instantiates: (binding, must, target, keyword) => `${binding} ${must} instanciar objetos`,
      IsLongCode: (binding, must, target, keyword) => `${binding} es muy largo. Tratá de delegar más`,
      OverridesEqualOrHashButNotBoth: (binding, must, target, keyword) => `${binding} redefine los métodos <i>equals</i> o <i>hash</i>, pero no ambos`,
      Raises_named: (binding, must, target, keyword) => `${binding} ${must} lanzar ${target}`,
      Raises: (binding, must, target, keyword) => `${binding} ${must} lanzar excepciones`,
      Rescues_named: (binding, must, target, keyword) => `${binding} ${must} capturar ${target}`,
      Rescues: (binding, must, target, keyword) => `${binding} ${must} capturar excepciones`,
      Returns: (binding, must, target, keyword) => `${binding} ${must} retornar`,
      ReturnsNil: (binding, must, target, keyword) => `${binding} retorna null, lo cual es una mala práctica`,
      ReturnsNull: (binding, must, target, keyword) => `${binding} retorna null, lo cual es una mala práctica`,
      TypesAs_named: (binding, must, target, keyword) => `${binding} ${must} ser de tipo ${target}`,
      TypesAs: (binding, must, target, keyword) => `${binding} ${must} tipar`,
      TypesParameterAs_named: (binding, must, target, keyword) => `${binding} ${must} tipar parámetros con el tipo ${target}`,
      TypesParameterAs: (binding, must, target, keyword) => `${binding} ${must} tipar parámetros`,
      TypesReturnAs_named: (binding, must, target, keyword) => `${binding} ${must} tipar su valor de retorno como ${target}`,
      TypesReturnAs: (binding, must, target, keyword) => `${binding} ${must} tipar su valor de retorno`,
      Uses_named: (binding, must, target, keyword) => `${binding} ${must} utilizar ${target}`,
      Uses: (binding, must, target, keyword) => `${binding} ${must} delegar`,
      UsesAnonymousVariable: (binding, must, target, keyword) => `${binding} ${must} utilizar una variable anónima`,
      UsesComposition: (binding, must, target, keyword) => `${binding} ${must} usar composición`,
      UsesComprehension: (binding, must, target, keyword) => `${binding} ${must} emplear listas por comprensión`,
      UsesConditional: (binding, must, target, keyword) => `${binding} ${must} utilizar condicionales`,
      UsesCut: (binding, must, target, keyword) => `${binding} usa el operador !, lo cual es una mala práctica`,
      UsesDyamicPolymorphism: (binding, must, target, keyword) => `${binding} ${must} usar polimorfismo`,
      UsesDynamicMethodOverload: (binding, must, target, keyword) => `${binding} ${must} usar sobrecarga`,
      UsesExceptionHandling: (binding, must, target, keyword) => `${binding} ${must} realizar manejo de excepciones`,
      UsesExceptions: (binding, must, target, keyword) => `${binding} ${must} usar excepciones`,
      UsesFail: (binding, must, target, keyword) => `${binding} usa <i>${keyword['fail']}</li>, lo cual es una mala práctica`,
      UsesFindall: (binding, must, target, keyword) => `${binding} ${must} utilizar <i>${keyword['findall']}</i>`,
      UsesForall: (binding, must, target, keyword) => `${binding} ${must} utilizar <i>${keyword['forall']}</i>`,
      UsesForeach: (binding, must, target, keyword) => `${binding} ${must} usar <i>${keyword['foreach']}</i>`,
      UsesForLoop: (binding, must, target, keyword) => `${binding} ${must} usar <i>${keyword['for']}</i>`,
      UsesGuards: (binding, must, target, keyword) => `${binding} ${must} usar guardas`,
      UsesIf: (binding, must, target, keyword) => `${binding} ${must} usar <i>${keyword['if']}</i>`,
      UsesInheritance: (binding, must, target, keyword) => `${binding} ${must} usar herencia`,
      UsesLambda: (binding, must, target, keyword) => `${binding} ${must} emplear expresiones lambda`,
      UsesLogic: (binding, must, target, keyword) => `${binding} ${must} usar expresiones booleanas`,
      UsesLoop: (binding, must, target, keyword) => `${binding} ${must} usar un bucle`,
      UsesMath: (binding, must, target, keyword) => `${binding} ${must} usar expresiones matemáticas`,
      UsesMixin: (binding, must, target, keyword) => `${binding} ${must} utilizar mixins`,
      UsesMixins: (binding, must, target, keyword) => `${binding} ${must} usar mixins`,
      UsesNot: (binding, must, target, keyword) => `${binding} ${must} utilizar <i>${keyword['not']}</i>`,
      UsesObjectComposition: (binding, must, target, keyword) => `${binding} ${must} usar composición`,
      UsesPatternMatching: (binding, must, target, keyword) => `${binding} ${must} utilizar pattern matching`,
      UsesRepeat: (binding, must, target, keyword) => `${binding} ${must} usar <i>${keyword['repeat']}</i>`,
      UsesRepeatOf: (binding, must, target, keyword) => `${binding} ${must} usar un <i>${keyword['repeat']}</i> de ${target}`,
      UsesStaticMethodOverload: (binding, must, target, keyword) => `${binding} ${must} usar sobrecarga`,
      UsesStaticPolymorphism: (binding, must, target, keyword) => `${binding} ${must} usar polimorfismo`,
      UsesSwitch: (binding, must, target, keyword) => `${binding} ${must} utilizar un <i>${keyword['switch']}</i>`,
      UsesTemplateMethod: (binding, must, target, keyword) => `${binding} ${must} usar un método plantilla`,
      UsesUnificationOperator: (binding, must, target, keyword) => `${binding} usa el operador de unificación (=)`,
      UsesWhile: (binding, must, target, keyword) => `${binding} ${must} utilizar un <i>${keyword['while']}</i>`,
      UsesYield: (binding, must, target, keyword) => `${binding} ${must} utilizar un <i>${keyword['yield']}</i>`
    },
    en: {},
    pt: {}
  };

  const I18n = new class {
    constructor() {
      this.locale = 'en';
    }

    get _translations() {
      return LOCALES[this.locale];
    }

    translate(binding, inspection) {
      const match = inspection.match(/^(Not:)?([^:]+)(:([^:]+))?$/);

      if (!match) throw `unsupported inspection ${inspection}`;

      let key;
      let targetHtml;

      if (match[3] && match[4] !== '*') {
        key = `${match[2]}_named`;
        targetHtml = `<strong>${match[4]}</strong>`
      } else {
        key = match[2];
        targetHtml = null;
      }

      if (!this._translations[key]) throw `unsupported inspection ${inspection} - ${match[1]} - ${match[2]}`;

      return this._translations[key](this._translateBinding(binding), this._translateMust(match), targetHtml, DEFAULT_KEYWORDS);
    }

    _translateBinding(binding) {
      return binding === '*' ? 'la solución' : `<strong>${binding.replace('Intransitive:', '')}</strong>`;
    }

    _translateMust(match) {
      return match[1] ? "no debe" : "debe";
    }
  }

  mulang.I18n = I18n;
})()

describe('v2 expectations', () => {
  before(() => mulang.I18n.locale = 'es');
  it('', () => should(mulang.I18n.translate('*', 'Declares:foo')).eql('la solución debe declarar <strong>foo</strong>'));
  it('', () => should(mulang.I18n.translate('*', 'DeclaresClass:foo')).eql('la solución debe declarar una clase <strong>foo</strong>'));

  it('', () => should(mulang.I18n.translate('Mulang', 'DeclaresClass:Inspection')).eql('<strong>Mulang</strong> debe declarar una clase <strong>Inspection</strong>'));

  it('', () => should(mulang.I18n.translate('*', 'DeclaresObject:foo')).eql('la solución debe declarar un objeto <strong>foo</strong>'));
  it('', () => should(mulang.I18n.translate('foo', 'DeclaresMethod:bar')).eql('<strong>foo</strong> debe declarar un método <strong>bar</strong>'));
  it('', () => should(mulang.I18n.translate('foo', 'Declares')).eql('<strong>foo</strong> debe contener declaraciones'));

  it('', () => should(mulang.I18n.translate('foo.bar', 'DeclaresMethod')).eql('<strong>foo.bar</strong> debe declarar métodos'));
  it('', () => should(mulang.I18n.translate('foo.bar', 'UsesIf')).eql('<strong>foo.bar</strong> debe usar <i>if</i>'));

  it('', () => should(mulang.I18n.translate('Intransitive:foo', 'Not:UsesLambda')).eql('<strong>foo</strong> no debe emplear expresiones lambda'));

  it('', () => should(mulang.I18n.translate('foo', 'Uses:*')).eql('<strong>foo</strong> debe delegar'));
  it('', () => should(mulang.I18n.translate('foo', 'DeclaresMethod:*')).eql('<strong>foo</strong> debe declarar métodos'));
  it('', () => should(mulang.I18n.translate('foo', 'Uses:baz')).eql('<strong>foo</strong> debe utilizar <strong>baz</strong>'));
  it('', () => should(mulang.I18n.translate('foo', 'Uses')).eql('<strong>foo</strong> debe delegar'));
  it('', () => should(mulang.I18n.translate('foo', 'UsesForall')).eql('<strong>foo</strong> debe utilizar <i>forall</i>'));

  it('', () => should(mulang.I18n.translate('foo', 'Not:Uses:*')).eql('<strong>foo</strong> no debe delegar'));
  it('', () => should(mulang.I18n.translate('foo', 'Not:Uses')).eql('<strong>foo</strong> no debe delegar'));
  it('', () => should(mulang.I18n.translate('foo', 'Not:Uses:baz')).eql('<strong>foo</strong> no debe utilizar <strong>baz</strong>'));
  it('', () => should(mulang.I18n.translate('foo', 'Not:UsesLambda')).eql('<strong>foo</strong> no debe emplear expresiones lambda'));

  it('', () => should(mulang.I18n.translate('*', 'DeclaresClass')).eql('la solución debe declarar clases'));
  it('', () => should(mulang.I18n.translate('*', 'Not:DeclaresMethod')).eql('la solución no debe declarar métodos'));
  it('', () => should(mulang.I18n.translate('*', 'Not:DeclaresClass')).eql('la solución no debe declarar clases'));

  it('', () => should(mulang.I18n.translate('foo', 'DeclaresObject')).eql('<strong>foo</strong> debe declarar objetos'));
  it('', () => should(mulang.I18n.translate('*', 'Not:DeclaresClass')).eql('la solución no debe declarar clases'));
  it('', () => should(mulang.I18n.translate('foo', 'UsesAnonymousVariable')).eql('<strong>foo</strong> debe utilizar una variable anónima'));
  it('', () => should(mulang.I18n.translate('*', 'UsesStaticPolymorphism')).eql('la solución debe usar polimorfismo'));
});

describe('smells', () => {
  it('', () => should(mulang.I18n.translate('foo', 'HasRedundantIf')).eql('<strong>foo</strong> tiene ifs innecesarios'));
  it('', () => should(mulang.I18n.translate('foo', 'HasRedundantBooleanComparison')).eql('<strong>foo</strong> hace comparaciones booleanas innecesarias'));
  it('', () => should(mulang.I18n.translate('bar', 'UsesFail')).eql('<strong>bar</strong> usa <i>fail</li>, lo cual es una mala práctica'));
  it('', () => should(mulang.I18n.translate('foo', 'HasEmptyIfBranches')).eql('<strong>foo</strong> tiene ramas de <i>if</i> vacías'));
  it('', () => should(mulang.I18n.translate('foo', 'HasRedundantRepeat')).eql('<strong>foo</strong> tiene un <i>repeat</i> innecesario'));
  it('', () => should(mulang.I18n.translate('foo', 'HasUnreachableCode')).eql('<strong>foo</strong> tiene código inalcanzable'));
  it('', () => should(mulang.I18n.translate('foo', 'HasLongParameterList')).eql('<strong>foo</strong> tiene demasiados parámetros. Te podría estar faltando una abstracción'));
  it('', () => should(mulang.I18n.translate('foo', 'OverridesEqualOrHashButNotBoth')).eql('<strong>foo</strong> redefine los métodos <i>equals</i> o <i>hash</i>, pero no ambos'));
});

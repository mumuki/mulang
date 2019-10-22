class MulangNativeLanguage {
  constructor(language) {
    this.language = language;
  }

  sample(content) {
    return { tag: 'CodeSample', language: this.language, content };
  }

  ast(code) {
    return code.computedAst;
  }
}

class MulangExternalLanguage {
  sample(content) {
    return { tag: 'MulangSample', ast: content };
  }

  ast(code) {
    return code.content;
  }
}

class MulangCode {
  constructor(mulang, language, content) {
    this.mulang = mulang;
    this.language = language;
    this.content = content;
  }

  // Returns the corresponding sample element
  // that is expected by Mulang's Analysis
  get sample() {
    return this.language.sample(this.content);
  }

  // Returns the ast for the given code
  // if the code is native, this ast is first computed by mulang, but
  // if this code is external, the original ast is returned instead
  get ast() {
    return this.language.ast(this);
  }

  get computedAst() {
    return this.analyse({expectations: [], smellsSet: { tag: 'NoSmells'}, includeIntermediateLanguage: true}).intermediateLanguage;
  }

  expect(binding, inspection) {
    return this
            .analyse({expectations: [{binding, inspection}]})
            .expectationResults[0]
            .result;
  }

  customExpect(edl) {
    return Object
      .values(this.analyse({customExpectations: edl}).expectationResults)
      .map((e) => [e.expectation.inspection, e.result]);
  }

  // Runs a mulang analysis, given an spec
  // Throws an error if analysis fails
  analyse(spec) {
    const result = this.mulang.analyse({ sample: this.sample, spec: spec });
    if (result.tag == 'AnalysisFailed') {
      throw new Error(result.reason);
    }
    return result;
  }
}

let ghcjsExports = {
  // internal, do not use
  main: ghcjsWrapperMain("main"),
  // internal, do not use
  analyseIO: ghcjsWrapperStringAny("analyseIO"),

  // Runs a Mulang's analysis
  //
  // Unless you have a good reason, please use one
  // of the following codes instead:
  //
  //   * `astCode(...).analyse(spec)`
  //   * `nativeCode(...).analyse(spec)`
  analyse: function(analysis) {
    return JSON.parse(this.analyseIO(JSON.stringify(analysis)));
  },

  // alias of nativeCode
  code: function(language, content) {
    return this.nativeCode(language, content);
  },

  nativeCode: function(language, content) {
    return new MulangCode(this, new MulangNativeLanguage(language), content);
  },

  astCode: function(ast) {
    return new MulangCode(this, new MulangExternalLanguage(), ast)
  }
};

if (typeof require != 'undefined') {
  ghcjsExports.version = require('../package.json').version;
}

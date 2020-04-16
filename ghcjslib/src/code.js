(() => {
  const Keywords = {
    java: {
      keyword_entry_point: 'main',
      keyword_foreach: 'for'
    },
    javascript: {
      keyword_equal: '===',
      keyword_foreach: 'for'
    },
    haskell: {
      keyword_entry_point: 'main',
      keyword_false: 'False',
      keyword_true: 'True'
    },
    python: {
      keyword_false: 'False',
      keyword_null: 'None',
      keyword_true: 'True',
    },
    ruby: {},
    php: {}
  }

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
    constructor(language, content) {
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
      const result = mulang.analyse({ sample: this.sample, spec: spec });
      if (result.tag == 'AnalysisFailed') {
        throw new Error(result.reason);
      }
      return result;
    }
  }

  ghcjsExports.Keywords = Keywords;
  ghcjsExports.MulangCode = MulangCode;
  ghcjsExports.MulangNativeLanguage = MulangNativeLanguage;
  ghcjsExports.MulangExternalLanguage = MulangExternalLanguage;
})();

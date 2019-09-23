class MulangNativeCode {
  constructor(mulang, language, content) {
    this.mulang = mulang;
    this.sample = { tag: 'CodeSample', language, content };
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

  analyse(spec) {
    const result = this.mulang.analyse({ sample: this.sample, spec: spec });
    if (result.tag == 'AnalysisFailed') {
      throw new Error(result.reason);
    }
    return result;
  }
}

let ghcjsExports = {
  main: ghcjsWrapperMain("main"),
  analyseIO: ghcjsWrapperStringAny("analyseIO"),
  analyse: function(spec) {
    return JSON.parse(this.analyseIO(JSON.stringify(spec)));
  },
  code: function(language, content) {
    return new MulangNativeCode(this, language, content);
  }
};

if (typeof require != 'undefined') {
  ghcjsExports.version = require('../package.json').version;
}

// internal, do not use
ghcjsExports.main = ghcjsWrapperMain("main");

// internal, do not use
ghcjsExports.analyseIO = ghcjsWrapperStringAny("analyseIO");

// Runs a Mulang's analysis
//
// Unless you have a good reason, please use one
// of the following codes instead:
//
//   * `astCode(...).analyse(spec)`
//   * `nativeCode(...).analyse(spec)`
ghcjsExports.analyse = function(analysis) {
    return JSON.parse(this.analyseIO(JSON.stringify(analysis)));
};

// alias of nativeCode
ghcjsExports.code = function(language, content) {
  return this.nativeCode(language, content);
};

ghcjsExports.nativeCode = function(language, content) {
  return new mulang.MulangCode(new mulang.MulangNativeLanguage(language), content);
};

ghcjsExports.astCode = function(ast) {
  return new mulang.MulangCode(new mulang.MulangExternalLanguage(), ast)
};

if (typeof require != 'undefined') {
  ghcjsExports.version = require('../package.json').version;
}

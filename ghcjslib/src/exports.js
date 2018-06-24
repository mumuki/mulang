let ghcjsExports = {
  main: ghcjsWrapperMain("main"),
  analyseIO: ghcjsWrapperStringAny("analyseIO"),
  analyse: function(spec) {
    return JSON.parse(this.analyseIO(JSON.stringify(spec)));
  },
  transpileHaskellIO: ghcjsWrapperStringAny("transpileHaskellIO"),
  evaluateHaskell: function(code) {
    eval(this.transpileHaskellIO(code));
  }
};

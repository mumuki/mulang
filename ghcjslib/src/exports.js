let ghcjsExports = {
  main: ghcjsWrapperUnit("main"),
  analyseIO: ghcjsWrapperStringAny("analyseIO"),
  analyse: function(spec) {
    return JSON.parse(this.analyseIO(JSON.stringify(spec)));
  }
};

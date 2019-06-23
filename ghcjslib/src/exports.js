let ghcjsExports = {
  main: ghcjsWrapperMain("main"),
  analyseIO: ghcjsWrapperStringAny("analyseIO"),
  analyse: function(spec) {
    return JSON.parse(this.analyseIO(JSON.stringify(spec)));
  }
};

if (typeof require != 'undefined') {
  ghcjsExports.version = require('../package.json').version;
}

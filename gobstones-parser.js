var gbs = require('gs-weblang-core/umd/index.umd.min');

function parse(sourceCode) {
    return gbs.parseProgram(sourceCode);
}

function withCode(action) {
  var code = '';
  process.stdin.setEncoding('utf8');
  process.stdin.on('readable', () => {
    var chunk = process.stdin.read();
    if (chunk !== null) {
      code += chunk;
    }
  });
  process.stdin.on('end', () => {
    action(code);
  });
}

withCode(code => {
  console.log(JSON.stringify(parse(code)))
})

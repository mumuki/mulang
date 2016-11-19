var gbs = require('gs-weblang-core/umd/index.umd.min');

function replacer(key,value) {
  if (key == "scope") return undefined;
  else if (key == "token") return undefined;
  else if (key == "interpret") return undefined;
  else if (key == "range") return undefined;
  else if (key == "eval") return undefined;
  else if (key == "type") return undefined;
  else return value;
}

function parse(sourceCode) {
  return gbs.getParser().parseProgram(sourceCode);
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
  console.log(JSON.stringify(parse(code), replacer))
})

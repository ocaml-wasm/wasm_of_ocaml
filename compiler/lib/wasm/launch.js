const fs = require('fs');
const runtime = fs.readFileSync('runtime.wasm');
const wasmBuffer = fs.readFileSync('/tmp/a.wasm');
function f(){console.log('ZZZ', arguments); return 0}
function g(x){return x}
function toto(x,y){console.log('AAA', x, y); return 0}


WebAssembly.instantiate(runtime).then(runtime =>
WebAssembly.instantiate(wasmBuffer, {env:runtime.instance.exports}).then(wasmModule => wasmModule.instance.exports.kernel_run()));

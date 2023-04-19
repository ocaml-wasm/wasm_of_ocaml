const fs = require('fs');
const runtime = fs.readFileSync('runtime.wasm');
const wasmBuffer = fs.readFileSync('/tmp/a.wasm');
function f(){console.log('ZZZ', arguments); return 0}
function g(x){return x}
function toto(x,y){console.log('AAA', x, y); return 0}


WebAssembly.instantiate(runtime, {Math:{cos:Math.cos,sin:Math.sin,asin:Math.asin,atan2:Math.atan2,pow:Math.pow,mod:(x, y) => x%y}}).then(runtime =>
WebAssembly.instantiate(wasmBuffer, {env:runtime.instance.exports}).then(wasmModule => wasmModule.instance.exports.kernel_run()));

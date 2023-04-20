const fs = require('fs');
const runtime = fs.readFileSync('runtime.wasm');
const wasmBuffer = fs.readFileSync('/tmp/a.wasm');
function f(){console.log('ZZZ', arguments); return 0}
function g(x){return x}
function toto(x,y){console.log('AAA', x, y); return 0}

let math = {cos:Math.cos,sin:Math.sin,asin:Math.asin,atan2:Math.atan2,pow:Math.pow,fmod:(x, y) => x%y}
WebAssembly.instantiate(runtime, {Math:math}).then(runtime =>
WebAssembly.instantiate(wasmBuffer, {env:runtime.instance.exports,Math:math}).then(wasmModule => wasmModule.instance.exports.kernel_run()));

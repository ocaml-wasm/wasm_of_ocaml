const fs = require('fs');
const runtime = fs.readFileSync('runtime.wasm');
const code = fs.readFileSync('/tmp/a.wasm');
let math = {cos:Math.cos,sin:Math.sin,asin:Math.asin,atan2:Math.atan2,pow:Math.pow,fmod:(x, y) => x%y}
WebAssembly.instantiate(runtime, {Math:math}).then(runtime =>
WebAssembly.instantiate(code, {env:runtime.instance.exports,Math:math}).then(wasmModule => wasmModule.instance.exports._initialize()));

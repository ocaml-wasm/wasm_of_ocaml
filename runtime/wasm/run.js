const fs = require('fs/promises');
const path = require('path');

async function main() {
    const runtimePath =
          path.resolve(path.dirname(process.argv[1]), 'runtime.wasm');
    const runtime = fs.readFile(runtimePath);
    const code = fs.readFile(process.argv[2]);
    let math =
        {cos:Math.cos, sin:Math.sin, asin:Math.asin, atan2:Math.atan2,
         pow:Math.pow, fmod:(x, y) => x%y,
         log:(x)=>console.log(x)}
    let bindings =
        {identity:(x)=>x,
         from_bool:(x)=>!!x}
    const runtimeModule =
          await WebAssembly.instantiate(await runtime,
                                        {Math:math,bindings:bindings});
    const wasmModule =
          await WebAssembly.instantiate(await code,
                                        {env:runtimeModule.instance.exports,
                                         Math:math})
    try {
      wasmModule.instance.exports._initialize()
    } catch (e) {
        if (e instanceof WebAssembly.Exception &&
            e.is(runtimeModule.instance.exports.ocaml_exit))
            process.exit(e.getArg(runtimeModule.instance.exports.ocaml_exit, 0));
        if (e instanceof WebAssembly.Exception &&
            e.is(runtimeModule.instance.exports.ocaml_exception)) {
            console.log('Uncaught exception')
            process.exit(1)
        }
        throw e;
    }
}

main ()

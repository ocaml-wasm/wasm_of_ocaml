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
         log:(x)=>console.log('ZZZZZ', x)}
    let bindings =
        {identity:(x)=>x,
         from_bool:(x)=>!!x,
         get:(x,y)=>{console.log('GET', x, y); return x[y]},
         set:(x,y,z)=>{console.log('SET', x, y, z); x[y]=z},
         eval:(x)=>{console.log('<===', x); var y = eval(x); console.log ('===>', y); return y},
         strict_equals:(x,y)=>x===y,
         fun_call:(f,args)=>{console.log('CALL', f, args); return f.apply(null,args)},
         meth_call:(o,f,args)=>{console.log('CALL', o, f, args); return o[f].apply(o,args)},
         new_array:(n)=>new Array(n),
         new_obj:()=>({}),
         new:(c,args)=>{return new c(...args)},
         array_length:(a)=>a.length,
         array_get:(a,i)=>a[i],
         array_set:(a,i,v)=>a[i]=v
        }
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

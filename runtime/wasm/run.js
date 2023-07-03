#!/usr/bin/env -S node --experimental-wasm-stringref  --experimental-wasm-gc
(async function () {
    const fs = require('fs/promises');
    const path = require('path');
    const runtimePath =
          path.resolve(path.dirname(process.argv[1]), 'runtime.wasm');
    const runtime = fs.readFile(runtimePath);
    const code = fs.readFile(process.argv[2]);

    var caml_callback;
    var caml_alloc_tm;

    let math =
        {cos:Math.cos, sin:Math.sin, tan:Math.tan,
         acos:Math.acos, asin:Math.asin, atan:Math.atan,
         cosh:Math.cosh, sinh:Math.sinh, tanh:Math.tanh,
         acosh:Math.acosh, asinh:Math.asinh, atanh:Math.atanh,
         cbrt:Math.cbrt, exp:Math.exp, expm1:Math.expm1,
         log:Math.log, log1p:Math.log1p, log2:Math.log2, log10:Math.log10,
         atan2:Math.atan2, hypot:Math.hypot, pow:Math.pow,
         fmod:(x, y) => x%y}

    let typed_arrays =
      [Float32Array, Float64Array, Int8Array, Uint8Array, Int16Array,
       Uint16Array, Int32Array, Int32Array, Int32Array, Int32Array,
       Float32Array, Float64Array, Uint8Array]

    let bindings =
        {identity:(x)=>x,
         from_bool:(x)=>!!x,
         get:(x,y)=>x[y],
         set:(x,y,z)=>x[y]=z,
         delete:(x,y)=>delete x[y],
         instanceof:(x,y)=>x instanceof y,
         typeof:(x)=>typeof x,
         eval:eval,
         equals:(x,y)=>x==y,
         strict_equals:(x,y)=>x===y,
         fun_call:(f,o,args)=>f.apply(o,args),
         meth_call:(o,f,args)=>o[f].apply(o,args),
         new_array:(n)=>new Array(n),
         new_obj:()=>({}),
         new:(c,args)=>new c(...args),
         iter_props:(o,f)=>{for (var nm in o) if(o.hasOwnsProperty(nm)) f(nm)},
         array_length:(a)=>a.length,
         array_get:(a,i)=>a[i],
         array_set:(a,i,v)=>a[i]=v,
         get_int:(a,i)=>a[i],
         ta_create:(k,sz)=> new(typed_arrays[k])(sz),
         ta_normalize:(a)=>
           a instanceof Uint8ClampedArray?
             new Uint8Array(a.buffer,a.byteOffset,a.byteLength):
             a instanceof Uint32Array?
               new Int32Array(a.buffer,a.byteOffset,a.byteLength):a,
         ta_kind:(a)=>typed_arrays.findIndex((c)=>a instanceof c),
         ta_length:(a)=>a.length,
         ta_get_f64:(a,i)=>a[i],
         ta_get_f32:(a,i)=>a[i],
         ta_get_i32:(a,i)=>a[i],
         ta_get_i16:(a,i)=>a[i],
         ta_get_ui16:(a,i)=>a[i],
         ta_get_i8:(a,i)=>a[i],
         ta_get_ui8:(a,i)=>a[i],
         ta_set_f64:(a,i,v)=>a[i]=v,
         ta_set_f32:(a,i,v)=>a[i]=v,
         ta_set_i32:(a,i,v)=>a[i]=v,
         ta_set_i16:(a,i,v)=>a[i]=v,
         ta_set_ui16:(a,i,v)=>a[i]=v,
         ta_set_i8:(a,i,v)=>a[i]=v,
         ta_set_ui8:(a,i,v)=>a[i]=v,
         wrap_callback_strict:(arity,f)=>function (){
             var n = arguments.length;
             var args = new Array(arity);
             var len = Math.min(arguments.length, arity)
             for (var i = 0; i < len; i++) args[i] = arguments[i];
             return caml_callback(f, arity, args);
         },
         wrap_fun_arguments:(f)=>function(){return f(arguments)},
         format:(f)=>""+f,
         gettimeofday:()=>(new Date()).getTime() / 1000,
         gmtime:(t)=>{
           var d = new Date (t * 1000);
           var d_num = d.getTime();
           var januaryfirst =
             (new Date(Date.UTC(d.getUTCFullYear(), 0, 1))).getTime();
           var doy = Math.floor((d_num - januaryfirst) / 86400000);
           return caml_alloc_tm(d.getUTCSeconds(), d.getUTCMinutes(),
                                d.getUTCHours(), d.getUTCDate(),
                                d.getUTCMonth(), d.getUTCFullYear() - 1900,
                                d.getUTCDay(), doy, false)
         },
         localtime:(t)=>{
           var d = new Date (t * 1000);
           var d_num = d.getTime();
           var januaryfirst = (new Date(d.getFullYear(), 0, 1)).getTime();
           var doy = Math.floor((d_num - januaryfirst) / 86400000);
           var jan = new Date(d.getFullYear(), 0, 1);
           var jul = new Date(d.getFullYear(), 6, 1);
           var stdTimezoneOffset =
               Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
           return caml_alloc_tm(d.getSeconds(), d.getMinutes(), d.getHours(),
                                d.getDate(), d.getMonth(),
                                d.getFullYear() - 1900,
                                d.getDay(), doy,
                                (d.getTimezoneOffset() < stdTimezoneOffset))
         }
         log:(x)=>console.log('ZZZZZ', x)
        }
    const runtimeModule =
          await WebAssembly.instantiate(await runtime,
                                        {Math:math,bindings:bindings});

    caml_callback = runtimeModule.instance.exports.caml_callback;
    caml_alloc_tm = runtimeModule.instance.exports.caml_alloc_tm;

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

})()

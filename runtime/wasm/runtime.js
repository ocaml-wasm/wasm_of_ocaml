#!/usr/bin/env -S node --experimental-wasm-stringref --experimental-wasm-gc
(async function (eval_function, js) {
    "use strict";
    const src = 'CODE';
    function loadRelative(src) {
      const path = require('path');
      const f = path.join(path.dirname(require.main.filename),src);
      return require('fs/promises').readFile(f)
    }
    const isNode = globalThis?.process?.versions?.node;
    const code = isNode?loadRelative(src):fetch(src);

    var caml_callback, caml_alloc_tm;

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

    const fs = isNode&&require('fs')

    let fs_cst = fs?.constants;

    let open_flags =
      fs?[fs_cst.RDONLY,fs_cst.O_WRONLY,fs_cst.O_APPEND,fs_cst.O_CREAT,
          fs_cst.O_TRUNC,fs_cst.O_EXCL,fs_cst.O_NONBLOCK]:[]

    var out_channels =
      { map : new WeakMap(), set : new Set(),
        finalization :
          new FinalizationRegistry ((ref)=>out_channels.set.delete(ref)) };

    function register_channel (ch) {
      const ref = new WeakRef (ch);
      out_channels.map.set(ch, ref);
      out_channels.set.add(ref);
      out_channels.finalization.register(ch, ref, ch);
    }

    function unregister_channel (ch) {
      const ref = out_channels.map.get(ch);
      if (ref) {
        out_channels.map.delete(ch);
        out_channels.set.delete(ref);
        out_channels.finalization.unregister(ch);
      }
    }

    function channel_list () {
      return [...out_channels.set].map((ref) => ref.deref()).filter((ch)=>ch);
    }

    var start_fiber

    function wrap_fun (t,f,a) {
       // Don't wrap if js-promise-integration is not enabled
       // There is no way to check this without calling WebAssembly.Function
       try {
         return new WebAssembly.Function(t,f,a)
       } catch (e) {
         return f
       }
    }

    let bindings =
        {jstag:WebAssembly.JSTag,
         identity:(x)=>x,
         from_bool:(x)=>!!x,
         get:(x,y)=>x[y],
         set:(x,y,z)=>x[y]=z,
         delete:(x,y)=>delete x[y],
         instanceof:(x,y)=>+(x instanceof y),
         typeof:(x)=>typeof x,
         eval:eval_function,
         equals:(x,y)=>+(x==y),
         strict_equals:(x,y)=>+(x===y),
         fun_call:(f,o,args)=>f.apply(o,args),
         meth_call:(o,f,args)=>o[f].apply(o,args),
         new_array:(n)=>new Array(n),
         new_obj:()=>({}),
         new:(c,args)=>new c(...args),
         global_this:globalThis,
         iter_props:(o,f)=>{for (var nm in o) if(o.hasOwnsProperty(nm)) f(nm)},
         array_length:(a)=>a.length,
         array_get:(a,i)=>a[i],
         array_set:(a,i,v)=>a[i]=v,
         ta_create:(k,sz)=> new(typed_arrays[k])(sz),
         ta_normalize:(a)=>
           a instanceof Uint32Array?
           new Int32Array(a.buffer,a.byteOffset,a.length):a,
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
         ta_fill:(a,v)=>a.fill(v),
         ta_blit:(s,d)=>d.set(s),
         ta_subarray:(a,i,j)=>a.subarray(i,j),
         ta_set:(a,b,i)=>a.set(b,i),
         ta_new:(len)=>new Uint8Array(len),
         ta_copy:(ta,t,s,n)=>ta.copyWithin(t,s,n),
         ta_bytes:(a)=>
           new Uint8Array(a.buffer, a.byteOffset,
                          a.length * a.BYTES_PER_ELEMENT),
         wrap_callback:(f)=>function (){
             var n = arguments.length;
             if(n > 0) {
                 var args = new Array(n);
                 for (var i = 0; i < n; i++) args[i] = arguments[i];
             } else {
                 args = [undefined];
             }
             return caml_callback(f, args.length, args, 1);
         },
         wrap_callback_args:(f)=>function (){
             var n = arguments.length;
             var args = new Array(n);
             for (var i = 0; i < n; i++) args[i] = arguments[i];
             return caml_callback(f, 1, [args], 0);
         },
         wrap_callback_strict:(arity,f)=>function (){
             var n = arguments.length;
             var args = new Array(arity);
             var len = Math.min(arguments.length, arity)
             for (var i = 0; i < len; i++) args[i] = arguments[i];
             return caml_callback(f, arity, args, 0);
         },
         wrap_callback_unsafe:(f)=>function (){
             var n = arguments.length;
             var args = new Array(n);
             for (var i = 0; i < n; i++) args[i] = arguments[i];
             return caml_callback(f, args.length, args, 2);
         },
         wrap_meth_callback:(f)=>function (){
             var n = arguments.length;
             var args = new Array(n+1);
             args[0] = this;
             for (var i = 0; i < n; i++) args[i+1] = arguments[i];
             return caml_callback(f, args.length, args, 1);
         },
         wrap_meth_callback_args:(f)=>function (){
             var n = arguments.length;
             var args = new Array(n);
             for (var i = 0; i < n; i++) args[i] = arguments[i];
             return caml_callback(f, 2, [this, args], 0);
         },
         wrap_meth_callback_strict:(arity,f)=>function (){
             var args = new Array(arity + 1);
             var len = Math.min(arguments.length, arity)
             args[0] = this;
             for (var i = 0; i < len; i++) args[i+1] = arguments[i];
             return caml_callback(f, args.length, args, 0);
         },
         wrap_meth_callback_unsafe:(f)=>function (){
             var n = arguments.length;
             var args = new Array(n+1);
             args[0] = this;
             for (var i = 0; i < n; i++) args[i+1] = arguments[i];
             return caml_callback(f, args.length, args, 2);
         },
         wrap_fun_arguments:(f)=>function(){return f(arguments)},
         format_float:(prec, conversion, x)=>{
           function toFixed(x,dp) {
             if (Math.abs(x) < 1.0) {
               return x.toFixed(dp);
             } else {
               var e = parseInt(x.toString().split('+')[1]);
               if (e > 20) {
                 e -= 20;
                 x /= Math.pow(10,e);
                 x += (new Array(e+1)).join('0');
                 if(dp > 0) {
                   x = x + '.' + (new Array(dp+1)).join('0');
                 }
                 return x;
               }
               else return x.toFixed(dp)
             }
           }
           switch (conversion) {
           case 0:
             var s = x.toExponential(prec);
             // exponent should be at least two digits
             var i = s.length;
             if (s.charAt(i - 3) == 'e')
               s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
             break;
           case 1:
             s = toFixed(x, prec); break;
           case 2:
             prec = prec?prec:1;
             s = x.toExponential(prec - 1);
             var j = s.indexOf('e');
             var exp = +s.slice(j + 1);
             if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
               // remove trailing zeroes
               var i = j - 1; while (s.charAt(i) == '0') i--;
               if (s.charAt(i) == '.') i--;
               s = s.slice(0, i + 1) + s.slice(j);
               i = s.length;
               if (s.charAt(i - 3) == 'e')
                 s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
               break;
             } else {
               var p = prec;
               if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
               else while (s = x.toFixed(p), s.length > prec + 1) p--;
               if (p) {
                 // remove trailing zeroes
                 var i = s.length - 1; while (s.charAt(i) == '0') i--;
                 if (s.charAt(i) == '.') i--;
                 s = s.slice(0, i + 1);
               }
             }
             break;
           }
           return s
         },
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
         },
         mktime:(year,month,day,h,m,s)=>new Date(year,month,day,h,m,s).getTime(),
         random_seed:()=>crypto.getRandomValues(new Int32Array(12)),
         open:(p,flags,perm)=>
           fs.openSync(p,open_flags.reduce((f,v,i)=>(flags&(1<<i))?(f|v):f,0),
                       perm),
         close:(fd)=>fs.closeSync(fd),
         write:(fd,b,o,l)=>fs?fs.writeSync(fd,b,o,l):(console.log(new TextDecoder().decode(b.slice(o,o+l))),l),
         read:(fd,b,o,l,p)=>fs.readSync(fd,b,o,l,p),
         file_size:(fd)=>fs.fstatSync(fd,{bigint:true}).size,
         register_channel,
         unregister_channel,
         channel_list,
         argv:()=>isNode?process.argv.slice(1):['a.out'],
         getenv:(n)=>isNode?process.env[n]:null,
         system:(c)=>{
           var res = require('child_process').spawnSync(c,{shell:true, stdio: 'inherit'});
           return res.signal?128:res.status
         },
         getcwd:()=>isNode?process.cwd():'/static',
         chdir:(x)=>process.chdir(x),
         unlink:(p)=>fs.unlinkSync(p),
         readdir:(p)=>fs.readdirSync(p),
         file_exists:(p)=>+fs.existsSync(p),
         start_fiber:(x)=>start_fiber(x),
         suspend_fiber:
         wrap_fun(
             {parameters: ['externref','funcref','eqref'], results: ['eqref']},
             ((f, env)=>new Promise((k)=> f(k, env))),
             {suspending:"first"}),
         resume_fiber:(k,v)=>k(v),
         weak_new:(v)=>new WeakRef(v),
         weak_deref:(w)=>{var v = w.deref(); return v==undefined?null:v},
         weak_map_new:()=>new WeakMap,
         weak_map_get:(m,x)=>{var v = m.get(x); return v==undefined?null:v},
         weak_map_set:(m,x,v)=>m.set(x,v),
         weak_map_delete:(m,x)=>m.delete(x),
         log:(x)=>console.log('ZZZZZ', x)
        }
    const imports = {Math:math,bindings:bindings,env:{},js:js}
    const wasmModule =
          isNode?await WebAssembly.instantiate(await code, imports)
                :await WebAssembly.instantiateStreaming(code,imports)

    caml_callback = wasmModule.instance.exports.caml_callback;
    caml_alloc_tm = wasmModule.instance.exports.caml_alloc_tm;

    start_fiber = wrap_fun(
        {parameters: ['eqref'], results: ['externref']},
        wasmModule.instance.exports.caml_start_fiber,
        {promising: 'first'}
    )
    var _initialize = wrap_fun(
        {parameters: [], results: ['externref']},
        wasmModule.instance.exports._initialize,
        {promising: 'first'}
    )

    try {
        await _initialize()
    } catch (e) {
        if (e instanceof WebAssembly.Exception) {
          const exit_tag = wasmModule.instance.exports.ocaml_exit;
          if (exit_tag && e.is(exit_tag))
            isNode && process.exit(e.getArg(exit_tag, 0));
          const exn_tag = wasmModule.instance.exports.ocaml_exception;
          if (exn_tag && e.is(exn_tag)) {
            var exn = e.getArg(exn_tag, 0)
            var handle_uncaught_exception =
              wasmModule.instance.exports.caml_named_value
                ('Printexc.handle_uncaught_exception');
            if (handle_uncaught_exception)
              wasmModule.instance.exports.caml_callback
                (handle_uncaught_exception, 2, [exn, 0], 0)
            else {
                var at_exit =
                    wasmModule.instance.exports.caml_named_value
                      ('Pervasives.do_at_exit');
                if (at_exit)
                    wasmModule.instance.exports.caml_callback
                      (at_exit, 1, [0], 0);
                console.error (
                    "Fatal error: exception " +
                        wasmModule.instance.exports.caml_format_exception(exn) +
                        "\n"
                )
            }
            isNode && process.exit(2)
          }
        } else {
            throw e;
        }
    }
})(((joo_global_object,globalThis)=>(x)=>eval(x))(globalThis,globalThis),
   PRIMITIVES);

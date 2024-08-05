(module
   (import "fail" "caml_raise_constant"
      (func $caml_raise_constant (param (ref eq))))
   (import "fail" "caml_raise_with_arg"
      (func $caml_raise_with_arg (param $tag (ref eq)) (param $arg (ref eq))))
   (import "obj" "caml_fresh_oo_id"
     (func $caml_fresh_oo_id (param (ref eq)) (result (ref eq))))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref $string)) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "jslib" "caml_wrap_exception"
      (func $caml_wrap_exception (param externref) (result (ref eq))))
   (import "bindings" "start_fiber" (func $start_fiber (param (ref eq))))
   (import "bindings" "suspend_fiber"
      (func $suspend_fiber
         (param externref) (param $f funcref) (param $env eqref)
         (result eqref)))
   (import "bindings" "resume_fiber"
      (func $resume_fiber (param externref) (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field (ref $function_1)))))
   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   ;; Effect types

   (tag $effect (param (ref eq)) (result (ref eq) (ref eq)))

   (type $cont_function (func (param (ref eq) (ref eq)) (result (ref eq))))

   (type $cont (cont $cont_function))

   (type $handlers
      (struct
         (field $value (ref eq))
         (field $exn (ref eq))
         (field $effect (ref eq))))

   (type $generic_fiber (sub (struct (field $handlers (mut (ref $handlers))))))

   (type $fiber
      (sub final $generic_fiber
         (struct
            (field $handlers (mut (ref $handlers)))
            (field $cont (ref $cont)))))

   (type $continuation (struct (mut eqref)))

(;ZZZ need to install an effect handler when starting up; not sure
      what to do in callback

   (data $effect_unhandled "Effect.Unhandled")

   (func $raise_unhandled
      (param $eff (ref eq)) (param (ref eq)) (result (ref eq))
      (local $effect_unhandled (ref $string))
      (local.set $effect_unhandled
         (array.new_data $string $effect_unhandled
            (i32.const 0) (i32.const 16)))
      (block $null
         (call $caml_raise_with_arg
            (br_on_null $null
               (call $caml_named_value
                  (local.get $effect_unhandled)))
            (local.get $eff)))
      (call $caml_raise_constant
         (array.new_fixed $block 3 (ref.i31 (i32.const 248))
            (local.get $effect_unhandled)
            (call $caml_fresh_oo_id (ref.i31 (i32.const 0)))))
      (ref.i31 (i32.const 0)))
;)

   ;; Resume

   (data $already_resumed "Effect.Continuation_already_resumed")

   (func $resume (export "native--%resume")
      (param $vfiber (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $fiber (ref $fiber))
      (local $res (ref eq))
      (local $exn (ref eq))
      (local $resume_res (tuple (ref eq) (ref $cont)))
      (local.set $fiber (ref.cast (ref $fiber) (local.get $vfiber)))
      (if (ref.eq (local.get $fiber) (ref.i31 (i32.const 0)))
         (then
            (call $caml_raise_constant
               (ref.as_non_null
                  (call $caml_named_value
                     (array.new_data $string $already_resumed
                        (i32.const 0) (i32.const 35)))))))
      (local.set $exn
         (block $handle_exception (result (ref eq))
            (local.set $resume_res
               (block $handle_effect (result (ref eq) (ref $cont))
                  (local.set $res
                     (try (result (ref eq))
                        (do
                           (resume $cont
                               (tag $effect $handle_effect)
                               (local.get $f) (local.get $v)
                               (struct.get $fiber $cont (local.get $fiber))))
                        (catch $javascript_exception
                           (br $handle_exception
                              (call $caml_wrap_exception (pop externref))))
                        (catch $ocaml_exception
                           (br $handle_exception (pop (ref eq))))))
                  ;; handle return
                  (return_call_ref $function_1 (local.get $res)
                     (local.tee $f
                        (struct.get $handlers $value
                           (struct.get $fiber $handlers (local.get $fiber))))
                     (struct.get $closure 0
                        (ref.cast (ref $closure) (local.get $f))))))
            ;; handle effect
            (return_call_ref $function_3
               (tuple.extract 2 0 (local.get $resume_res))
               (struct.new $continuation
                  (struct.new $fiber
                     (struct.get $fiber $handlers (local.get $fiber))
                     (tuple.extract 2 1 (local.get $resume_res))))
               (ref.i31 (i32.const 0)) ;; unused
               (local.tee $f
                  (struct.get $handlers $effect
                     (struct.get $fiber $handlers (local.get $fiber))))
               (struct.get $closure_3 1
                  (ref.cast (ref $closure_3) (local.get $f))))))
      ;; handle exception
      (return_call_ref $function_1 (local.get $exn)
         (local.tee $f
            (struct.get $handlers $value
               (struct.get $fiber $handlers (local.get $fiber))))
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   ;; Perform

   (func (export "native--%reperform")
      (param $eff (ref eq)) (param $cont (ref eq)) (result (ref eq))
      (local $res (tuple (ref eq) (ref eq)))
      (local.set $res (suspend $effect (local.get $eff)))
      (return_call $resume
         (ref.as_non_null
            (struct.get $continuation 0
               (ref.cast (ref $continuation) (local.get $cont))))
         (tuple.extract 2 0 (local.get $res))
         (tuple.extract 2 1 (local.get $res))))

   (func (export "native--%perform") (param $eff (ref eq)) (result (ref eq))
      (local $res (tuple (ref eq) (ref eq)))
      (local.set $res (suspend $effect (local.get $eff)))
      (return_call_ref $function_1 (tuple.extract 2 1 (local.get $res))
         (tuple.extract 2 0 (local.get $res))
         (struct.get $closure 0
            (ref.cast (ref $closure) (tuple.extract 2 0 (local.get $res))))))

   ;; Allocate a stack

   (func $initial_cont
      (param $f (ref $closure)) (param $x (ref eq)) (result (ref eq))
      (return_call_ref $function_1 (local.get $x)
         (local.get $f)
         (struct.get $closure 0 (local.get $f))))

   (func (export "native--caml_alloc_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (result (ref eq))
      (struct.new $fiber
         (struct.new $handlers (local.get $hv) (local.get $hx) (local.get $hf))
         (cont.new $cont (ref.func $initial_cont))))
)

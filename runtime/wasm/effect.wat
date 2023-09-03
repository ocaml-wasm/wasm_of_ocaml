(module
   (import "fail" "caml_raise_constant"
      (func $caml_raise_constant (param (ref eq))))
   (import "fail" "caml_raise_with_arg"
      (func $caml_raise_with_arg (param $tag (ref eq)) (param $arg (ref eq))))
   (import "obj" "caml_fresh_oo_id"
     (func $caml_fresh_oo_id (param (ref eq)) (result (ref eq))))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param anyref) (result (ref null eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "jslib" "caml_wrap_exception"
      (func $caml_wrap_exception (param externref) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field (ref $function_1)))))
   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   ;; Apply a function f to a value v, both contained in a pair (f, v)

   (type $pair (struct (field (ref eq)) (field (ref eq))))

   (func $apply_pair (param $p (ref $pair)) (result (ref eq))
      (local $f (ref eq))
      (return_call_ref $function_1 (struct.get $pair 1 (local.get $p))
         (local.tee $f (struct.get $pair 0 (local.get $p)))
         (struct.get $closure 0 (ref.cast (ref $closure) (local.get $f)))))

   ;; Low-level primitives

   (import "bindings" "start_fiber" (func $start_fiber (param (ref eq))))
   (import "bindings" "suspend_fiber"
      (func $suspend_fiber
         (param externref) (param $f funcref) (param $env eqref)
         (result eqref)))
   (import "bindings" "resume_fiber"
      (func $resume_fiber (param externref) (param (ref eq))))

   (global $current_suspender (mut (externref)) (ref.null extern))

   ;; Capturing the current continuation

   (type $cont_func (func (param (ref $pair)) (param (ref eq))))
   (type $cont (sub (struct (field $cont_func (ref $cont_func)))))

   (type $called_with_continuation
      (func (param (ref $cont)) (param (ref eq))))

   (type $thunk
      (struct (field (ref $called_with_continuation)) (field (ref eq))))

   (type $cont_resume
      (sub final $cont
         (struct
            (field $cont_func (ref $cont_func))
            (field $cont_resolver externref))))

   (func $invoke_promise_resolver (param $p (ref $pair)) (param (ref eq))
      (call $resume_fiber
         (struct.get $cont_resume $cont_resolver
            (ref.cast (ref $cont_resume) (local.get 1)))
         (local.get $p)))

   (func $apply_continuation (param $resolver (ref extern)) (param $v (ref eq))
      (local $t (ref $thunk))
      (local.set $t (ref.cast (ref $thunk) (local.get $v)))
      (return_call_ref $called_with_continuation
         (struct.new $cont_resume
            (ref.func $invoke_promise_resolver) (local.get $resolver))
         (struct.get $thunk 1 (local.get $t))
         (struct.get $thunk 0 (local.get $t))))

   (func $capture_continuation
      (param $f (ref $called_with_continuation))
      (param $v (ref eq))
      (result (ref eq))
      (return_call $apply_pair
         (ref.cast (ref $pair)
            (call $suspend_fiber
               (global.get $current_suspender)
               (ref.func $apply_continuation)
               (struct.new $thunk (local.get $f) (local.get $v))))))

   ;; Stack of fibers

   (type $handlers (array (ref eq)))

   (type $fiber
      (struct
         (field $fiber_handlers (mut (ref $handlers)))
         (field $fiber_cont (ref $cont))
         (field $fiber_suspender externref)
         (field $fiber_next (ref null $fiber))))

   (type $continuation (struct (mut (ref null $fiber))))

   (data $effect_unhandled "Effect.Unhandled")

   (func $raise_unhandled
      (param $eff (ref eq)) (param (ref eq)) (result (ref eq))
      (block $null
         (call $caml_raise_with_arg
            (br_on_null $null
               (call $caml_named_value
                  (string.const "Effect.Unhandled")))
            (local.get $eff)))
      (call $caml_raise_constant
         (array.new_fixed $block 3 (i31.new (i32.const 248))
            (array.new_data $string $effect_unhandled
               (i32.const 0) (i32.const 16))
            (call $caml_fresh_oo_id (i31.new (i32.const 0)))))
      (i31.new (i32.const 0)))

   (func $uncaught_effect_handler
      (param $eff (ref eq)) (param $cont (ref eq)) (param $k (ref eq))
      (param (ref eq)) (result (ref eq))
      (local $k' (ref $cont))
      (local.set $k'
         (call $push_stack
            (ref.as_non_null
               (struct.get $continuation 0
                  (ref.cast (ref $continuation) (local.get $cont))))
            (ref.cast (ref $cont) (local.get $k))))
      (call_ref $cont_func
         (struct.new $pair
            (struct.new $closure (ref.func $raise_unhandled))
            (local.get $eff))
         (local.get $k')
         (struct.get $cont $cont_func (local.get $k')))
      (i31.new (i32.const 0)))

   (func $dummy_fun (param (ref eq)) (param (ref eq)) (result (ref eq))
      (unreachable))

   (func $default_continuation (param $p (ref $pair)) (param (ref eq))
      (drop (call $apply_pair (local.get $p))))

   (global $fiber_stack (mut (ref null $fiber))
      (struct.new $fiber
         (array.new_fixed $handlers 3
            (i31.new (i32.const 0))
            (i31.new (i32.const 0))
            (struct.new $closure_3
               (ref.func $dummy_fun)
               (ref.func $uncaught_effect_handler)))
         (struct.new $cont (ref.func $default_continuation))
         (ref.null extern)
         (ref.null $fiber)))

   ;; Utility functions moving fibers between a continuation and the
   ;; current stack of fibers

   (func $pop_fiber (result (ref $cont))
      (local $f (ref $fiber))
      (local.set $f (ref.as_non_null (global.get $fiber_stack)))
      (global.set $fiber_stack
         (struct.get $fiber $fiber_next (local.get $f)))
      (global.set $current_suspender
         (struct.get $fiber $fiber_suspender (local.get $f)))
      (struct.get $fiber $fiber_cont (local.get $f)))

   (func $push_stack
      (param $stack (ref $fiber)) (param $k (ref $cont))
      (result (ref $cont))
      (block $done
         (loop $loop
            (global.set $fiber_stack
               (struct.new $fiber
                  (struct.get $fiber $fiber_handlers (local.get $stack))
                  (local.get $k)
                  (global.get $current_suspender)
                  (global.get $fiber_stack)))
            (global.set $current_suspender
               (struct.get $fiber $fiber_suspender (local.get $stack)))
            (local.set $k
               (struct.get $fiber $fiber_cont (local.get $stack)))
            (local.set $stack
               (br_on_null $done
                  (struct.get $fiber $fiber_next (local.get $stack))))
            (br $loop)))
      (local.get $k))

   ;; Resume

   (func $do_resume (param $k (ref $cont)) (param $vp (ref eq))
      (local $p (ref $pair))
      (local $stack (ref $fiber))
      (local.set $p (ref.cast (ref $pair) (local.get $vp)))
      (local.set $stack
         (ref.cast (ref $fiber) (struct.get $pair 0 (local.get $p))))
      (local.set $p (ref.cast (ref $pair) (struct.get $pair 1 (local.get $p))))
      (local.set $k (call $push_stack (local.get $stack) (local.get $k)))
      (call_ref $cont_func (local.get $p) (local.get $k)
         (struct.get $cont $cont_func (local.get $k))))

   (func (export "%resume")
      (param $stack (ref eq)) (param $f (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $k (ref $cont))
      (local $pair (ref $pair))
      (if (ref.eq (local.get $stack) (i31.new (i32.const 0)))
         (then
            (call $caml_raise_constant
               (ref.as_non_null
                  (call $caml_named_value
                     (string.const "Effect.Continuation_already_resumed"))))))
      (call $capture_continuation
         (ref.func $do_resume)
          (struct.new $pair
             (local.get $stack)
             (struct.new $pair (local.get $f) (local.get $v)))))

   ;; Perform

   (type $call_handler_env
      (sub final $closure
         (struct
            (field (ref $function_1))
            (field $handler (ref eq))
            (field $eff (ref eq))
            (field $cont (ref eq)))))

   (func $call_effect_handler
      (param $k (ref eq)) (param $venv (ref eq)) (result (ref eq))
      (local $env (ref $call_handler_env))
      (local $handler (ref $closure_3))
      (local.set $env (ref.cast (ref $call_handler_env) (local.get $venv)))
      (return_call_ref $function_3
         (struct.get $call_handler_env $eff (local.get $env))
         (struct.get $call_handler_env $cont (local.get $env))
         (local.get $k)
         (local.tee $handler
            (ref.cast (ref $closure_3)
              (struct.get $call_handler_env $handler (local.get $env))))
         (struct.get $closure_3 1 (local.get $handler))))

   (func $do_perform
      (param $k0 (ref $cont)) (param $vp (ref eq))
      (local $eff (ref eq)) (local $cont (ref $continuation))
      (local $handler (ref eq))
      (local $k1 (ref $cont))
      (local $p (ref $pair))
      (local.set $p (ref.cast (ref $pair) (local.get $vp)))
      (local.set $eff (struct.get $pair 0 (local.get $p)))
      (local.set $cont
         (ref.cast (ref $continuation) (struct.get $pair 1 (local.get $p))))
      (local.set $handler
         (array.get $handlers
            (struct.get $fiber $fiber_handlers (global.get $fiber_stack))
            (i32.const 2)))
      (struct.set $continuation 0
         (local.get $cont)
         (struct.new $fiber
             (struct.get $fiber $fiber_handlers
                (global.get $fiber_stack))
             (local.get $k0)
             (global.get $current_suspender)
             (struct.get $continuation 0 (local.get $cont))))
      (local.set $k1 (call $pop_fiber))
      (call_ref $cont_func
         (struct.new $pair
            (struct.new $call_handler_env
               (ref.func $call_effect_handler)
               (local.get $handler)
               (local.get $eff)
               (local.get $cont))
            (local.get $k1))
         (local.get $k1)
         (struct.get $cont $cont_func (local.get $k1))))

   (func $reperform (export "%reperform")
      (param $eff (ref eq)) (param $cont (ref eq))
      (result (ref eq))
      (call $capture_continuation
         (ref.func $do_perform)
         (struct.new $pair (local.get $eff) (local.get $cont))))

   (func (export "%perform") (param $eff (ref eq)) (result (ref eq))
      (return_call $reperform (local.get $eff)
         (struct.new $continuation (ref.null $fiber))))

   ;; Allocate a stack

   (func $call_handler (param $i i32) (param $x (ref eq))
      ;; Propagate a value or an exception to the parent fiber
      (local $f (ref eq))
      (local $cont (ref $cont))
      (local.set $f
         (array.get $handlers
            (struct.get $fiber $fiber_handlers (global.get $fiber_stack))
            (local.get $i)))
      (call_ref $cont_func (struct.new $pair (local.get $f) (local.get $x))
         (local.tee $cont (call $pop_fiber))
         (struct.get $cont $cont_func (local.get $cont))))

   (func (export "caml_start_fiber")
      (param $suspender externref) (param $p eqref)
      ;; Start executing some code in a new fiber
      (local $exn (ref eq))
      (local $res (ref eq))
      (global.set $current_suspender (local.get $suspender))
      (local.set $res
         (try (result (ref eq))
            (do
               (try (result (ref eq))
                  (do
                     (call $apply_pair (ref.cast (ref $pair) (local.get $p))))
                  (catch $javascript_exception
                     (throw $ocaml_exception
                        (call $caml_wrap_exception (pop externref))))))
            (catch $ocaml_exception
               (local.set $exn (pop (ref eq)))
               (call $call_handler (i32.const 1) (local.get $exn))
               (return))))
      (call $call_handler (i32.const 0) (local.get $res)))

   (func $initial_cont (param $p (ref $pair)) (param (ref eq))
      (call $start_fiber (local.get $p)))

(;ZZZZZZZZ
   (func (export "caml_alloc_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (result (ref eq))
      (struct.new $fiber
         (array.new_fixed $handlers 3
            (local.get $hv) (local.get $hx) (local.get $hf))
         (struct.new $cont (ref.func $initial_cont))
         (ref.null extern)
         (ref.null $fiber)))
;)

   ;; Other functions

(;ZZZZZZZZZZZZ
   (func $caml_continuation_use_noexc (export "caml_continuation_use_noexc")
      (param (ref eq)) (result (ref eq))
      (local $cont (ref $continuation))
      (local $stack (ref $fiber))
      (block $used
         (local.set $cont (ref.cast (ref $continuation) (local.get 0)))
         (local.set $stack
            (br_on_null $used (struct.get $continuation 0 (local.get $cont))))
         (struct.set $continuation 0 (local.get $cont) (ref.null $fiber))
         (return (local.get $stack)))
      (i31.new (i32.const 0)))

   (func (export "caml_continuation_use_and_update_handler_noexc")
      (param $cont (ref eq)) (param $hval (ref eq)) (param $hexn (ref eq))
      (param $heff (ref eq)) (result (ref eq))
      (local $stack (ref $fiber))
      (local.set $stack
         (ref.cast (ref $fiber)
            (call $caml_continuation_use_noexc (local.get $cont))))
      (block $used
         (struct.set $fiber $fiber_handlers
            (br_on_null $used (local.get $stack))
            (array.new_fixed $handlers 3
               (local.get $hval) (local.get $hexn) (local.get $heff))))
      (local.get $stack))
;)

   (func (export $caml_get_continuation_callstack)
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block 1 (i31.new (i32.const 0))))

   ;;ZZZZZZZZZZ
   (func (export "caml_is_continuation") (param (ref eq)) (result i32)
      (ref.test (ref $continuation) (local.get 0)))

   (func (export "caml_initialize_effects") (param $s externref)
      (global.set $current_suspender (local.get $s)))

;;;;;;;;;;;;;;;;;;;

   (type $exn_stack
      (struct (field $h (ref eq)) (field $next (ref null $exn_stack))))

   (global $caml_exn_stack (mut (ref null $exn_stack)) (ref.null $exn_stack))

   (func (export "caml_push_trap") (param $h (ref eq)) (result (ref eq))
      (global.set $caml_exn_stack
         (struct.new $exn_stack (local.get $h) (global.get $caml_exn_stack)))
      (i31.new (i32.const 0)))

   (func $raise_exception
      (param $exn (ref eq)) (param (ref eq)) (result (ref eq))
      (throw $ocaml_exception (local.get $exn)))

   (global $raise_exception (ref eq)
      (struct.new $closure (ref.func $raise_exception)))

   (func (export "caml_pop_trap") (result (ref eq))
      (local $top (ref $exn_stack))
      (block $empty
         (local.set $top (br_on_null $empty (global.get $caml_exn_stack)))
         (global.set $caml_exn_stack
            (struct.get $exn_stack $next (local.get $top)))
         (return (struct.get $exn_stack $h (local.get $top))))
      (global.get $raise_exception))


   (func (export "caml_maybe_attach_backtrace")
      (param $exn (ref eq)) (param (ref eq)) (result (ref eq))
      (local.get $exn))

   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $cps_closure (sub (struct (field (ref $function_2)))))
   (type $cps_closure_0 (sub (struct (field (ref $function_1)))))

   (type $iterator
     (sub $closure
       (struct
          (field (ref $function_1))
          (field $i (mut i32))
          (field $args (ref $block)))))

   (func $identity (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local.get 0))

   (global $identity (ref $closure) (struct.new $closure (ref.func $identity)))

   (func $trampoline_iterator
      (param $f (ref eq)) (param $venv (ref eq)) (result (ref eq))
      (local $env (ref $iterator))
      (local $i i32) (local $args (ref $block))
      (local.set $env (ref.cast (ref $iterator) (local.get $venv)))
      (local.set $i (struct.get $iterator $i (local.get $env)))
      (local.set $args (struct.get $iterator $args (local.get $env)))
      (struct.set $iterator $i (local.get $env)
         (i32.add (local.get $i) (i32.const 1)))
      (return_call_ref $function_2
         (array.get $block (local.get $args) (local.get $i))
         (if (result (ref eq))
             (i32.eq (i32.add (local.get $i) (i32.const 1))
                (array.len (local.get $args)))
            (then (global.get $identity))
            (else (local.get $env)))
         (local.get $f)
         (struct.get $cps_closure 0
            (ref.cast (ref $cps_closure) (local.get $f)))))

   (func $apply_iterator
      (param $f (ref eq)) (param $venv (ref eq)) (result (ref eq))
      (local $env (ref $iterator))
      (local $i i32) (local $args (ref $block))
      (local.set $env (ref.cast (ref $iterator) (local.get $venv)))
      (local.set $i (struct.get $iterator $i (local.get $env)))
      (local.set $args (struct.get $iterator $args (local.get $env)))
      (struct.set $iterator $i (local.get $env)
         (i32.add (local.get $i) (i32.const 1)))
      (return_call_ref $function_2
         (array.get $block (local.get $args) (local.get $i))
         (if (result (ref eq))
             (i32.eq (i32.add (local.get $i) (i32.const 2))
                (array.len (local.get $args)))
            (then
               (array.get $block (local.get $args)
                  (i32.add (local.get $i) (i32.const 1))))
            (else
               (local.get $env)))
         (local.get $f)
         (struct.get $cps_closure 0
            (ref.cast (ref $cps_closure) (local.get $f)))))

   (func (export "caml_apply_continuation")
      (param $args (ref eq)) (result (ref eq))
      (struct.new $iterator
         (ref.func $apply_iterator)
         (i32.const 1)
         (ref.cast (ref $block) (local.get $args))))

   ;; ZZZ initialize effect handlers
   ;; ZZZ js exceptions
   ;; ZZZ use in caml_callback
   (func (export "caml_trampoline")
      (param $f (ref eq)) (param $vargs (ref eq)) (result (ref eq))
      (local $args (ref $block))
      (local $i i32)
      (local $exn (ref eq)) (local $top (ref $exn_stack))
      (local.set $args (ref.cast (ref $block) (local.get $vargs)))
      (try (result (ref eq))
         (do
            (if (i32.eq (array.len $block (local.get $args)) (i32.const 1))
               (then
                  (return ;; NOT a tail call
                     (call_ref $function_1 (global.get $identity)
                        (local.get $f)
                        (struct.get $cps_closure_0 0
                           (ref.cast (ref $cps_closure_0) (local.get $f)))))))
            (return ;; NOT a tail call
               (call_ref $function_2
                  (array.get $block (local.get $args) (i32.const 1))
                  (if (result (ref eq))
                      (i32.eq (i32.const 2) (array.len (local.get $args)))
                     (then (global.get $identity))
                     (else
                        (struct.new $iterator
                           (ref.func $trampoline_iterator)
                           (i32.const 2)
                           (local.get $args))))
                  (local.get $f)
                  (struct.get $cps_closure 0
                     (ref.cast (ref $cps_closure) (local.get $f))))))
         (catch $ocaml_exception
            (local.set $exn (pop (ref eq)))
            (loop $loop
               (block $empty
                  (local.set $top
                     (br_on_null $empty (global.get $caml_exn_stack)))
                  (global.set $caml_exn_stack
                     (struct.get $exn_stack $next (local.get $top)))
                  (local.set $f (struct.get $exn_stack $h (local.get $top)))
                  (try
                     (do
                        (return ;; NOT a tail call
                           (call_ref $function_1
                              (local.get $exn)
                              (local.get $f)
                              (struct.get $closure 0
                                 (ref.cast (ref $closure) (local.get $f))))))
                     (catch $ocaml_exception
                        (local.set $exn (pop (ref eq)))
                        (br $loop))))
               (throw $ocaml_exception (local.get $exn))))))

   (type $cps_handlers
      (struct
         (field $value (ref eq))
         (field $exn (ref eq))
         (field $effect (ref eq))))

   (type $cps_fiber
      (struct
         (field $handlers (mut (ref $cps_handlers)))
         (field $cont (ref eq))
         (field $exn_stack (ref null $exn_stack))
         (field $next (ref null $cps_fiber))))

   (global $cps_fiber_stack (mut (ref null $cps_fiber)) (ref.null $cps_fiber))

   (type $function_4
      (func (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq))
         (result (ref eq))))
   (type $cps_closure_3
      (sub $cps_closure
         (struct (field (ref $function_2)) (field (ref $function_4)))))

   (func $caml_pop_fiber (result (ref eq))
      (local $top (ref $cps_fiber))
      (local.set $top (ref.as_non_null (global.get $cps_fiber_stack)))
      (global.set $cps_fiber_stack
         (struct.get $cps_fiber $next (local.get $top)))
      (global.set $caml_exn_stack
         (struct.get $cps_fiber $exn_stack (local.get $top)))
      (struct.get $cps_fiber $cont (local.get $top)))

   (import "obj" "cont_tag" (global $cont_tag i32))

   (func (export "caml_resume_stack")
      (param $vstack (ref eq)) (param $k (ref eq)) (result (ref eq))
      (local $stack (ref $cps_fiber))
      (drop (block $already_resumed (result (ref eq))
         (local.set $stack
            (br_on_cast_fail $already_resumed (ref eq) (ref $cps_fiber)
               (local.get $vstack)))
         (block $done
            (loop $loop
               (global.set $cps_fiber_stack
                  (struct.new $cps_fiber
                     (struct.get $cps_fiber $handlers (local.get $stack))
                     (local.get $k)
                     (global.get $caml_exn_stack)
                     (global.get $cps_fiber_stack)))
               (local.set $k (struct.get $cps_fiber $cont (local.get $stack)))
               (global.set $caml_exn_stack
                  (struct.get $cps_fiber $exn_stack (local.get $stack)))
               (local.set $stack
                  (br_on_null $done
                     (struct.get $cps_fiber $next (local.get $stack))))
               (br $loop)))
         (return (local.get $k))))
      (call $caml_raise_constant
         (ref.as_non_null
            (call $caml_named_value
               (string.const "Effect.Continuation_already_resumed"))))
      (i31.new (i32.const 0)))

   (func (export "caml_perform_effect")
      (param $eff (ref eq)) (param $vcont (ref eq)) (param $k0 (ref eq))
      (result (ref eq))
      (local $handlers (ref $cps_handlers))
      (local $handler (ref eq)) (local $k1 (ref eq))
      (local $cont (ref $block))
      (local.set $cont
         (block $reperform (result (ref $block))
            (drop
               (br_on_cast $reperform (ref eq) (ref $block) (local.get $vcont)))
            (array.new_fixed $block 2 (i31.new (global.get $cont_tag))
               (i31.new (i32.const 0)))))
      (local.set $handlers
         (struct.get $cps_fiber $handlers
            (ref.as_non_null (global.get $cps_fiber_stack))))
      (local.set $handler
         (struct.get $cps_handlers $effect (local.get $handlers)))
      (array.set $block (local.get $cont) (i32.const 1)
         (struct.new $cps_fiber
            (local.get $handlers)
            (local.get $k0)
            (global.get $caml_exn_stack)
            (if (result (ref null $cps_fiber))
                (ref.test (ref $block) (local.get $vcont))
               (then
                  (ref.cast (ref $cps_fiber)
                     (array.get $block (local.get $cont) (i32.const 1))))
               (else
                  (ref.null $cps_fiber)))))
      (local.set $k1 (call $caml_pop_fiber))
      (call_ref $function_4
         (local.get $eff) (local.get $cont) (local.get $k1) (local.get $k1)
         (local.get $handler)
         (struct.get $cps_closure_3 1
            (ref.cast (ref $cps_closure_3) (local.get $handler)))))

   (func $cps_call_handler
      (param $handler (ref eq)) (param $x (ref eq)) (result (ref eq))
      (return_call_ref $function_2
         (local.get $x)
         (call $caml_pop_fiber)
         (local.get $handler)
         (struct.get $cps_closure 0
            (ref.cast $cps_closure (local.get $handler)))))

   (func $value_handler (param $x (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $cps_call_handler
          (struct.get $cps_handlers $value
             (struct.get $cps_fiber $handlers
                (ref.as_non_null (global.get $cps_fiber_stack))))
          (local.get $x)))

   (global $value_handler (ref $closure)
      (struct.new $closure (ref.func $value_handler)))

   (func $exn_handler (param $x (ref eq)) (param (ref eq)) (result (ref eq))
      (return_call $cps_call_handler
          (struct.get $cps_handlers $exn
             (struct.get $cps_fiber $handlers
                (ref.as_non_null (global.get $cps_fiber_stack))))
          (local.get $x)))

   (global $exn_handler (ref $closure)
      (struct.new $closure (ref.func $exn_handler)))

   (func (export "caml_alloc_stack")
      (param $hv (ref eq)) (param $hx (ref eq)) (param $hf (ref eq))
      (result (ref eq))
      (struct.new $cps_fiber
         (struct.new $cps_handlers
            (local.get $hv) (local.get $hx) (local.get $hf))
         (global.get $value_handler)
         (struct.new $exn_stack
            (global.get $exn_handler) (ref.null $exn_stack))
         (ref.null $cps_fiber)))

   (func $caml_continuation_use_noexc (export "caml_continuation_use_noexc")
      (param $vcont (ref eq)) (result (ref eq))
      (local $cont (ref $block)) (local $stack (ref eq))
      (local.set $cont (ref.cast (ref $block) (local.get $vcont)))
      (local.set $stack (array.get $block (local.get $cont) (i32.const 1)))
      (array.set $block (local.get $cont) (i32.const 1) (i31.new (i32.const 0)))
      (local.get $stack))

   (func (export "caml_continuation_use_and_update_handler_noexc")
      (param $cont (ref eq)) (param $hval (ref eq)) (param $hexn (ref eq))
      (param $heff (ref eq)) (result (ref eq))
      (local $stack (ref $cps_fiber))
      (local.set $stack
         (ref.cast (ref $cps_fiber)
            (call $caml_continuation_use_noexc (local.get $cont))))
      (struct.set $cps_fiber $handlers (local.get $stack)
         (struct.new $cps_handlers
            (local.get $hval) (local.get $hexn) (local.get $heff)))
      (local.get $stack))
)

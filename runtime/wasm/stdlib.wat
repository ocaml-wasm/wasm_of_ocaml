(module
   (import "hash" "caml_string_hash"
      (func $caml_string_hash
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_equal"
      (func $caml_string_equal
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (type $assoc
      (struct (field (ref $string)) (field (ref eq)) (field (ref null $assoc))))

   (type $assoc_array (array (field (mut (ref null $assoc)))))

   (global $Named_value_size i32 (i32.const 13))

   (global $named_value_table (ref $assoc_array)
     (array.new $assoc_array (ref.null $assoc) (global.get $Named_value_size)))

   (func $find_named_value
      (param $s (ref eq)) (param $l (ref null $assoc)) (result (ref null eq))
      (local $a (ref $assoc))
      (block $tail (result (ref null eq))
         (loop $loop
            (local.set $a
               (br_on_cast_fail $tail (ref null eq) (ref $assoc) (local.get $l)))
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_string_equal
                          (local.get $s)
                          (struct.get $assoc 0 (local.get $a)))))
               (then
                  (return (struct.get $assoc 1 (local.get $a)))))
            (local.set $l (struct.get $assoc 2 (local.get $a)))
            (br $loop))))

   (func (export "caml_named_value") (param anyref) (result (ref null eq))
      (local $s (ref eq))
      (local.set $s
         (block $convert (result (ref eq))
            (call $caml_string_of_jsstring
               (call $wrap
                  (br_on_cast $convert anyref (ref $string) (local.get 0))))))
      (return_call $find_named_value
         (local.get $s)
         (array.get $assoc_array (global.get $named_value_table)
            (i32.rem_u
               (i31.get_s
                  (ref.cast (ref i31)
                     (call $caml_string_hash
                        (ref.i31 (i32.const 0)) (local.get $s))))
               (global.get $Named_value_size)))))

   (func (export "caml_register_named_value")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $h i32)
      (local $r (ref null $assoc))
      (local.set $h
         (i32.rem_u
            (i31.get_s
               (ref.cast (ref i31)
                  (call $caml_string_hash
                     (ref.i31 (i32.const 0)) (local.get 0))))
            (global.get $Named_value_size)))
      (local.set $r
         (array.get $assoc_array
            (global.get $named_value_table) (local.get $h)))
      (if (ref.is_null (call $find_named_value (local.get 0) (local.get $r)))
         (then
            (array.set $assoc_array
               (global.get $named_value_table) (local.get $h)
               (struct.new $assoc
                  (ref.cast (ref $string) (local.get 0))
                  (local.get 1) (local.get $r)))))
      (ref.i31 (i32.const 0)))

   (global $caml_global_data (export "caml_global_data") (mut (ref $block))
      (array.new $block (ref.i31 (i32.const 0)) (i32.const 12)))

   (func (export "caml_register_global")
      (param (ref eq)) (param $v (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (i31.get_u (ref.cast (ref i31) (local.get 0))))
      (if (i32.lt_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (array.set $block (global.get $caml_global_data)
               (local.get $i) (local.get $v))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_get_global_data") (param (ref eq)) (result (ref eq))
      (global.get $caml_global_data))

   (import "sys" "ocaml_exit" (tag $ocaml_exit (param i32)))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "bindings" "exit" (func $exit (param i32)))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param anyref) (result (ref null eq))))

   (type $func (func))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub open (struct (field (ref $function_1)))))
   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))
   (type $closure_2
      (sub open $closure
         (struct (field (ref $function_1)) (field (ref $function_2)))))

   (func (export "caml_main") (param $start (ref $func))
      (local $exn (ref eq))
      (local $handle_uncaught_exception (ref eq)) (local $do_at_exit (ref eq))
      (try
         (do
            (call_ref $func (local.get $start)))
         (catch $ocaml_exit
            (call $exit (pop i32)))
         (catch $ocaml_exception
            (local.set $exn (pop (ref eq)))
            (block $exit
               (block $not_registered
                  (local.set $handle_uncaught_exception
                     (br_on_null $not_registered
                        (call $caml_named_value
                           (string.const "Printexc.handle_uncaught_exception"))))
                  ;; ZZZ CPS
                  (drop (call_ref $function_2
                     (local.get $exn) (ref.i31 (i32.const 0))
                     (local.get $handle_uncaught_exception)
                     (struct.get $closure_2 1
                        (ref.cast (ref $closure_2)
                           (local.get $handle_uncaught_exception)))))
                  (br $exit))
               (block $null
                  (local.set $do_at_exit
                     (br_on_null $null
                        (call $caml_named_value
                           (string.const "Pervasives.do_at_exit"))))
                  (drop (call_ref $function_1
                     (ref.i31 (i32.const 0))
                     (local.get $do_at_exit)
                     (struct.get $closure 0
                        (ref.cast (ref $closure) (local.get $do_at_exit))))))
(;
                console.error (
                    "Fatal error: exception " +
                        wasmModule.instance.exports.caml_format_exception(exn) +
                        "\n")
;)
)
(call $exit (i32.const 2)))))

)

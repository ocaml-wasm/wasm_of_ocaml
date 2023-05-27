(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "hash" "caml_string_hash"
      (func $caml_string_hash
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))

   (type $assoc
      (struct (field (ref $string)) (field (ref eq)) (field (ref null $assoc))))

   (type $assoc_array (array (field (mut (ref null $assoc)))))

   (global $named_value_table (ref $assoc_array)
     (array.new $assoc_array (ref.null $assoc) (i32.const 13)))

   (func (export "caml_register_named_value")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $h i32)
      (local.set $h
         (i31.get_s
            (ref.cast i31
               (call $caml_string_hash (i31.new (i32.const 0)) (local.get $0)))))
      ;; ZZZ
      (call $log_js (string.const "caml_register_named_value"))
      (call $log_js
         (call $unwrap (call $caml_jsstring_of_string (local.get $0))))
      (i31.new (i32.const 0)))

   (global $caml_global_data (export "caml_global_data") (mut (ref $block))
      (array.new $block (i31.new (i32.const 0)) (i32.const 12)))

   (func (export "caml_register_global")
      (param (ref eq)) (param $v (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (i31.get_u (ref.cast i31 (local.get 0))))
      (if (i32.lt_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (array.set $block (global.get $caml_global_data)
               (local.get $i) (local.get $v))))
      (i31.new (i32.const 0)))
)

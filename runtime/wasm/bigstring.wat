(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_to_typed_array"
     (func $caml_ba_to_typed_array (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_ba_from_typed_array"
      (func $caml_ba_from_typed_array (param (ref eq)) (result (ref eq))))
   (import "bindings" "ta_create"
      (func $ta_create (param i32) (param anyref) (result anyref)))

   (type $string (array (mut i8)))
   (type $value->value->int->int
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $value->int
      (func (param (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field $cust_id (ref $string))
         (field $cust_compare (ref null $value->value->int->int))
         (field $cust_compare_ext (ref null $value->value->int->int))
         (field $cust_hash (ref null $value->int))
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))
   (type $int_array (array (mut i32)))
   (type $bigarray
      (sub $custom
         (struct
            (field $ba_ops (ref $custom_operations))
            (field $ba_data (mut (ref extern))) ;; data
            (field $ba_dim (ref $int_array)) ;; size in each dimension
            (field $ba_num_dims i8) ;; number of dimensions
            (field $ba_kind i8) ;; kind
            (field $ba_layout i8)))) ;; layout

   (func (export "caml_hash_mix_bigstring")
      (param i32) (param (ref $bigarray)) (result i32)
      ;; ZZZ
      (call $log_js (string.const "caml_hash_mix_bigstring"))
      (i32.const 0))

   (func (export "bigstring_to_array_buffer")
      (param $bs (ref eq)) (result (ref eq))
      (return_call $caml_js_get
         (call $caml_ba_to_typed_array (local.get $bs))
         (call $wrap (string.const "buffer"))))

   (export "bigstring_to_typed_array" (func $caml_ba_to_typed_array))

   (func (export "bigstring_of_array_buffer") (param (ref eq)) (result (ref eq))
       (return_call $caml_ba_from_typed_array
          (call $wrap (call $ta_create (i32.const 12) (local.get $0)))))

   (export "bigstring_of_typed_array" (func $caml_ba_from_typed_array))

   (func (export "caml_bigstring_memcmp")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_memcmp"))
      (i31.new (i32.const 0)))

   (export "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_bytes_to_ba))
   (func $caml_bigstring_blit_bytes_to_ba
      (export "caml_bigstring_blit_bytes_to_ba")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_blit_bytes_to_ba"))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_bytes")
     (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_blit_ba_to_bytes"))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_ba")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_blit_ba_to_ba"))
      (i31.new (i32.const 0)))
)

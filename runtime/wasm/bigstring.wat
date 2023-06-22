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
   (import "bindings" "ta_get_ui8"
      (func $ta_get_ui8 (param (ref extern)) (param i32) (result i32)))
   (import "bindings" "ta_set_ui8"
      (func $ta_set_ui8 (param (ref extern)) (param i32) (param (ref i31))))
   (import "bindings" "ta_subarray"
      (func $ta_subarray
         (param (ref extern)) (param i32) (param i32) (result (ref extern))))
   (import "bindings" "ta_set"
      (func $ta_set (param (ref extern)) (param (ref extern)) (param i32)))

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
      (param $s1 (ref eq)) (param $vpos1 (ref eq))
      (param $s2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $c1 i32) (local $c2 i32)
      (local $d1 (ref extern))
      (local $d2 (ref extern))
      (local.set $d1
         (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get $s1))))
      (local.set $pos1 (i31.get_s (ref.cast i31 (local.get $vpos1))))
      (local.set $d2
         (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get $s2))))
      (local.set $pos2 (i31.get_s (ref.cast i31 (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (call $ta_get_ui8 (local.get $d1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $c2
                  (call $ta_get_ui8 (local.get $d2)
                     (i32.add (local.get $pos2) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.eq (local.get $c1) (local.get $c2)))
               (return
                  (select (i31.new (i32.const -1)) (i31.new (i32.const 1))
                     (i32.lt_u (local.get $c1) (local.get $c2)))))))
      (i31.new (i32.const 0)))

   (export "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_bytes_to_ba))
   (func $caml_bigstring_blit_bytes_to_ba
      (export "caml_bigstring_blit_bytes_to_ba")
      (param $str1 (ref eq)) (param $vpos1 (ref eq))
      (param $ba2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $s1 (ref $string))
      (local $d2 (ref extern))
      (local.set $s1 (ref.cast $string (local.get $str1)))
      (local.set $pos1 (i31.get_s (ref.cast i31 (local.get $vpos1))))
      (local.set $d2
         (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get $ba2))))
      (local.set $pos2 (i31.get_s (ref.cast i31 (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (call $ta_set_ui8 (local.get $d2)
                  (i32.add (local.get $pos2) (local.get $i))
                  (i31.new
                     (array.get_u $string (local.get $s1)
                        (i32.add (local.get $pos1) (local.get $i)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_bytes")
      (param $ba1 (ref eq)) (param $vpos1 (ref eq))
      (param $str2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $i i32) (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $d1 (ref extern))
      (local $s2 (ref $string))
      (local.set $d1
         (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get $ba1))))
      (local.set $pos1 (i31.get_s (ref.cast i31 (local.get $vpos1))))
      (local.set $s2 (ref.cast $string (local.get $str2)))
      (local.set $pos2 (i31.get_s (ref.cast i31 (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s2)
                  (i32.add (local.get $pos2) (local.get $i))
                  (call $ta_get_ui8 (local.get $d1)
                     (i32.add (local.get $pos1) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_ba")
      (param $ba1 (ref eq)) (param $vpos1 (ref eq))
      (param $ba2 (ref eq)) (param $vpos2 (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos1 i32) (local $pos2 i32) (local $len i32)
      (local $d1 (ref extern))
      (local $d2 (ref extern))
      (local.set $d1
         (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get $ba1))))
      (local.set $pos1 (i31.get_s (ref.cast i31 (local.get $vpos1))))
      (local.set $d2
         (struct.get $bigarray $ba_data (ref.cast $bigarray (local.get $ba2))))
      (local.set $pos2 (i31.get_s (ref.cast i31 (local.get $vpos2))))
      (local.set $len (i31.get_s (ref.cast i31 (local.get $vlen))))
      (call $ta_set (local.get $d2)
         (call $ta_subarray (local.get $d1)
            (local.get $pos1) (i32.add (local.get $pos1) (local.get $len)))
         (local.get $pos2))
      (i31.new (i32.const 0)))
)

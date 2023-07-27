(module
;; ml_z_of_substring_base
;; ml_z_init
;; ml_z_format
   (import "js" "bigint"
      (func $bigint (param (ref string)) (result (ref extern))))
   (import "js" "bigint_to_string"
      (func $bigint_to_string (param (ref extern)) (result anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))

   (func (export "ml_z_init") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (type $string (array (mut i8)))

(;
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
;)

   (type $bigint
      (sub final (; $custom ;)
         (struct
;;            (field (ref $custom_operations))
            (field $v (mut (ref extern))))))

   (func (export "ml_z_of_substring_base")
      (param $base (ref eq)) (param $s (ref eq)) (param $vpos (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (local $pos i32)
      (local.set $pos (i31.get_s (ref.cast i31 (local.get $vpos))))
      (struct.new $bigint
         (call $bigint
            (string.new_wtf8_array replace
               (ref.cast $string (local.get $s))
               (local.get $pos)
               (i32.add (local.get $pos)
                  (i31.get_s (ref.cast i31 (local.get $len))))))))

   (func (export "ml_z_format")
      (param $fmt (ref eq)) (param $z (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring
         (call $wrap
            (call $bigint_to_string
               (struct.get $bigint $v (ref.cast $bigint (local.get $z)))))))
)

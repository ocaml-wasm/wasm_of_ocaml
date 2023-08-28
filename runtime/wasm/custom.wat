(module
   (type $string (array (mut i8)))
   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $custom_operations
      (struct
         (field $id (ref $string))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))

   (type $custom_with_id
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $id i64))))

   (func (export "caml_is_custom") (param (ref eq)) (result i32)
      (ref.test (ref $custom) (local.get 0)))

   (func (export "custom_compare_id")
      (param (ref eq)) (param (ref eq)) (param i32) (result i32)
      (local $i1 i64) (local $i2 i64)
      (local.set $i1
         (struct.get $custom_with_id $id
            (ref.cast (ref $custom_with_id) (local.get 0))))
      (local.set $i2
         (struct.get $custom_with_id $id
            (ref.cast (ref $custom_with_id) (local.get 1))))
      (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
               (i64.lt_s (local.get $i1) (local.get $i2))))

   (func (export "custom_hash_id") (param (ref eq)) (result i32)
      (i32.wrap_i64
         (struct.get $custom_with_id $id
           (ref.cast (ref $custom_with_id) (local.get 0)))))

   (global $next_id (mut i64) (i64.const 0))

   (func (export "custom_next_id") (result i64)
      (local $id i64)
      (local.set $id (global.get $next_id))
      (global.set $next_id (i64.add (local.get $id) (i64.const 1)))
      (local.get $id))
)

(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "hash" "caml_hash_mix_string"
      (func $caml_hash_mix_string
         (param i32) (param (ref $string)) (result i32)))
   (import "hash" "caml_hash_mix_final"
      (func $caml_hash_mix_final (param i32) (result i32)))
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref $int64))))

   (type $string (array (mut i8)))
   (type $value->value->int
      (func (param (ref eq)) (param (ref eq)) (result i32)))
   (type $value->int
      (func (param (ref eq)) (result i32)))
   (type $custom_operations
      (struct
         (field (ref $string)) ;; identifier
         (field (ref $value->value->int)) ;; compare
         (field (ref null $value->int)) ;; hash
         ;; ZZZ
      ))
   (type $custom (struct (field (ref $custom_operations))))
   (type $int64
      (sub $custom (struct (field (ref $custom_operations)) (field i64))))

   (func (export "Base_internalhash_fold_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new
         (call $caml_hash_mix_int
            (i31.get_s (ref.cast i31 (local.get 0)))
            (i31.get_s (ref.cast i31 (local.get 1))))))

   (func (export "Base_internalhash_fold_string")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new
         (call $caml_hash_mix_string
            (i31.get_s (ref.cast i31 (local.get 0)))
            (ref.cast $string (local.get 1)))))

   (func (export "Base_internalhash_get_hash_value")
      (param (ref eq)) (result (ref eq))
      (i31.new
         (call $caml_hash_mix_final (i31.get_s (ref.cast i31 (local.get 0))))))

   (func (export "Base_clear_caml_backtrace_pos")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "Base_clear_caml_backtrace_pos"))
      (i31.new (i32.const 0)))

   (func (export "Base_am_testing") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "Base_am_testing"))
      (i31.new (i32.const 1)))

   (func (export "time_now_nanoseconds_since_unix_epoch_or_zero")
      (param (ref eq)) (result (ref eq))
      (call $caml_copy_int64
         (i64.trunc_sat_f64_s (f64.mul (call $gettimeofday) (f64.const 2e9)))))

   (func (export "expect_test_collector_before_test")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "expect_test_collector_before_test"))
      (i31.new (i32.const 0)))

   (func (export "expect_test_collector_after_test")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "expect_test_collector_after_test"))
      (i31.new (i32.const 0)))

   (func (export "caml_out_channel_pos_fd") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_out_channel_pos_fd"))
      (i31.new (i32.const 0)))
)

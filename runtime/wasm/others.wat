(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "gettimeofday" (func $gettimeofday (result f64)))
   (import "hash" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "hash" "caml_hash_mix_int64"
      (func $caml_hash_mix_int64 (param i32) (param i64) (result i32)))
   (import "hash" "caml_hash_mix_float"
      (func $caml_hash_mix_float (param i32) (param f64) (result i32)))
   (import "hash" "caml_hash_mix_string"
      (func $caml_hash_mix_string
         (param i32) (param (ref $string)) (result i32)))
   (import "hash" "caml_hash_mix_final"
      (func $caml_hash_mix_final (param i32) (result i32)))
   (import "hash" "caml_hash"
      (func $caml_hash
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "int64" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref $int64))))

   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
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
   (type $int32
      (sub $custom (struct (field (ref $custom_operations)) (field i32))))

   ;;;;;; base

   (func (export "Base_int_math_int_popcount")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.popcnt (i31.get_s (ref.cast i31 (local.get 0))))))

   (func (export "Base_int_math_int_clz")
      (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.clz
            (i32.shl (i31.get_s (ref.cast i31 (local.get 0))) (i32.const 1)))))

   (export "Base_int_math_nativeint_clz" (func $Base_int_math_int32_clz))
   (func $Base_int_math_int32_clz (export "Base_int_math_int32_clz")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.clz (struct.get $int32 1 (ref.cast $int32 (local.get 0))))))

   (func $Base_int_math_int64_clz (export "Base_int_math_int64_clz")
      (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.wrap_i64
            (i64.clz (struct.get $int64 1 (ref.cast $int64 (local.get 0)))))))

   (func (export "Base_int_math_int_ctz")
      (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.ctz (i31.get_s (ref.cast i31 (local.get 0))) (i32.const 1))))

   (export "Base_int_math_nativeint_ctz" (func $Base_int_math_int32_ctz))
   (func $Base_int_math_int32_ctz (export "Base_int_math_int32_ctz")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.ctz (struct.get $int32 1 (ref.cast $int32 (local.get 0))))))

   (func $Base_int_math_int64_ctz (export "Base_int_math_int64_ctz")
      (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.wrap_i64
            (i64.ctz (struct.get $int64 1 (ref.cast $int64 (local.get 0)))))))

   (func $Base_int_math_int_pow_stub
      (param $vbase (ref eq)) (param $vexp (ref eq)) (result (ref eq))
      (local $base i32) (local $exp i32) (local $res i32)
      (local.set $base (i31.get_s (ref.cast i31 (local.get $vbase))))
      (local.set $exp (i31.get_s (ref.cast i31 (local.get $vexp))))
      (local.set $res (i32.const 1))
      (loop $loop
         (if (i32.ne (local.get $exp) (i32.const 0))
            (then
               (if (i32.and (local.get $exp) (i32.const 1))
                  (then
                     (local.set $res
                        (i32.mul (local.get $res) (local.get $base)))))
               (local.set $exp (i32.shr_u (local.get $exp) (i32.const 1)))
               (local.set $base (i32.mul (local.get $base) (local.get $base)))
               (br $loop))))
      (i31.new (local.get $res)))

   (func $Base_int_math_int64_pow_stub
      (param $vbase (ref eq)) (param $vexp (ref eq)) (result (ref eq))
      (local $base i64) (local $exp i64) (local $res i64)
      (local.set $base
         (struct.get $int64 1 (ref.cast $int64 (local.get $vbase))))
      (local.set $exp
         (struct.get $int64 1 (ref.cast $int64 (local.get $vexp))))
      (local.set $res (i64.const 1))
      (loop $loop
         (if (i64.ne (local.get $exp) (i64.const 0))
            (then
               (if (i32.wrap_i64 (i64.and (local.get $exp) (i64.const 1)))
                  (then
                     (local.set $res
                        (i64.mul (local.get $res) (local.get $base)))))
               (local.set $exp (i64.shr_u (local.get $exp) (i64.const 1)))
               (local.set $base (i64.mul (local.get $base) (local.get $base)))
               (br $loop))))
      (return_call $caml_copy_int64 (local.get $res)))

   (func (export "Base_clear_caml_backtrace_pos")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "Base_caml_exn_is_most_recent_exn")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 1)))

   (func (export "Base_internalhash_fold_int64")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new
         (call $caml_hash_mix_int64
            (i31.get_s (ref.cast i31 (local.get 0)))
            (struct.get $int64 1 (ref.cast $int64 (local.get 1))))))

   (func (export "Base_internalhash_fold_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new
         (call $caml_hash_mix_int
            (i31.get_s (ref.cast i31 (local.get 0)))
            (i31.get_s (ref.cast i31 (local.get 1))))))

   (func (export "Base_internalhash_fold_float")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new
         (call $caml_hash_mix_float
            (i31.get_s (ref.cast i31 (local.get 0)))
            (struct.get $float 0 (ref.cast $float (local.get 1))))))

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

   (func (export "Base_hash_string") (param $s (ref eq)) (result (ref eq))
      (return_call $caml_hash
         (i31.new (i32.const 1)) (i31.new (i32.const 1)) (i31.new (i32.const 0))
         (local.get $s)))

   (func (export "Base_hash_double") (param $d (ref eq)) (result (ref eq))
      (return_call $caml_hash
         (i31.new (i32.const 1)) (i31.new (i32.const 1)) (i31.new (i32.const 0))
         (local.get $d)))

   (func (export "Base_am_testing") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "Base_am_testing"))
      (i31.new (i32.const 1)))

   ;;;;;; time_now

   (func (export "time_now_nanoseconds_since_unix_epoch_or_zero")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_int64
         (i64.trunc_sat_f64_s (f64.mul (call $gettimeofday) (f64.const 2e9)))))

   ;;;;;; ppx_expect

   (type $offset_array (array (mut i64)))

   (import "env" "fd_offsets" (global $fd_offsets (mut (ref $offset_array))))

   (type $channel
      (struct
         (field $fd (mut i32))
         (field $buffer (mut (ref extern)))
         (field $curr (mut i32))
         (field $max (mut i32))
         (field $size (mut i32))
         (field $flags (mut i32)))) ;; flags

   (global $saved_stdout (mut i32) (i32.const 0))
   (global $saved_stderr (mut i32) (i32.const 0))

   (func (export "expect_test_collector_before_test")
      (param $voutput (ref eq)) (param $vstdout (ref eq))
      (param $vstderr (ref eq)) (result (ref eq))
      (local $output (ref $channel))
      (local $stdout (ref $channel))
      (local $stderr (ref $channel))
      (local $fd i32)
      (local.set $output (ref.cast $channel (local.get $voutput)))
      (local.set $stdout (ref.cast $channel (local.get $vstdout)))
      (local.set $stderr (ref.cast $channel (local.get $vstderr)))
      (global.set $saved_stdout (struct.get $channel $fd (local.get $stdout)))
      (global.set $saved_stderr (struct.get $channel $fd (local.get $stderr)))
      (local.set $fd (struct.get $channel $fd (local.get $output)))
      (struct.set $channel $fd (local.get $stdout) (local.get $fd))
      (struct.set $channel $fd (local.get $stderr) (local.get $fd))
      (i31.new (i32.const 0)))

   (func (export "expect_test_collector_after_test")
      (param $vstdout (ref eq)) (param $vstderr (ref eq)) (result (ref eq))
      (local $stdout (ref $channel))
      (local $stderr (ref $channel))
      (local.set $stdout (ref.cast $channel (local.get $vstdout)))
      (local.set $stderr (ref.cast $channel (local.get $vstderr)))
      (struct.set $channel $fd (local.get $stdout) (global.get $saved_stdout))
      (struct.set $channel $fd (local.get $stderr) (global.get $saved_stderr))
      (i31.new (i32.const 0)))

   (func (export "caml_out_channel_pos_fd") (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.wrap_i64
            (array.get $offset_array (global.get $fd_offsets)
               (struct.get $channel $fd (ref.cast $channel (local.get 0)))))))

   ;;;; compiler/test-jsoo

   (func (export "flush_stdout_stderr") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)

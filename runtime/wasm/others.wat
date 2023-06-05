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

   ;;;;;; base

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

   ;;;;;; time_now

   (func (export "time_now_nanoseconds_since_unix_epoch_or_zero")
      (param (ref eq)) (result (ref eq))
      (call $caml_copy_int64
         (i64.trunc_sat_f64_s (f64.mul (call $gettimeofday) (f64.const 2e9)))))

   ;;;;;; ppx_expect

   (type $channel
      (struct
         (field (mut i32)) ;; fd
         (field (mut i64)) ;; offset
         (field (mut (ref extern))) ;; buffer
         (field (mut i32)) ;; current position in buffer
         (field (mut i32)) ;; logical end of the buffer (for input)
         (field (mut i32)) ;; buffer size
         (field (mut i32)))) ;; flags

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
      (global.set $saved_stdout (struct.get $channel 0 (local.get $stdout)))
      (global.set $saved_stderr (struct.get $channel 0 (local.get $stderr)))
      (local.set $fd (struct.get $channel 0 (local.get $output)))
      (struct.set $channel 0 (local.get $stdout) (local.get $fd))
      (struct.set $channel 0 (local.get $stderr) (local.get $fd))
      (i31.new (i32.const 0)))

   (func (export "expect_test_collector_after_test")
      (param $vstdout (ref eq)) (param $vstderr (ref eq)) (result (ref eq))
      (local $stdout (ref $channel))
      (local $stderr (ref $channel))
      (local.set $stdout (ref.cast $channel (local.get $vstdout)))
      (local.set $stderr (ref.cast $channel (local.get $vstderr)))
      (struct.set $channel 0 (local.get $stdout) (global.get $saved_stdout))
      (struct.set $channel 0 (local.get $stderr) (global.get $saved_stderr))
      (i31.new (i32.const 0)))

   (func (export "caml_out_channel_pos_fd") (param (ref eq)) (result (ref eq))
      (i31.new
         (i32.wrap_i64
            (struct.get $channel 1 (ref.cast $channel (local.get $0))))))
)

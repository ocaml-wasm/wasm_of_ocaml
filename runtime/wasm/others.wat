(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

   ;;;;;; bin_prot

   (import "fail" "caml_array_bound_error" (func $caml_array_bound_error))
   (import "bigstring" "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_string_to_ba
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "bigstring" "caml_bigstring_blit_ba_to_bytes"
      (func $caml_bigstring_blit_ba_to_bytes
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_get_dim"
      (func $caml_ba_get_dim (param (ref eq)) (result (ref $int_array))))

   (type $int_array (array (mut i32)))

   (func (export "caml_check_bound_bigstring")
      (param $bigstring (ref eq)) (param $i (ref eq)) (result (ref eq))
      (if (i32.ge_u (i31.get_s (ref.cast i31 (local.get $i)))
             (array.get $int_array
                (call $caml_ba_get_dim (local.get $bigstring))
                (i32.const 0)))
         (then (call $caml_array_bound_error)))
      (i31.new (i32.const 0)))

   (func (export "bin_prot_blit_buf_float_array_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bin_prot_blit_buf_float_array_stub"))
      (i31.new (i32.const 0)))

   (func (export "bin_prot_blit_buf_bytes_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (return_call $caml_bigstring_blit_ba_to_bytes
         (local.get $src) (local.get $src_pos)
         (local.get $dst) (local.get $dst_pos)
         (local.get $len)))

   (func (export "bin_prot_blit_float_array_buf_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bin_prot_blit_float_array_buf_stub"))
      (i31.new (i32.const 0)))

   (export "bin_prot_blit_bytes_buf_stub" (func $bin_prot_blit_string_buf_stub))
   (func $bin_prot_blit_string_buf_stub (export "bin_prot_blit_string_buf_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (return_call $caml_bigstring_blit_string_to_ba
         (local.get $src) (local.get $src_pos)
         (local.get $dst) (local.get $dst_pos)
         (local.get $len)))

   (func (export "bin_prot_blit_buf_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bin_prot_blit_buf_stub"))
      (i31.new (i32.const 0)))

   ;;;; compiler/test-jsoo

   (func (export "flush_stdout_stderr") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)

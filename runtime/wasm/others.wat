(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

   ;;;;;; base_bigstring

   (import "bigarray" "caml_ba_create"
      (func $caml_ba_create
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (import "bigstring" "caml_bigstring_blit_ba_to_ba"
      (func $bigstring_blit_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (import "bigstring" "caml_bigstring_blit_bytes_to_ba"
      (func $bigstring_blit_bytes_bigstring_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (import "bigstring" "caml_bigstring_blit_string_to_ba"
      (func $bigstring_blit_string_bigstring_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (import "bigstring" "caml_bigstring_blit_ba_to_bytes"
      (func $bigstring_blit_bigstring_bytes_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (import "bigstring" "caml_bigstring_memcmp"
      (func $bigstring_memcmp_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (func (export "bigstring_alloc")
       (param (ref eq)) (param $size (ref eq)) (result (ref eq))
       (return_call $caml_ba_create
          (i31.new (i32.const 12)) (i31.new (i32.const 0))
          (array.new_fixed $block (i31.new (i32.const 0)) (local.get $size))))

   (func (export "bigstring_is_mmapped_stub") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (export "bigstring_blit_stub" (func $bigstring_blit_stub))

   (export "bigstring_blit_bytes_bigstring_stub"
      (func $bigstring_blit_bytes_bigstring_stub))

   (export "bigstring_blit_bigstring_bytes_stub"
      (func $bigstring_blit_bigstring_bytes_stub))

   (export "bigstring_blit_string_bigstring_stub"
      (func $bigstring_blit_string_bigstring_stub))

   (export "bigstring_memcmp_stub"
      (func $bigstring_memcmp_stub))

   (func (export "bigstring_memset_stub")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bigstring_memset_stub"))
      (i31.new (i32.const 0)))

   (func (export "bigstring_memcmp_bytes_stub")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bigstring_memcmp_bytes_stub"))
      (i31.new (i32.const 0)))

   (func (export "internalhash_fold_bigstring")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "internalhash_fold_bigstring"))
      (i31.new (i32.const 0)))

   (func (export "bigstring_find")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bigstring_find"))
      (i31.new (i32.const 0)))

   (func (export "bigstring_memmem_bytecode")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "bigstring_memmem_bytecode"))
      (i31.new (i32.const 0)))

   ;;;;;; bin_prot

   (import "fail" "caml_array_bound_error" (func $caml_array_bound_error))
   (import "bigarray" "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_string_to_ba
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "bigarray" "caml_bigstring_blit_ba_to_bytes"
      (func $caml_bigstring_blit_ba_to_bytes
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (func (export "caml_check_bound_bigstring")
      (param $bigstring (ref eq)) (param $i (ref eq)) (result (ref eq))
      (if (i32.ge_u (i31.get_s (ref.cast i31 (local.get $i)))
             (array.get $int_array
                (struct.get $bigarray $ba_dim
                   (ref.cast $bigarray (local.get $bigstring)))
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

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
   (import "array" "caml_array_blit"
      (func $caml_array_blit
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))
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

   (global $Base_am_testing_flag (export "Base_am_testing_flag") (mut (i32))
      (i32.const 0))

   (func (export "Base_am_testing") (param (ref eq)) (result (ref eq))
      (call $log_js (string.const "Base_am_testing"))
      (i31.new (global.get $Base_am_testing_flag)))

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

   ;;;;;; core

   ;; ZZZ float arrays
   (export "core_array_unsafe_int_blit" (func $caml_array_blit))
   (export "core_array_unsafe_float_blit" (func $caml_array_blit))

   (import "jslib" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_new"
      (func $caml_js_new (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_fun_call"
      (func $caml_js_fun_call
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_from_array"
      (func $caml_js_from_array (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_from_float"
      (func $caml_js_from_float (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_pure_js_expr"
      (func $caml_pure_js_expr (param (ref eq)) (result (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))

   (func (export "core_time_ns_format")
      (param $time (ref eq)) (param $format (ref eq)) (result (ref eq))
      (local $d (ref eq))
      (local.set $d
         (call $caml_js_new
            (call $caml_js_get
               (call $caml_pure_js_expr
                  (call $wrap (string.const "globalThis")))
               (call $wrap (string.const "Date")))
            (call $caml_js_from_array
               (array.new_fixed $block (i31.new (i32.const 0))
                  (call $caml_js_from_float
                     (struct.new $float
                        (f64.mul
                           (struct.get $float 0
                              (ref.cast $float (local.get $time)))
                           (f64.const 1000.))))))))
      (return_call $caml_string_of_jsstring
         (call $caml_js_fun_call
            (call $caml_js_get
               (call $caml_pure_js_expr
                  (call $wrap (string.const "globalThis")))
               (call $wrap (string.const "strftime")))
            (call $caml_js_from_array
               (array.new_fixed $block (i31.new (i32.const 0))
                  (call $caml_jsstring_of_string (local.get $format))
                  (local.get $d))))))

   (func (export "core_gc_compactions") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_heap_chunks") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_heap_words") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_major_collections") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_major_plus_minor_words")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_major_words") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_minor_collections") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_minor_words") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_promoted_words") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_top_heap_words") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "core_gc_run_memprof_callbacks")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (import "md5" "caml_md5_chan"
      (func $caml_md5_chan (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "io" "caml_ml_open_descriptor_in"
      (func $caml_ml_open_descriptor_in (param (ref eq)) (result (ref eq))))
   (import "io" "caml_ml_close_channel"
      (func $caml_ml_close_channel (param (ref eq)) (result (ref eq))))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))

   (func (export "core_md5_fd") (param $fd (ref eq)) (result (ref eq))
      (local $ic (ref eq))
      (local $s (ref eq))
      (local.set $ic (call $caml_ml_open_descriptor_in (local.get $fd)))
      (local.set $s
         (try (result (ref eq)) ;; ZZZ Javascript exceptions
            (do
              (call $caml_md5_chan (local.get $ic) (i31.new (i32.const -1))))
            (catch $ocaml_exception
               (drop (pop (ref eq)))
               (drop (call $caml_ml_close_channel (local.get $ic)))
               (rethrow 0))))
      (drop (call $caml_ml_close_channel (local.get $ic)))
      (return (local.get $s)))

   (import "string" "caml_create_bytes"
      (func $caml_create_bytes (param (ref eq)) (result (ref eq))))
   (import "string" "caml_blit_string"
      (func $caml_blit_string
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "md5" "caml_md5_string"
      (func $caml_md5_string
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (func (export "core_md5_digest_subbigstring")
      (param $buf (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (param $res (ref eq)) (result (ref eq))
      (local $bytes (ref eq))
      (local $res2 (ref eq))
      (local.set $bytes (call $caml_create_bytes (local.get $len)))
      (drop
         (call $bigstring_blit_bigstring_bytes_stub
            (local.get $buf) (local.get $ofs) (local.get $bytes)
            (i31.new (i32.const 0)) (local.get $len)))
      (local.set $res2
         (call $caml_md5_string
            (local.get $bytes) (i31.new (i32.const 0)) (local.get $len)))
      (drop
         (call $caml_blit_string
           (local.get $res2) (i31.new (i32.const 0))
           (local.get $res) (i31.new (i32.const 0))
           (i31.new (i32.const 16))))
     (i31.new (i32.const 0)))

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

   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))

   (data $bigstring_destroy_already_deallocated
      "bigstring_destroy: bigstring is already deallocated")

   (func $bigstring_destroy_stub (export "bigstring_destroy_stub")
      (param $v (ref eq)) (result (ref eq))
      (local $b (ref $bigarray))
      (local.set $b (ref.cast $bigarray (local.get $v)))
      (if (ref.test i31
            (extern.internalize (struct.get $bigarray $ba_data (local.get $b))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $bigstring_destroy_already_deallocated
                  (i32.const 0) (i32.const 51)))))
      (struct.set $bigarray $ba_data (local.get $b)
         (extern.externalize (i31.new (i32.const 0))))
      (array.set $int_array (struct.get_u $bigarray $ba_dim (local.get $b))
            (i32.const 0)
            (i32.const 0))
      (i31.new (i32.const 0)))

   (data $bigstring_realloc_already_deallocated
      "bigstring_realloc: bigstring is already deallocated")

   (import "bigarray" "caml_ba_create_buffer"
      (func $caml_ba_create_buffer
         (param i32) (param i32) (result (ref extern))))

   (func (export "bigstring_realloc_stub")
      (param $vbigstring (ref eq)) (param $vsize (ref eq))
      (result (ref eq))
      (local $bigstring (ref $bigarray)) (local $new_bigstring (ref $bigarray))
      (local $size i32)
      (local $new_data (ref extern))
      (local.set $bigstring (ref.cast $bigarray (local.get $vbigstring)))
      (local.set $size (i31.get_u (ref.cast i31 (local.get $vsize))))
      (if (ref.test i31
            (extern.internalize
               (struct.get $bigarray $ba_data (local.get $bigstring))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $bigstring_realloc_already_deallocated
                  (i32.const 0) (i32.const 51)))))
      (local.set $new_data
         (call $caml_ba_create_buffer
            (struct.get $bigarray $ba_kind (local.get $bigstring))
            (local.get $size)))
      (local.set $new_bigstring
         (struct.new $bigarray
            (struct.get $bigarray $ba_ops (local.get $bigstring))
            (local.get $new_data)
            (array.new_fixed $int_array (local.get $size))
            (i32.const 1)
            (struct.get $bigarray $ba_kind (local.get $bigstring))
            (struct.get $bigarray $ba_layout (local.get $bigstring))))
      (drop (call $bigstring_destroy_stub (local.get $bigstring)))
      (local.get $new_bigstring))

   (import "obj" "lazy_tag" (global $lazy_tag i32))
   (import "obj" "forward_tag" (global $forward_tag i32))

   (func "core_heap_block_is_heap_block" (param (ref eq)) (result (ref eq))
      (local $tag i32)
      (drop (block $not_block (result (ref eq))
         (local.set $tag
            (i31.get_u
               (ref.cast i31
               (array.get $block
                  (br_on_cast_fail $not_block $block (local.get $0))
                  (i32.const 0)))))
         (i31.new
            (i32.eqz
               (i32.or (i32.eq (local.get $tag) (global.get $lazy_tag))
                       (i32.eq (local.get $tag) (global.get $forward_tag)))))))
      (i31.new (i32.const 0)))

   ;;;;;; time_now

   (func (export "time_now_nanoseconds_since_unix_epoch_or_zero")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_copy_int64
         (i64.trunc_sat_f64_s (f64.mul (call $gettimeofday) (f64.const 2e9)))))

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

   (func $set_am_testing
      (global.set $Base_am_testing_flag (i32.const 1)))

   (start $set_am_testing)

   ;;;; compiler/test-jsoo

   (func (export "flush_stdout_stderr") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)

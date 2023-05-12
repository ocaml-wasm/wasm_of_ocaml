(module
   (tag $ocaml_exception (export "ocaml_exception") (param (ref eq)))
   (tag $ocaml_exit (export "ocaml_exit") (param i32))

   (import "bindings" "log" (func $log (param i32)))
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $float (struct (field f64)))

   (type $block (array (mut (ref eq))))

   (type $string (array (mut i8)))

   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))

   (type $closure (struct (field i32) (field (ref $function_1))))

   (type $compare_ext (func (param (ref eq)) (param (ref eq)) (result i32)))

   (type $custom_operations
      (struct
         (field (ref $compare_ext))
         ;; ZZZ
      ))

   (type $custom (struct (field (ref $custom_operations))))

   (global $caml_global_data (mut (ref $block))
      (array.new $block (i31.new (i32.const 0)) (i32.const 12)))

   (func (export "caml_register_global")
      (param (ref eq)) (param $v (ref eq)) (param (ref eq)) (result (ref eq))
      (local $i i32)
      (local.set $i (i31.get_u (ref.cast i31 (local.get 0))))
      (if (i32.lt_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (array.set $block (global.get $caml_global_data)
               (local.get $i) (local.get $v))))
      (i31.new (i32.const 0)))

   (func $caml_raise_constant (param (ref eq))
      (throw $ocaml_exception (local.get 0)))

   (func $caml_raise_with_arg (param $tag (ref eq)) (param $arg (ref eq))
      (throw $ocaml_exception
         (array.new_fixed $block
            (i31.new (i32.const 0)) (local.get $tag) (local.get $arg))))

   (global $INVALID_EXN i32 (i32.const 3))

   (func $caml_invalid_argument (param $arg (ref eq))
       (call $caml_raise_with_arg
           (array.get $block (global.get $caml_global_data)
              (global.get $INVALID_EXN))
           (local.get 0)))

   (data $index_out_of_bounds "index out of bounds")

   (func $caml_bound_error (export "caml_bound_error")
      (call $caml_invalid_argument
         (array.new_data $string $index_out_of_bounds
            (i32.const 0) (i32.const 19))))

   (global $ZERO_DIVIDE_EXN i32 (i32.const 5))

   (func (export "caml_raise_zero_divide")
      (call $caml_raise_constant
         (array.get $block (global.get $caml_global_data)
                    (global.get $ZERO_DIVIDE_EXN))))

   (global $int64_ops (export "int64_ops") (ref $custom_operations)
      (struct.new $custom_operations (ref.func $int64_cmp)))

   (type $int64
      (sub $custom (struct (field (ref $custom_operations)) (field i64))))

   (func $int64_cmp (param $v1 (ref eq)) (param $v2 (ref eq)) (result i32)
      (local $i1 i64) (local $i2 i64)
      (local.set $i1 (struct.get $int64 1 (ref.cast $int64 (local.get $v1))))
      (local.set $i2 (struct.get $int64 1 (ref.cast $int64 (local.get $v2))))
      (i32.sub (i64.gt_s (local.get $i1) (local.get $i2))
               (i64.lt_s (local.get $i1) (local.get $i2))))

   (func $caml_copy_int64 (param $i i64) (result (ref eq))
      (struct.new $int64 (global.get $int64_ops) (local.get $i)))

   (func (export "caml_int64_of_string") (param $v (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $i i32) (local $len i32)
      (local $res i64)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $res (i64.const 0))
      (local.set $i (i32.const 0))
      (local.set $len (array.len (local.get $s)))
      ;; ZZZ validation / negative numbers / ...
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $res
                  (i64.add (i64.mul (local.get $res) (i64.const 10))
                     (i64.extend_i32_s
                        (i32.sub
                           (array.get_u $string (local.get $s) (local.get $i))
                           (i32.const 48)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (return_call $caml_copy_int64 (local.get $res)))

   (data $Array_make "Array.make")

   (func (export "caml_make_vect")
      (param $n (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $sz i32) (local $b (ref $block))
      (local.set $sz (i32.add (i31.get_s (ref.cast i31 (local.get $n)))
                              (i32.const 1)))
      (if (i32.lt_s (local.get $sz) (i32.const 1))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Array_make
                               (i32.const 0) (i32.const 10)))))
      (local.set $b (array.new $block (local.get $v) (local.get $sz)))
      ;; ZZZ float array
      (array.set $block (local.get $b) (i32.const 0) (i31.new (i32.const 0)))
      (local.get $b))

   (func (export "caml_array_sub")
      (param $a (ref eq)) (param $i (ref eq)) (param $vlen (ref eq))
      (result (ref eq))
      (local $a1 (ref $block)) (local $a2 (ref $block)) (local $len i32)
      (local.set $len (i31.get_u (ref.cast i31 (local.get $vlen))))
      (local.set $a1 (ref.cast $block (local.get $a)))
      (local.set $a2 (array.new $block (i31.new (i32.const 0))
                        (i32.add (local.get $len) (i32.const 1))))
      (array.copy $block $block
         (local.get $a2) (i32.const 1) (local.get $a1)
         (i32.add (i31.get_u (ref.cast i31 (local.get $i))) (i32.const 1))
         (local.get $len))
      (local.get $a2))

   (func (export "caml_array_append")
      (param $va1 (ref eq)) (param $va2 (ref eq)) (result (ref eq))
      (local $a1 (ref $block)) (local $a2 (ref $block)) (local $a (ref $block))
      (local $l1 i32) (local $l2 i32)
      (local.set $a1 (ref.cast $block (local.get $va1)))
      (local.set $l1 (array.len (local.get $a1)))
      (local.set $a2 (ref.cast $block (local.get $va2)))
      (local.set $l2 (array.len (local.get $a2)))
      (local.set $a
         (array.new $block (i31.new (i32.const 0))
            (i32.sub (i32.add (local.get $l1) (local.get $l2)) (i32.const 1))))
      ;; ZZZ float array
      (array.copy $block $block
         (local.get $a) (i32.const 1) (local.get $a1) (i32.const 1)
         (i32.sub (local.get $l1) (i32.const 1)))
      (array.copy $block $block
         (local.get $a) (i32.const 1) (local.get $a2) (local.get $l1)
         (i32.sub (local.get $l2) (i32.const 1)))
      (local.get $a))

   (func (export "caml_array_blit")
      (param $a1 (ref eq)) (param $i1 (ref eq))
      (param $a2 (ref eq)) (param $i2 (ref eq))
      (param $len (ref eq))
      (result (ref eq))
      (array.copy $string $string
         (ref.cast $string (local.get $a2))
         (i31.get_s (ref.cast i31 (local.get $i2)))
         (ref.cast $string (local.get $a1))
         (i31.get_s (ref.cast i31 (local.get $i1)))
         (i31.get_s (ref.cast i31 (local.get $len))))
      (i31.new (i32.const 0)))

   (func (export "caml_fs_init") (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_flush") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_in")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_out")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_out_channels_list")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_char")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_register_named_value")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_int_of_string")
      (param $v (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $i i32) (local $len i32)
      (local $res i32)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $res (i32.const 0))
      (local.set $i (i32.const 0))
      (local.set $len (array.len (local.get $s)))
      ;; ZZZ validation / negative numbers / ...
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $res
                  (i32.add (i32.mul (local.get $res) (i32.const 10))
                     (i32.sub
                        (array.get_u $string (local.get $s) (local.get $i))
                        (i32.const 48))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (local.get $res)))

   (func (export "caml_sys_exit") (param (ref eq)) (result (ref eq))
      (throw $ocaml_exit (i31.get_s (ref.cast i31 (local.get 0)))))

   (global $caml_oo_last_id (mut i32) (i32.const 0))

   (func (export "caml_fresh_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (i31.new (local.get $id)))

   (func (export "caml_obj_dup") (param (ref eq)) (result (ref eq))
      ;; ZZZ Deal with non-block values?
      (local $orig (ref $block))
      (local $res (ref $block))
      (local $len i32)
      (local.set $orig (ref.cast $block (local.get 0)))
      (local.set $len (array.len (local.get $orig)))
      (local.set $res
         (array.new $block (array.get $block (local.get $orig) (i32.const 0))
            (local.get $len)))
      (array.copy $block $block
         (local.get $res) (i32.const 1) (local.get $orig) (i32.const 1)
         (i32.sub (local.get $len) (i32.const 1)))
      (local.get $res))

   (global $closure_tag i32 (i32.const 247))
   (global $object_tag i32 (i32.const 248))
   (global $forward_tag i32 (i32.const 250))
   (global $string_tag i32 (i32.const 252))
   (global $float_tag i32 (i32.const 253))
   (global $double_array_tag i32 (i32.const 254))
   (global $custom_tag i32 (i32.const 255))

   (func (export "caml_obj_tag") (param $v (ref eq)) (result (ref eq))
      (if (ref.test i31 (local.get $v))
         (then (return (i31.new (i32.const 1000)))))
      (drop (block $not_block (result (ref eq))
         (return (array.get $block
                    (br_on_cast_fail $not_block $block (local.get $v))
                    (i32.const 0)))))
      (if (ref.test $string (local.get $v))
         (then (return (i31.new (global.get $string_tag)))))
      (if (ref.test $float (local.get $v))
         (then (return (i31.new (global.get $float_tag)))))
      (if (ref.test $custom (local.get $v))
         (then (return (i31.new (global.get $custom_tag)))))
      (if (ref.test $closure (local.get $v))
         (then (return (i31.new (global.get $closure_tag)))))
      ;; ZZZ float array
      ;; ZZZ others?
      (if (ref.test $js (local.get $v))
         (then (return (i31.new (global.get $custom_tag))))) ;; ZZZ ???
      (call $log (i32.const 0))
      (unreachable))

   (func (export "caml_obj_make_forward")
      (param $b (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $block (ref $block))
      (local.set $block (ref.cast $block (local.get $b)))
      (array.set $block (local.get $block)
         (i32.const 0) (i31.new (global.get $forward_tag)))
      (array.set $block (local.get $block) (i32.const 1) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_alloc_dummy") (param $size (ref eq)) (result (ref eq))
      (array.new $block (i31.new (i32.const 0))
                 (i32.add (i31.get_u (ref.cast i31 (local.get $size)))
                          (i32.const 1))))

   (func (export "caml_update_dummy")
      (param $dummy (ref eq)) (param $newval (ref eq)) (result (ref eq))
      (local $i i32)
      (local $dst (ref $block)) (local $src (ref $block))
      ;; ZZZ check for closure or float array
      (local.set $src (ref.cast $block (local.get $newval)))
      (local.set $dst (ref.cast $block (local.get $dummy)))
      (array.copy $block $block
         (local.get $dst) (i32.const 0) (local.get $src) (i32.const 0)
         (array.len (local.get $dst)))
      (i31.new (i32.const 0)))

   (export "caml_bytes_equal" (func $caml_string_equal))
   (func $caml_string_equal (export "caml_string_equal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (local $s1 (ref $string)) (local $s2 (ref $string))
      (local $len i32) (local $i i32)
      (if (ref.eq (local.get $p1) (local.get $p2))
         (then (return (i31.new (i32.const 1)))))
      (local.set $s1 (ref.cast $string (local.get $p1)))
      (local.set $s2 (ref.cast $string (local.get $p2)))
      (local.set $len (array.len $string (local.get $s1)))
      (if (i32.ne (local.get $len) (array.len $string (local.get $s2)))
         (then (return (i31.new (i32.const 0)))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (if (i32.ne (array.get_u $string (local.get $s1) (local.get $i))
                           (array.get_u $string (local.get $s2) (local.get $i)))
                  (then (return (i31.new (i32.const 0)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i31.new (i32.const 1)))

   (export "caml_bytes_notequal" (func $caml_string_notequal))
   (func $caml_string_notequal (export "caml_string_notequal")
      (param $p1 (ref eq)) (param $p2 (ref eq)) (result (ref eq))
      (return
         (i31.new (i32.eqz (i31.get_u (ref.cast i31
            (call $caml_string_equal (local.get $p1) (local.get $p2))))))))

   (export "caml_bytes_of_string" (func $caml_string_of_bytes))
   (func $caml_string_of_bytes (export "caml_string_of_bytes")
      (param $v (ref eq)) (result (ref eq))
      (local.get $v))

   (func (export "caml_string_get")
      (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $s (ref $string)) (local $p i32)
      (local.set $s (ref.cast $string (local.get $v)))
      (local.set $p (i31.get_s (ref.cast i31 (local.get $i))))
      (if (i32.ge_u (local.get $p) (array.len (local.get $s)))
         (then (call $caml_bound_error)))
      (i31.new (array.get_u $string (local.get $s) (local.get $p))))

   (data $Bytes_create "Bytes.create")

   (func (export "caml_create_bytes")
      (param $len (ref eq)) (result (ref eq))
      (local $l i32)
      (local.set $l (i31.get_u (ref.cast i31 (local.get $len))))
      (if (i32.lt_s (local.get $l) (i32.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $Bytes_create
                               (i32.const 0) (i32.const 12)))))
      (array.new $string (i32.const 0) (local.get $l)))

   (export "caml_blit_bytes" (func $caml_blit_string))
   (func $caml_blit_string (export "caml_blit_string")
      (param $v1 (ref eq)) (param $i1 (ref eq))
      (param $v2 (ref eq)) (param $i2 (ref eq))
      (param $n (ref eq)) (result (ref eq))
      (array.copy $string $string
         (ref.cast $string (local.get $v2))
         (i31.get_s (ref.cast i31 (local.get $i2)))
         (ref.cast $string (local.get $v1))
         (i31.get_s (ref.cast i31 (local.get $i1)))
         (i31.get_s (ref.cast i31 (local.get $n))))
      (i31.new (i32.const 0)))

   (func (export "caml_fill_bytes")
      (param $v (ref eq)) (param $offset (ref eq))
      (param $len (ref eq)) (param $init (ref eq))
      (result (ref eq))
      (array.fill $string (ref.cast $string (local.get $v))
         (i31.get_u (ref.cast i31 (local.get $offset)))
         (i31.get_u (ref.cast i31 (local.get $init)))
         (i31.get_u (ref.cast i31 (local.get $len))))
      (i31.new (i32.const 0)))

   (type $int_array (array (mut i32)))
   (type $block_array (array (mut (ref $block))))
   (type $compare_stack
      (struct (field (mut i32))          ;; position in stack
              (field (ref $block_array)) ;; first value
              (field (ref $block_array)) ;; second value
              (field (ref $int_array)))) ;; position in value

   (global $dummy_block (ref $block)
      (array.new $block (i31.new (i32.const 0)) (i32.const 0)))

   (global $default_compare_stack (ref $compare_stack)
      (struct.new $compare_stack (i32.const -1)
         (array.new $block_array (global.get $dummy_block) (i32.const 8))
         (array.new $block_array (global.get $dummy_block) (i32.const 8))
         (array.new $int_array (i32.const 0) (i32.const 8))))

   (func $compare_stack_is_not_empty
      (param $stack (ref $compare_stack)) (result i32)
      (i32.ge_s (struct.get $compare_stack 0 (local.get $stack)) (i32.const 0)))

   (func $pop_compare_stack (param $stack (ref $compare_stack))
      (result (ref eq)) (result (ref eq))
      (local $i i32) (local $p i32) (local $p' i32)
      (local $v1 (ref $block)) (local $v2 (ref $block))
      (local.set $i (struct.get $compare_stack 0 (local.get $stack)))
      (local.set $p
         (array.get $int_array (struct.get $compare_stack 3 (local.get $stack))
            (local.get $i)))
      (local.set $p' (i32.add (local.get $p) (i32.const 1)))
      (array.set $int_array (struct.get $compare_stack 3 (local.get $stack))
            (local.get $i) (local.get $p'))
      (local.set $v1
         (array.get $block_array
            (struct.get $compare_stack 1 (local.get $stack)) (local.get $i)))
      (local.set $v2
         (array.get $block_array
            (struct.get $compare_stack 2 (local.get $stack)) (local.get $i)))
      (if (i32.eq (local.get $p') (array.len (local.get $v1)))
         (then
            (array.set $block_array
               (struct.get $compare_stack 1 (local.get $stack))
               (local.get $i) (global.get $dummy_block))
            (array.set $block_array
               (struct.get $compare_stack 2 (local.get $stack))
               (local.get $i) (global.get $dummy_block))
            (struct.set $compare_stack 0 (local.get $stack)
               (i32.sub (local.get $i) (i32.const 1)))))
      (tuple.make (array.get $block (local.get $v1) (local.get $p))
                  (array.get $block (local.get $v2) (local.get $p))))

   (func $push_compare_stack (param $stack (ref $compare_stack))
      (param $v1 (ref $block)) (param $v2 (ref $block)) (param $p i32)
      (result (ref $compare_stack))
      (local $i i32)
      (local.set $i
         (i32.add (struct.get $compare_stack 0 (local.get $stack))
            (i32.const 1)))
      ;; ZZZ Allocate a larger stack if necessary
      (if (i32.ge_u (local.get $i)
             (array.len (struct.get $compare_stack 1 (local.get $stack))))
         (then       (call $log (i32.const 1))
(unreachable)))
      (struct.set $compare_stack 0 (local.get $stack) (local.get $i))
      (array.set $block_array (struct.get $compare_stack 1 (local.get $stack))
         (local.get $i) (local.get $v1))
      (array.set $block_array (struct.get $compare_stack 2 (local.get $stack))
         (local.get $i) (local.get $v2))
      (array.set $int_array (struct.get $compare_stack 3 (local.get $stack))
         (local.get $i) (local.get $p))
      (local.get $stack))

   (global $unordered i32 (i32.const 0x80000000))

   (func $compare_strings
      (param $s1 (ref $string)) (param $s2 (ref $string)) (result i32)
      (local $l1 i32) (local $l2 i32) (local $len i32) (local $i i32)
      (local $c1 i32) (local $c2 i32)
      (if (ref.eq (local.get $s1) (local.get $s2))
         (then (return (i32.const 0))))
      (local.set $l1 (array.len $string (local.get $s1)))
      (local.set $l2 (array.len $string (local.get $s2)))
      (local.set $len (select (local.get $l1) (local.get $l2)
                         (i32.le_u (local.get $l1) (local.get $l2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $c1
                  (array.get_u $string (local.get $s1) (local.get $i)))
               (local.set $c2
                  (array.get_u $string (local.get $s2) (local.get $i)))
               (if (i32.ne (local.get $c1) (local.get $c2))
                  (then
                     (if (i32.le_u (local.get $c1) (local.get $c2))
                        (then (return (i32.const -1)))
                        (else (return (i32.const 1))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.sub (local.get $l1) (local.get $l2)))

   (func $compare_val
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32)
      (result i32)
      (local $stack (ref $compare_stack)) (local $n i32) (local $res i32)
      (local.set $stack (global.get $default_compare_stack))
      (struct.set $compare_stack 0 (local.get $stack) (i32.const -1))
      (local.set $res
         (call $do_compare_val
            (local.get $stack) (local.get $v1) (local.get $v2)
            (local.get $total)))
;;      (if (i32.gt_s (local.get $res) (i32.const 0)) (then (local.set $res (i32.const 1))))
;;      (if (i32.lt_s (local.get $res) (i32.const 0)) (then (local.set $res (i32.const -1))))
;;      (call $log (local.get $res))
      ;; clear stack (to avoid memory leaks)
      (local.set $n (struct.get $compare_stack 0 (local.get $stack)))
      (if (i32.ge_s (local.get $n) (i32.const 0))
         (then
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (array.fill $block_array
               (struct.get $compare_stack 1 (local.get $stack))
               (i32.const 0) (global.get $dummy_block) (local.get $n))
            (array.fill $block_array
               (struct.get $compare_stack 1 (local.get $stack))
               (i32.const 0) (global.get $dummy_block) (local.get $n))))
      (local.get $res))

   (func $do_compare_val
      (param $stack (ref $compare_stack))
      (param $v1 (ref eq)) (param $v2 (ref eq)) (param $total i32) (result i32)
      (local $i1 (ref i31)) (local $i2 (ref i31))
      (local $b1 (ref $block)) (local $b2 (ref $block))
      (local $t1 i32) (local $t2 i32)
      (local $s1 i32) (local $s2 i32)
      (local $f1 f64) (local $f2 f64)
      (local $str1 (ref $string)) (local $str2 (ref $string))
      (local $c1 (ref $custom)) (local $c2 (ref $custom))
      (local $tuple ((ref eq) (ref eq)))
      (local $res i32)
      (loop $loop
         (block $next_item
            (br_if $next_item
               (i32.and (ref.eq (local.get $v1) (local.get $v2))
                        (local.get $total)))
            (drop (block $v1_is_not_int (result (ref eq))
               (local.set $i1
                  (br_on_cast_fail $v1_is_not_int i31 (local.get $v1)))
               (br_if $next_item (ref.eq (local.get $v1) (local.get $v2)))
               (drop (block $v2_is_not_int (result (ref eq))
                  (local.set $i2
                     (br_on_cast_fail $v2_is_not_int i31 (local.get $v2)))
                  ;; v1 and v2 are both integers
                  (return (i32.sub (i31.get_s (local.get $i1))
                                   (i31.get_s (local.get $i2))))))
               ;; check for forward tag
               (drop (block $v2_not_forward (result (ref eq))
                  (local.set $b2
                     (br_on_cast_fail $v2_not_forward $block (local.get $v2)))
                  (local.set $t2
                     (i31.get_u (ref.cast i31 (array.get $block (local.get $b2)
                                                 (i32.const 0)))))
                  (if (i32.eq (local.get $t2) (global.get $forward_tag))
                     (then
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  (i31.new (i32.const 1))))
               ;; ZZZ custom tag
               ;; v1 long < v2 block
               (return (i32.const -1))))
            (if (ref.test i31 (local.get $v2))
               (then
                  ;; check for forward tag
                  (drop (block $v1_not_forward (result (ref eq))
                     (local.set $b1
                        (br_on_cast_fail
                           $v1_not_forward $block (local.get $v1)))
                     (local.set $t1
                        (i31.get_u (ref.cast i31
                                      (array.get $block (local.get $b1)
                                         (i32.const 0)))))
                     (if (i32.eq (local.get $t1) (global.get $forward_tag))
                        (then
                           (local.set $v1
                              (array.get $block (local.get $b1) (i32.const 1)))
                           (br $loop)))
                     (i31.new (i32.const 1))))
                  ;; ZZZ custom tag
                  ;; v1 block > v1 long
                  (return (i32.const 1))))
            (drop (block $v1_not_block (result (ref eq))
               (local.set $b1
                  (br_on_cast_fail $v1_not_block $block (local.get $v1)))
               (local.set $t1
                  (i31.get_u (ref.cast i31 (array.get $block (local.get $b1)
                                              (i32.const 0)))))
               (drop (block $v2_not_block (result (ref eq))
                  (local.set $b2
                     (br_on_cast_fail $v2_not_block $block (local.get $v2)))
                  (local.set $t2
                     (i31.get_u (ref.cast i31 (array.get $block (local.get $b2)
                                                 (i32.const 0)))))
                  (if (i32.ne (local.get $t1) (local.get $t2))
                     (then
                        ;; check for forward tag
                        (if (i32.eq (local.get $t1) (global.get $forward_tag))
                           (then
                              (local.set $v1
                                 (array.get $block
                                    (local.get $b1) (i32.const 1)))
                              (br $loop)))
                        (if (i32.eq (local.get $t2) (global.get $forward_tag))
                           (then
                              (local.set $v2
                                 (array.get
                                    $block (local.get $b2) (i32.const 1)))
                              (br $loop)))
                        ;; compare tags
                        (return (i32.sub (local.get $t1) (local.get $t2)))))
                  ;; forward tag
                  (if (i32.eq (local.get $t1) (global.get $forward_tag))
                     (then
                        (local.set $v1
                           (array.get $block (local.get $b1) (i32.const 1)))
                        (local.set $v2
                           (array.get $block (local.get $b2) (i32.const 1)))
                        (br $loop)))
                  ;; ZZZ object tag
                  (local.set $s1 (array.len (local.get $b1)))
                  (local.set $s2 (array.len (local.get $b2)))
                  ;; compare size first
                  (if (i32.ne (local.get $s1) (local.get $s2))
                     (then (return (i32.sub (local.get $s1) (local.get $s2)))))
                  (br_if $next_item (i32.eq (local.get $s1) (i32.const 1)))
                  (if (i32.gt_u (local.get $s1) (i32.const 2))
                     (then
                       (local.set $stack
                          (call $push_compare_stack (local.get $stack)
                             (local.get $b1) (local.get $b2) (i32.const 2)))))
                  (local.set $v1
                     (array.get $block (local.get $b1) (i32.const 1)))
                  (local.set $v2
                     (array.get $block (local.get $b2) (i32.const 1)))
                  (br $loop)))
               ;; check for forward tag
               (if (i32.eq (local.get $t1) (global.get $forward_tag))
                  (then
                     (local.set $v1
                        (array.get $block (local.get $b1) (i32.const 1)))
                     (br $loop)))
               ;; v1 float array > v2 not represented as block
               (if (i32.eq (local.get $t1) (global.get $double_array_tag))
                   (then (return (i32.const 1))))
               (return (i32.const -1))))
            (drop (block $v1_not_float (result (ref eq))
               (local.set $f1
                  (struct.get $float 0
                     (br_on_cast_fail $v1_not_float $float (local.get $v1))))
               (drop (block $v2_not_float (result (ref eq))
                  (local.set $f2
                     (struct.get $float 0
                        (br_on_cast_fail $v2_not_float $float (local.get $v2))))
                  (if (f64.lt (local.get $f1) (local.get $f2))
                     (then (return (i32.const -1))))
                  (if (f64.gt (local.get $f1) (local.get $f2))
                     (then (return (i32.const 1))))
                  (if (f64.ne (local.get $f1) (local.get $f2))
                     (then
                        (if (i32.eqz (local.get $total))
                           (then (return (global.get $unordered))))
                        (if (f64.eq (local.get $f1) (local.get $f1))
                           (then (return (i32.const 1))))
                        (if (f64.eq (local.get $f2) (local.get $f2))
                           (then (return (i32.const -1))))))
                  (br $next_item)))
               ;; ZZZ forward tag
               ;; ZZZ float array
      (call $log (i32.const 2))
               (unreachable)
               (return (i32.const 1))))
            (if (ref.test $float (local.get $v2))
               (then
                  ;; ZZZ forward tag
                  ;; ZZZ float array
      (call $log (i32.const 3))
                  (unreachable)
                  (return (i32.const -1))))
            (drop (block $v1_not_string (result (ref eq))
               (local.set $str1
                  (br_on_cast_fail $v1_not_string $string (local.get $v1)))
               (drop (block $v2_not_string (result (ref eq))
                  (local.set $str2
                      (br_on_cast_fail $v2_not_string $string (local.get $v2)))
                  (local.set $res
                     (call $compare_strings
                        (local.get $str1) (local.get $str2)))
                  (br_if $next_item (i32.eqz (local.get $res)))
                  (return (local.get $res))))
               ;; ZZZ forward tag
               ;; ZZZ float array
      (call $log (i32.const 4))
               (unreachable)
               (return (i32.const 1))))
            (drop (block $v1_not_custom (result (ref eq))
               (local.set $c1
                  (br_on_cast_fail $v1_not_custom $custom (local.get $v1)))
               (drop (block $v2_not_custom (result (ref eq))
                  (local.set $c2
                      (br_on_cast_fail $v2_not_custom $custom (local.get $v2)))
                  ;; ZZZ compare types
                  ;; ZZZ abstract value?
                  (local.set $res
                     (call_ref $compare_ext
                        (local.get $v1) (local.get $v2)
                        (struct.get $custom_operations 0
                           (struct.get $custom 0 (local.get $c1)))
                        ))
                  (br_if $next_item (i32.eqz (local.get $res)))
                  (return (local.get $res))))
               ;; ZZZ forward tag
               ;; ZZZ float array
      (call $log (i32.const 5))
               (unreachable)
               (return (i32.const 1))))
      (call $log (i32.const 6))
            (unreachable)
            ;; ZZZ forward tag
            ;; ZZZ float array
            (return (i32.const 1)))
         (if (call $compare_stack_is_not_empty (local.get $stack))
            (then
               (local.set $tuple (call $pop_compare_stack (local.get $stack)))
               (local.set $v1 (tuple.extract 0 (local.get $tuple)))
               (local.set $v2 (tuple.extract 1 (local.get $tuple)))
               (br $loop))))
     (i32.const 0))

   (func (export "caml_compare")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 1)))
      (if (i32.lt_s (local.get $res) (i32.const 0))
         (then (return (i31.new (i32.const -1)))))
      (if (i32.gt_s (local.get $res) (i32.const 0))
         (then (return (i31.new (i32.const 1)))))
      (i31.new (i32.const 0)))

   (func (export "caml_equal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new
         (i32.eqz
            (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func (export "caml_notequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new
         (i32.ne (i32.const 0)
            (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func (export "caml_lessthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))
      (i31.new
         (i32.and (i32.lt_s (local.get $res) (i32.const 0))
                  (i32.ne (local.get $res) (global.get $unordered)))))

   (func (export "caml_lessequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $res i32)
      (local.set $res
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))
      (i31.new
         (i32.and (i32.le_s (local.get $res) (i32.const 0))
                  (i32.ne (local.get $res) (global.get $unordered)))))

   (func (export "caml_greaterthan")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new (i32.lt_s (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   (func (export "caml_greaterequal")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (i31.new (i32.le_s (i32.const 0)
         (call $compare_val (local.get $v1) (local.get $v2) (i32.const 0)))))

   ;; ZZZ
   (func $dummy_format_fun (param (ref eq)) (param (ref eq)) (result (ref eq))
      (array.new_fixed $string (i32.const 64)))
   (func (export "%caml_format_int_special") (param (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring (call $wrap (call $format_int (local.get 0)))))
   (func (export "caml_format_int") (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring (call $wrap (call $format_int (local.get 1)))))
   (export "caml_int32_format" (func $dummy_format_fun))
   (export "caml_int64_format" (func $dummy_format_fun))
   (export "caml_nativeint_format" (func $dummy_format_fun))
   (func (export "caml_hexstring_of_float")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (array.new_fixed $string (i32.const 64)))
   (func (export "caml_format_float")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $caml_string_of_jsstring (call $wrap (call $format_float (struct.get $float 0 (ref.cast $float (local.get 1)))))))

   (func (export "caml_get_exception_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_convert_raw_backtrace")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "caml_ml_debug_info_status")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_const_max_wosize")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0xfffffff)))

   (global $bigarray_ops (ref $custom_operations)
      ;; ZZZ
      (struct.new $custom_operations (ref.func $int64_cmp)))

   (type $bigarray
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field (ref array)) ;; data
            (field (ref $int_array)) ;; size in each dimension
            (field i8) ;; number of dimensions
            (field i8) ;; kind
            (field i8)))) ;; layout

   (func (export "caml_ba_create")
      (param $kind (ref eq)) (param $layout (ref eq)) (param $d (ref eq))
      (result (ref eq))
      (local $dims (ref $block))
      (local $num_dims i32)
      (local $len i32)
      (local $data (ref $string))
      (local.set $dims (ref.cast $block (local.get $d)))
      (local.set $num_dims (i32.sub (array.len (local.get $dims)) (i32.const 1)))
      (if (i32.eqz (i32.eq (local.get $num_dims) (i32.const 1)))
         (then (unreachable))) ;;ZZZ
      (local.set $len
         (i31.get_u (ref.cast i31
            (array.get $block (local.get $dims) (i32.const 1)))))
      (local.set $data (array.new $string (i32.const 0) (local.get $len)))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (array.new_fixed $int_array (i32.const 1))
         (local.get $num_dims)
         (i31.get_s (ref.cast i31 (local.get $kind)))
         (i31.get_s (ref.cast i31 (local.get $layout)))))

   (func (export "caml_ba_from_typed_array") (param (ref eq)) (result (ref eq))
      (local $ta externref)
      (local $len i32) (local $i i32)
      (local $data (ref $string))
      ;; ZZZ
      (local.set $ta (extern.externalize (call $unwrap (local.get 0))))
      (local.set $len (call $array_length (local.get $ta)))
      (local.set $data (array.new $string (i32.const 0) (local.get $len)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $data) (local.get $i)
                  (call $get_int (local.get $ta) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $bigarray
         (global.get $bigarray_ops)
         (local.get $data)
         (array.new_fixed $int_array (i32.const 1))
         (i32.const 1)
         (i32.const 0)
         (i32.const 0)))

   (func (export "caml_ba_get_1")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast $bigarray (local.get 0)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get 1))))
      ;; ZZZ bound check / kind / layout
      (i31.new (array.get_u $string
                  (ref.cast $string (struct.get $bigarray 1 (local.get $ba)))
                  (local.get $i))))

   (func (export "caml_ba_set_1")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $ba (ref $bigarray))
      (local $i i32)
      (local.set $ba (ref.cast $bigarray (local.get 0)))
      (local.set $i (i31.get_u (ref.cast i31 (local.get 1))))
      ;; ZZZ bound check / kind / layout
      (array.set $string
         (ref.cast $string (struct.get $bigarray 1 (local.get $ba)))
         (local.get $i) (i31.get_u (ref.cast i31 (local.get 2))))
      (i31.new (i32.const 0)))

   (func (export "caml_classify_float") (param (ref eq)) (result (ref eq))
      (local $a f64)
      (local.set $a
         (f64.abs (struct.get $float 0 (ref.cast $float (local.get 0)))))
      (i31.new
         (if (result i32) (f64.ge (local.get $a) (f64.const 0x1p-1022))
            (then
               (if (result i32) (f64.lt (local.get $a) (f64.const infinity))
                  (then (i32.const 0)) ;; normal
                  (else (i32.const 3)))) ;; infinity
            (else
               (if (result i32) (f64.eq (local.get $a) (f64.const 0))
                  (then (i32.const 2)) ;; zero
                  (else
                     (if (result i32) (f64.eq (local.get $a) (local.get $a))
                        (then (i32.const 1)) ;; subnormal
                        (else (i32.const 4))))))))) ;; nan

   (func (export "caml_modf_float") (param (ref eq)) (result (ref eq))
      (local $x f64) (local $a f64) (local $i f64) (local $f f64)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $a (f64.abs (local.get $x)))
      (if (f64.ge (local.get $a) (f64.const 0))
         (then
            (if (f64.lt (local.get $a) (f64.const infinity))
               (then ;; normal
                  (local.set $i (f64.floor (local.get $a)))
                  (local.set $f (f64.sub (local.get $a) (local.get $i)))
                  (local.set $i (f64.copysign (local.get $i) (local.get $x)))
                  (local.set $f (f64.copysign (local.get $f) (local.get $x))))
               (else ;; infinity
                  (local.set $i (local.get $x))
                  (local.set $f (f64.copysign (f64.const 0) (local.get $x))))))
         (else ;; zero or nan
            (local.set $i (local.get $x))
            (local.set $f (local.get $x))))
      (array.new_fixed $block (i31.new (i32.const 0))
         (struct.new $float (local.get $f)) (struct.new $float (local.get $i))))

   (func (export "caml_ldexp")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $n i32)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $n (i31.get_s (ref.cast i31 (local.get 1))))
      (if (i32.gt_s (local.get $n) (i32.const 1023))
         (then
            (local.set $x (f64.mul (local.get $x) (f64.const 0x1p1023)))
            (local.set $n (i32.sub (local.get $n) (i32.const 1023)))
            (if (i32.gt_s (local.get $n) (i32.const 1023))
               (then
                  ;; subnormal
                  (local.set $x (f64.mul (local.get $x) (f64.const 0x1p1023)))
                  (local.set $n (i32.sub (local.get $n) (i32.const 1023)))
                  (if (i32.gt_s (local.get $n) (i32.const 1023))
                     (then (local.set $n (i32.const 1023))))))
         (else
            (if (i32.lt_s (local.get $n) (i32.const -1022))
               (then
                  (local.set $x (f64.mul (local.get $x) (f64.const 0x1p-969)))
                  (local.set $n (i32.add (local.get $n) (i32.const 969)))
                  (if (i32.lt_s (local.get $n) (i32.const -1022))
                     (then
                        (local.set $x
                           (f64.mul (local.get $x) (f64.const 0x1p-969)))
                        (local.set $n (i32.add (local.get $n) (i32.const 969)))
                        (if (i32.lt_s (local.get $n) (i32.const -1022))
                           (then (local.set $n (i32.const -1022)))))))))))
      (struct.new $float
         (f64.mul (local.get $x)
            (f64.reinterpret_i64
               (i64.shl (i64.add (i64.extend_i32_s (local.get $n))
                                 (i64.const 0x3ff))
                        (i64.const 52))))))

   (func (export "caml_float_compare")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $y f64)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $y (struct.get $float 0 (ref.cast $float (local.get 1))))
      (if (f64.eq (local.get $x) (local.get $y))
         (then (return (i31.new (i32.const 0)))))
      (if (f64.lt (local.get $x) (local.get $y))
         (then (return (i31.new (i32.const -1)))))
      (if (f64.gt (local.get $x) (local.get $y))
         (then (return (i31.new (i32.const -1)))))
      (if (f64.eq (local.get $x) (local.get $x))
         (then (return (i31.new (i32.const 1)))))
      (if (f64.eq (local.get $y) (local.get $y))
         (then (return (i31.new (i32.const -1)))))
      (i31.new (i32.const 0)))

   (func (export "caml_nextafter")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $x f64) (local $y f64) (local $i i64) (local $j i64)
      (local.set $x (struct.get $float 0 (ref.cast $float (local.get 0))))
      (local.set $y (struct.get $float 0 (ref.cast $float (local.get 1))))
      (if (f64.ne (local.get $x) (local.get $x)) (then (return (local.get 0))))
      (if (f64.ne (local.get $y) (local.get $y)) (then (return (local.get 1))))
      (if (f64.eq (local.get $x) (local.get $y))
         (then (return (local.get 1))))
      (if (result (ref eq)) (f64.eq (local.get $x) (f64.const 0))
         (then
            (if (f64.ge (local.get $y) (f64.const 0))
               (then (return (struct.new $float (f64.const 0x1p-1074))))
               (else (return (struct.new $float (f64.const -0x1p-1074))))))
         (else
            (local.set $i (i64.reinterpret_f64 (local.get $x)))
            (local.set $j (i64.reinterpret_f64 (local.get $y)))
            (if (i32.and (i64.lt_s (local.get $i) (local.get $j))
                         (i64.lt_u (local.get $i) (local.get $j)))
               (then (local.set $i (i64.add (local.get $i) (i64.const 1))))
               (else (local.set $i (i64.sub (local.get $i) (i64.const 1)))))
            (return (struct.new $float (f64.reinterpret_i64 (local.get $i)))))))

   (type $js (struct (field anyref)))

   (func $wrap (param anyref) (result (ref eq))
      (block $is_eq (result (ref eq))
         (return (struct.new $js (br_on_cast $is_eq eq (local.get 0))))))

   (func $unwrap (param (ref eq)) (result anyref)
      (block $not_js (result anyref)
         (return (struct.get $js 0
                    (br_on_cast_fail $not_js $js (local.get 0))))))

   (import "bindings" "identity" (func $to_float (param anyref) (result f64)))
   (import "bindings" "identity" (func $from_float (param f64) (result anyref)))
   (import "bindings" "identity" (func $to_bool (param anyref) (result i32)))
   (import "bindings" "identity" (func $ref_cast_string (param anyref) (result stringref)))
   (import "bindings" "from_bool" (func $from_bool (param i32) (result anyref)))
   (import "bindings" "eval" (func $eval (param anyref) (result anyref)))
   (import "bindings" "get" (func $get (param externref) (param anyref) (result anyref)))
   (import "bindings" "set" (func $set (param anyref) (param anyref) (param anyref)))
   (import "bindings" "delete" (func $delete (param anyref) (param anyref)))
   (import "bindings" "instanceof"
      (func $instanceof (param anyref) (param anyref) (result i32)))
   (import "bindings" "typeof" (func $typeof (param anyref) (result anyref)))
   (import "bindings" "strict_equals" (func $strict_equals (param anyref) (param anyref) (result i32)))
   (import "bindings" "fun_call" (func $fun_call (param anyref) (param anyref) (result anyref)))
   (import "bindings" "meth_call" (func $meth_call (param anyref) (param anyref) (param anyref) (result anyref)))
   (import "bindings" "new" (func $new (param anyref) (param anyref) (result anyref)))
   (import "bindings" "new_obj" (func $new_obj (result anyref)))
   (import "bindings" "new_array" (func $new_array (param i32) (result externref)))
   (import "bindings" "array_length"
      (func $array_length (param externref) (result i32)))
   (import "bindings" "array_get"
      (func $array_get (param externref) (param i32) (result anyref)))
   (import "bindings" "array_set"
      (func $array_set (param externref) (param i32) (param anyref)))
   (import "bindings" "wrap_callback_strict"
      (func $wrap_callback_strict (param i32) (param (ref eq)) (result anyref)))
   (import "bindings" "get_int" (func $get_int (param externref) (param i32) (result i32)))
   (import "bindings" "format" (func $format_float (param f64) (result anyref)))
   (import "bindings" "format" (func $format_int (param (ref eq)) (result anyref)))

   (func (export "caml_js_strict_equals")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (call $strict_equals
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   ;; ZZZ We should generate JavaScript code instead of using 'eval'
   (export "caml_pure_js_expr" (func $caml_js_expr))
   (export "caml_js_var" (func $caml_js_expr))
   (export "caml_js_eval_string" (func $caml_js_expr))
   (func $caml_js_expr (export "caml_js_expr")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local.set $s (ref.cast $string (local.get 0)))
      (call $wrap (call $eval (string.new_wtf8_array replace (local.get $s) (i32.const 0) (array.len (local.get $s))))))

   (func (export "caml_js_to_float") (param (ref eq)) (result (ref eq))
      (struct.new $float (call $to_float (call $unwrap (local.get 0)))))

   (func (export "caml_js_from_float") (param (ref eq)) (result (ref eq))
      (call $wrap
         (call $from_float
            (struct.get $float 0 (ref.cast $float (local.get 0))))))

   (func (export "caml_js_to_bool") (param (ref eq)) (result (ref eq))
      (i31.new (call $to_bool (struct.get $js 0 (ref.cast $js (local.get 0))))))

   (func (export "caml_js_from_bool") (param (ref eq)) (result (ref eq))
      (struct.new $js
         (call $from_bool (i31.get_s (ref.cast i31 (local.get 0))))))

   (func (export "caml_js_fun_call")
      (param $f (ref eq)) (param $args (ref eq)) (result (ref eq))
      (call $wrap
         (call $fun_call (call $unwrap (local.get $f))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_meth_call")
      (param $o (ref eq)) (param $f (ref eq)) (param $args (ref eq))
      (result (ref eq))
      (call $wrap
         (call $meth_call (call $unwrap (local.get $o))
            (call $unwrap (call $caml_jsstring_of_string (local.get $f)))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_js_get")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test $string (local.get 1))
         (then
            ;; ZZZ jsbytes
            (local.set 1 (call $caml_jsstring_of_string (local.get 1)))))
      (call $wrap
         (call $get (extern.externalize (call $unwrap (local.get 0)))
            (call $unwrap (local.get 1)))))

   (func (export "caml_js_set")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test $string (local.get 1))
         (then
            ;; ZZZ jsbytes
            (local.set 1 (call $caml_jsstring_of_string (local.get 1)))))
      (call $set (call $unwrap (local.get 0)) (call $unwrap (local.get 1))
         (call $unwrap (local.get 2)))
      (i31.new (i32.const 0)))

   (func (export "caml_js_delete")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (if (ref.test $string (local.get 1))
         (then
            ;; ZZZ jsbytes
            (local.set 1 (call $caml_jsstring_of_string (local.get 1)))))
      (call $delete (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))
      (i31.new (i32.const 0)))

   (func (export "caml_js_instanceof")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (call $instanceof
                  (call $unwrap (local.get 0)) (call $unwrap (local.get 1)))))

   (func (export "caml_js_typeof")
      (param (ref eq)) (result (ref eq))
      (struct.new $js (call $typeof (call $unwrap (local.get 0)))))

   (func (export "caml_js_new")
      (param $c (ref eq)) (param $args (ref eq)) (result (ref eq))
      (call $wrap
         (call $new (call $unwrap (local.get $c))
            (call $unwrap (call $caml_js_from_array (local.get $args))))))

   (func (export "caml_ojs_new_arr")
      (param $c (ref eq)) (param $args (ref eq)) (result (ref eq))
      (call $wrap
         (call $new (call $unwrap (local.get $c))
            (call $unwrap (local.get $args)))))

   (func (export "caml_js_object")
      (param (ref eq)) (result (ref eq))
      (local $a (ref $block)) (local $p (ref $block))
      (local $i i32) (local $l i32)
      (local $o anyref)
      (local.set $a (ref.cast $block (local.get 0)))
      (local.set $l (array.len (local.get $a)))
      (local.set $i (i32.const 1))
      (local.set $o (call $new_obj))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $p
                  (ref.cast $block
                     (array.get $block (local.get $a) (local.get $i))))
               (call $set (local.get $o)
                  (call $unwrap
                     (call $caml_jsstring_of_string
                        (array.get $block (local.get $p) (i32.const 1))))
                  (call $unwrap
                        (array.get $block (local.get $p) (i32.const 2))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $js (local.get $o)))

   (func $caml_js_from_array (export "caml_js_from_array")
      (param (ref eq)) (result (ref eq))
      (local $a (ref $block))
      (local $a' externref)
      (local $i i32) (local $l i32)
      (local.set $a (ref.cast $block (local.get 0)))
      (local.set $l (i32.sub (array.len (local.get $a)) (i32.const 1)))
      (local.set $a' (call $new_array (local.get $l)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (call $array_set (local.get $a') (local.get $i)
                  (call $unwrap (array.get $block (local.get $a)
                                   (i32.add (local.get $i) (i32.const 1)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $js (extern.internalize (local.get $a'))))

   (func (export "caml_js_to_array")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log (i32.const 16))
(unreachable)
      (i31.new (i32.const 0)))

   (func (export "caml_js_wrap_callback_strict")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $wrap (call $wrap_callback_strict
                     (i31.get_u (ref.cast i31 (local.get 0))) (local.get 1))))

   (func (export "caml_callback")
      (param $f (ref eq)) (param $count i32) (param $args (ref extern))
      (result anyref)
      (local $acc (ref eq)) (local $i i32)
      (local.set $acc (local.get $f))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $count))
            (then
               (local.set $acc
                  (call_ref $function_1
                     (call $wrap
                        (call $get (local.get $args) (i31.new (local.get $i))))
                     (local.get $acc)
                     (struct.get $closure 1
                        (ref.cast $closure (local.get $acc)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (call $unwrap (local.get $acc)))

   (export "caml_js_from_string" (func $caml_jsstring_of_string))
   (func $caml_jsstring_of_string (export "caml_jsstring_of_string")
      (param (ref eq)) (result (ref eq))
      (local $s (ref $string))
      (local.set $s (ref.cast $string (local.get 0)))
      (struct.new $js
         (string.new_wtf8_array replace (local.get $s) (i32.const 0)
           (array.len (local.get $s)))))

   (export "caml_js_to_string" (func $caml_string_of_jsstring))
   (func $caml_string_of_jsstring (export "caml_string_of_jsstring")
      (param (ref eq)) (result (ref eq))
      (local $s stringref)
      (local $l i32)
      (local $s' (ref $string))
      ;; ZZZ ref.cast string not yet implemented by V8
      (local.set $s
         (call $ref_cast_string (struct.get $js 0 (ref.cast $js (local.get 0)))))
      (local.set $l (string.measure_wtf8 wtf8 (local.get $s)))
      (local.set $s' (array.new $string (i32.const 0) (local.get $l)))
      (drop (string.encode_wtf8_array replace
               (local.get $s) (local.get $s') (i32.const 0)))
      (local.get $s'))

   (func (export "caml_list_to_js_array")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log (i32.const 20))
(unreachable)
      (i31.new (i32.const 0)))
)


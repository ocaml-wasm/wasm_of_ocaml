(module
   (import "bindings" "log" (func $log_js (param anyref)))

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
   (type $js (struct (field anyref)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (struct (;(field i32);) (field (ref $function_1))))
   (type $closure_last_arg
      (sub $closure (struct (;(field i32);) (field (ref $function_1)))))

   (type $dummy_closure_1
      (sub $closure_last_arg
         (struct (field (ref $function_1)) (field (mut (ref null $closure))))))

   (type $function_2
      (func (param (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_2
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_2)))))

   (type $dummy_closure_2
      (sub $closure_2
         (struct (field (ref $function_1)) (field (ref $function_2))
            (field (mut (ref null $closure_2))))))

   (type $function_3
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_3
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_3)))))

   (type $dummy_closure_3
      (sub $closure_3
         (struct (field (ref $function_1)) (field (ref $function_3))
            (field (mut (ref null $closure_3))))))

   (type $function_4
      (func (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))))

   (type $closure_4
      (sub $closure
         (struct (field (ref $function_1)) (field (ref $function_4)))))

   (type $dummy_closure_4
      (sub $closure_4
         (struct (field (ref $function_1)) (field (ref $function_4))
            (field (mut (ref null $closure_4))))))

   (global $forcing_tag i32 (i32.const 244))
   (global $cont_tag i32 (i32.const 245))
   (global $lazy_tag i32 (i32.const 246))
   (global $closure_tag i32 (i32.const 247))
   (global $object_tag i32 (i32.const 248))
   (global $forward_tag (export "forward_tag") i32 (i32.const 250))
   (global $abstract_tag (export "abstract_tag") i32 (i32.const 251))
   (global $string_tag i32 (i32.const 252))
   (global $float_tag i32 (i32.const 253))
   (global $double_array_tag (export "double_array_tag") i32 (i32.const 254))
   (global $custom_tag i32 (i32.const 255))

   (func (export "caml_alloc_dummy") (param $size (ref eq)) (result (ref eq))
      (array.new $block (i31.new (i32.const 0))
                 (i32.add (i31.get_u (ref.cast i31 (local.get $size)))
                          (i32.const 1))))

   (func (export "caml_update_dummy")
      (param $dummy (ref eq)) (param $newval (ref eq)) (result (ref eq))
      (local $i i32)
      (local $dst (ref $block)) (local $src (ref $block))
      (drop (block $not_block (result (ref eq))
         (local.set $dst
            (br_on_cast_fail $not_block $block (local.get $dummy)))
         (local.set $src (ref.cast $block (local.get $newval)))
         (array.copy $block $block
            (local.get $dst) (i32.const 0) (local.get $src) (i32.const 0)
            (array.len (local.get $dst)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_1 (result (ref eq))
         (struct.set $dummy_closure_1 1
            (br_on_cast_fail $not_closure_1 $dummy_closure_1 (local.get $dummy))
            (ref.cast $closure (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_2 (result (ref eq))
         (struct.set $dummy_closure_2 2
            (br_on_cast_fail $not_closure_2 $dummy_closure_2 (local.get $dummy))
            (ref.cast $closure_2 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_3 (result (ref eq))
         (struct.set $dummy_closure_3 2
            (br_on_cast_fail $not_closure_3 $dummy_closure_3 (local.get $dummy))
            (ref.cast $closure_3 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      (drop (block $not_closure_4 (result (ref eq))
         (struct.set $dummy_closure_4 2
            (br_on_cast_fail $not_closure_4 $dummy_closure_4 (local.get $dummy))
            (ref.cast $closure_4 (local.get $newval)))
         (return (i31.new (i32.const 0)))))
      ;; ZZZ float array
      (unreachable))

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

   (func (export "caml_obj_block")
      (param $tag (ref eq)) (param $size (ref eq)) (result (ref eq))
      (local $res (ref $block))
      ;; ZZZ float array / specific types
      (local.set $res
         (array.new $block
            (i31.new (i32.const 0))
            (i32.add (i31.get_s (ref.cast i31 (local.get $size)))
                     (i32.const 1))))
      (array.set $block (local.get $res) (i32.const 0) (local.get $tag))
      (local.get $res))

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
      (if (ref.test $js (local.get $v))
         (then (return (i31.new (global.get $abstract_tag)))))
      (unreachable))

   (func (export "caml_obj_make_forward")
      (param $b (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $block (ref $block))
      (local.set $block (ref.cast $block (local.get $b)))
      (array.set $block (local.get $block)
         (i32.const 0) (i31.new (global.get $forward_tag)))
      (array.set $block (local.get $block) (i32.const 1) (local.get $v))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_make_forward")
      (param (ref eq)) (result (ref eq))
      (array.new_fixed $block (i31.new (global.get $forward_tag))
         (local.get $0)))

   (func $obj_update_tag
      (param (ref eq)) (param $o i32) (param $n i32) (result i32)
      (local $b (ref $block))
      (local.set $b (ref.cast $block (local.get $0)))
      (if (result i32) (ref.eq (array.get $block (local.get $b) (i32.const 0))
                               (i31.new (local.get $o)))
         (then
            (array.set $block (local.get $b) (i32.const 0)
               (i31.new (local.get $n)))
            (i32.const 1))
         (else
            (i32.const 0))))

   (func (export "caml_lazy_reset_to_lazy") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $lazy_tag)))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_update_to_forward") (param (ref eq)) (result (ref eq))
      (drop (call $obj_update_tag (local.get 0)
               (global.get $forcing_tag) (global.get $forward_tag)))
      (i31.new (i32.const 0)))

   (func (export "caml_lazy_update_to_forcing")
      (param (ref eq)) (result (ref eq))
      (if (ref.test $block (local.get $0))
         (then
            (if (call $obj_update_tag (local.get 0)
                   (global.get $lazy_tag) (global.get $forcing_tag))
               (then (return (i31.new (i32.const 0)))))))
      (i31.new (i32.const 1)))

   (func (export "caml_get_public_method")
      (param (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;;ZZZ
      (call $log_js (string.const "caml_get_public_method"))
      (i31.new (i32.const 0)))

   (global $caml_oo_last_id (mut i32) (i32.const 0))

   (func (export "caml_set_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (array.set $block (ref.cast $block (local.get 0)) (i32.const 2)
         (i31.new (local.get $id)))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (local.get $0))

   (func (export "caml_fresh_oo_id") (param (ref eq)) (result (ref eq))
      (local $id i32)
      (local.set $id (global.get $caml_oo_last_id))
      (global.set $caml_oo_last_id (i32.add (local.get $id) (i32.const 1)))
      (i31.new (local.get $id)))
)

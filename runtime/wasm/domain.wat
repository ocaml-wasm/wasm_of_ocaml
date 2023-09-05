(module
   (type $block (array (mut (ref eq))))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (sub (struct (;(field i32);) (field (ref $function_1)))))
   (import "sync" "caml_ml_mutex_unlock"
      (func $caml_ml_mutex_unlock (param (ref eq)) (result (ref eq))))
   (import "effect" "caml_trampoline"
      (func $caml_trampoline
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (func (export "caml_atomic_cas")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (if (result (ref eq))
         (ref.eq (array.get $block (local.get $b) (i32.const 1))
                 (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (i31.new (i32.const 1)))
         (else
            (i31.new (i32.const 0)))))

   (func (export "caml_atomic_load") (param (ref eq)) (result (ref eq))
      (array.get $block (ref.cast (ref $block) (local.get 0)) (i32.const 1)))

   (func (export "caml_atomic_fetch_add")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (i31.new (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (local.get $old))

   (func (export "caml_atomic_exchange")
      (param $ref (ref eq)) (param $v (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $r (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $r (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1) (local.get $v))
      (local.get $r))

   (global $caml_domain_dls (mut (ref eq))
      (array.new_fixed $block 1 (i31.new (i32.const 0))))

   (func (export "caml_domain_dls_set") (param $a (ref eq)) (result (ref eq))
      (global.set $caml_domain_dls (local.get $a))
      (i31.new (i32.const 0)))

   (func (export "caml_domain_dls_get") (param (ref eq)) (result (ref eq))
      (global.get $caml_domain_dls))

   (global $caml_ml_domain_unique_token (ref eq)
      (array.new_fixed $block 1 (i31.new (i32.const 0))))

   (func (export "caml_ml_domain_unique_token")
      (param (ref eq)) (result (ref eq))
      (global.get $caml_ml_domain_unique_token))

   (func (export "caml_ml_domain_set_name")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_recommended_domain_count")
      (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 1)))

   (global $caml_domain_id (mut i32) (i32.const 0))
   (global $caml_domain_latest_id (mut i32) (i32.const 1))

   (func (export "caml_domain_spawn")
      (param $f (ref eq)) (param $mutex (ref eq)) (result (ref eq))
      (local $id i32) (local $old i32)
      (local.set $id (global.get $caml_domain_latest_id))
      (global.set $caml_domain_latest_id
         (i32.add (local.get $id) (i32.const 1)))
      (local.set $old (global.get $caml_domain_id))
      (block $done
         (drop (block $not_direct (result (ref eq))
            (drop (call_ref $function_1 (i31.new (i32.const 0))
                     (local.get $f)
                        (struct.get $closure 0
                           (br_on_cast_fail $not_direct (ref eq) (ref $closure)
                              (local.get $f)))))
            (br $done)))
         (drop (call $caml_trampoline
            (local.get $f)
            (array.new_fixed $block 2
               (i31.new (i32.const 0)) (i31.new (i32.const 0))))))
      (global.set $caml_domain_id (local.get $old))
      (drop (call $caml_ml_mutex_unlock (local.get $mutex)))
      (i31.new (local.get $id)))

   (func (export "caml_ml_domain_id") (param (ref eq)) (result (ref eq))
      (i31.new (global.get $caml_domain_id)))

   (func (export "caml_ml_domain_cpu_relax") (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))
)

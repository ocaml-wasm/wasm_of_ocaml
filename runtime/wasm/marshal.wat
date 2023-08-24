(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "obj" "double_array_tag" (global $double_array_tag i32))
   (import "string" "caml_string_cat"
      (func $caml_string_cat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "effect" "caml_is_continuation"
      (func $caml_is_continuation (param (ref eq)) (result i32)))
   (import "bindings" "weak_map_new" (func $weak_map_new (result (ref any))))
   (import "bindings" "weak_map_get"
      (func $weak_map_get (param (ref any)) (param (ref eq)) (result i31ref)))
   (import "bindings" "weak_map_set"
      (func $weak_map_set (param (ref any)) (param (ref eq)) (param (ref i31))))

   (global $input_val_from_string (ref $string)
      (array.new_fixed $string 21
         (i32.const 105) (i32.const 110) (i32.const 112) (i32.const 117)
         (i32.const 116) (i32.const 95) (i32.const 118) (i32.const 97)
         (i32.const 108) (i32.const 95) (i32.const 102) (i32.const 114)
         (i32.const 111) (i32.const 109) (i32.const 95) (i32.const 115)
         (i32.const 116) (i32.const 114) (i32.const 105) (i32.const 110)
         (i32.const 103)))

   (export "caml_input_value_from_string" (func $caml_input_value_from_bytes))
   (func $caml_input_value_from_bytes (export "caml_input_value_from_bytes")
      (param $vstr (ref eq)) (param $vofs (ref eq)) (result (ref eq))
      (local $str (ref $string))
      (local $ofs i32)
      (local $s (ref $intern_state))
      (local $h (ref $marshal_header))
      (local.set $str (ref.cast (ref $string) (local.get $vstr)))
      (local.set $ofs (i31.get_u (ref.cast (ref i31) (local.get $vofs))))
      (local.set $s
         (call $get_intern_state (local.get $str) (local.get $ofs)))
      (local.set $h
         (call $parse_header (local.get $s) (global.get $input_val_from_string)))
      (if (i32.gt_s
             (i32.add (local.get $ofs)
                (i32.add (struct.get $marshal_header $data_len (local.get $h))
                   (i32.const 20)))
             (array.len (local.get $str)))
         (then
            (call $bad_length (global.get $input_val_from_string))))
      (return_call $intern_rec (local.get $s) (local.get $h)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $js (struct (field anyref)))
   (type $function_1 (func (param (ref eq) (ref eq)) (result (ref eq))))
   (type $closure (struct (;(field i32);) (field (ref $function_1))))

   (global $Intext_magic_number_small i32 (i32.const 0x8495A6BE))
   (global $Intext_magic_number_big i32 (i32.const 0x8495A6BF))

   (global $PREFIX_SMALL_BLOCK i32 (i32.const 0x80))
   (global $PREFIX_SMALL_INT i32 (i32.const 0x40))
   (global $PREFIX_SMALL_STRING i32 (i32.const 0x20))
   (global $CODE_INT8 i32 (i32.const 0x00))
   (global $CODE_INT16 i32 (i32.const 0x01))
   (global $CODE_INT32 i32 (i32.const 0x02))
   (global $CODE_INT64 i32 (i32.const 0x03))
   (global $CODE_SHARED8 i32 (i32.const 0x04))
   (global $CODE_SHARED16 i32 (i32.const 0x05))
   (global $CODE_SHARED32 i32 (i32.const 0x06))
   (global $CODE_BLOCK32 i32 (i32.const 0x08))
   (global $CODE_BLOCK64 i32 (i32.const 0x13))
   (global $CODE_STRING8 i32 (i32.const 0x09))
   (global $CODE_STRING32 i32 (i32.const 0x0A))
   (global $CODE_DOUBLE_BIG i32 (i32.const 0x0B))
   (global $CODE_DOUBLE_LITTLE i32 (i32.const 0x0C))
   (global $CODE_DOUBLE_ARRAY8_BIG i32 (i32.const 0x0D))
   (global $CODE_DOUBLE_ARRAY8_LITTLE i32 (i32.const 0x0E))
   (global $CODE_DOUBLE_ARRAY32_BIG i32 (i32.const 0x0F))
   (global $CODE_DOUBLE_ARRAY32_LITTLE i32 (i32.const 0x07))
   (global $CODE_CODEPOINTER i32 (i32.const 0x10))
   (global $CODE_INFIXPOINTER i32 (i32.const 0x11))
   (global $CODE_CUSTOM i32 (i32.const 0x12))
   (global $CODE_CUSTOM_LEN i32 (i32.const 0x18))
   (global $CODE_CUSTOM_FIXED i32 (i32.const 0x19))

   (type $intern_state
      (struct
         (field $src (ref $string))
         (field $pos (mut i32))
         (field $obj_table (mut (ref null $block)))
         (field $obj_counter (mut i32))))

   (func $get_intern_state
      (param $src (ref $string)) (param $pos i32) (result (ref $intern_state))
      (struct.new $intern_state
         (local.get $src) (local.get $pos) (ref.null $block) (i32.const 0)))

   (func $read8u (param $s (ref $intern_state)) (result i32)
      (local $pos i32) (local $res i32)
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (array.get_u $string
            (struct.get $intern_state $src (local.get $s))
            (local.get $pos)))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)))
      (local.get $res))

   (func $read8s (param $s (ref $intern_state)) (result i32)
      (local $pos i32) (local $res i32)
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (array.get_s $string
            (struct.get $intern_state $src (local.get $s))
            (local.get $pos)))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)))
      (local.get $res))

   (func $read16u (param $s (ref $intern_state)) (result i32)
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (i32.or
            (i32.shl
               (array.get_u $string (local.get $src) (local.get $pos))
               (i32.const 8))
            (array.get_u $string (local.get $src)
               (i32.add (local.get $pos) (i32.const 1)))))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 2)))
      (local.get $res))

   (func $read16s (param $s (ref $intern_state)) (result i32)
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (i32.or
            (i32.shl
               (array.get_s $string (local.get $src) (local.get $pos))
               (i32.const 8))
            (array.get_u $string (local.get $src)
               (i32.add (local.get $pos) (i32.const 1)))))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 2)))
      (local.get $res))

   (func $read32 (param $s (ref $intern_state)) (result i32)
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (local.set $res
         (i32.or
            (i32.or
               (i32.shl
                  (array.get_u $string (local.get $src) (local.get $pos))
                  (i32.const 24))
               (i32.shl
                  (array.get_u $string (local.get $src)
                     (i32.add (local.get $pos) (i32.const 1)))
                  (i32.const 16)))
            (i32.or
               (i32.shl
                  (array.get_s $string (local.get $src)
                     (i32.add (local.get $pos) (i32.const 2)))
                  (i32.const 8))
               (array.get_u $string (local.get $src)
                  (i32.add (local.get $pos) (i32.const 3))))))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 4)))
      (local.get $res))

   (func $readblock (param $s (ref $intern_state)) (param $str (ref $string))
      (local $len i32) (local $pos i32)
      (local.set $len (array.len (local.get $str)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (array.copy $string $string
         (local.get $str) (i32.const 0)
         (struct.get $intern_state $src (local.get $s)) (local.get $pos)
         (local.get $len))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (local.get $len))))

   (func $readfloat
      (param $s (ref $intern_state)) (param $code i32) (result (ref eq))
      (local $src (ref $string)) (local $pos i32) (local $res i32)
      (local $d i64)
      (local $i i32)
      (local $v (ref eq))
      (local.set $src (struct.get $intern_state $src (local.get $s)))
      (local.set $pos (struct.get $intern_state $pos (local.get $s)))
      (struct.set $intern_state $pos (local.get $s)
         (i32.add (local.get $pos) (i32.const 8)))
      (if (i32.eq (local.get $code) (global.get $CODE_DOUBLE_BIG))
         (then
            (loop $loop
               (local.set $d
                  (i64.or
                     (i64.shl (local.get $d) (i64.const 8))
                     (i64.extend_i32_u
                        (array.get_u $string (local.get $src)
                          (i32.add (local.get $pos) (local.get $i))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.lt_u (local.get $i) (i32.const 8)))))
         (else
            (loop $loop
               (local.set $d
                  (i64.rotr
                     (i64.or (local.get $d)
                        (i64.extend_i32_u
                           (array.get_u $string (local.get $src)
                             (i32.add (local.get $pos) (local.get $i)))))
                     (i64.const 8)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br_if $loop (i32.lt_u (local.get $i) (i32.const 8))))))
      (struct.new $float (f64.reinterpret_i64 (local.get $d))))

   (func $readfloats
      (param $s (ref $intern_state)) (param $code i32) (param $len i32)
      (result (ref eq))
      ;; ZZZ float array
      (local $dest (ref $block))
      (local $i i32)
      (local.set $code
         (select (global.get $CODE_DOUBLE_BIG) (global.get $CODE_DOUBLE_LITTLE)
            (i32.or
               (i32.eq (local.get $code) (global.get $CODE_DOUBLE_ARRAY8_BIG))
               (i32.eq (local.get $code)
                 (global.get $CODE_DOUBLE_ARRAY32_BIG)))))
      (local.set $dest
         (array.new $block (i31.new (i32.const 0))
            (i32.add (local.get $len) (i32.const 1))))
      (array.set $block (local.get $dest) (i32.const 0)
         (i31.new (global.get $double_array_tag)))
      (loop $loop
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (if (i32.le_u (local.get $i) (local.get $len))
            (then
               (array.set $block (local.get $dest) (local.get $i)
                  (call $readfloat (local.get $s) (local.get $code)))
               (br $loop))))
      (local.get $dest))

   (func $register_object (param $s (ref $intern_state)) (param $v (ref eq))
      (local $obj_table (ref $block))
      (local $p i32)
      (block $exit
         (local.set $obj_table
            (br_on_null $exit
               (struct.get $intern_state $obj_table (local.get $s))))
         (local.set $p (struct.get $intern_state $obj_counter (local.get $s)))
         (array.set $block (local.get $obj_table) (local.get $p) (local.get $v))
         (struct.set $intern_state $obj_counter (local.get $s)
            (i32.add (local.get $p) (i32.const 1)))))

   (type $intern_item ;; ZZZ rename
      (struct
         (field $dest (ref $block))
         (field $pos (mut i32))
         (field $next (ref null $intern_item))))

   (data $integer_too_large "input_value: integer too large")
   (data $code_pointer "input_value: code pointer")
   (data $ill_formed "input_value: ill-formed message")

   (func $intern_rec
      (param $s (ref $intern_state)) (param $h (ref $marshal_header))
      (result (ref eq))
      (local $dest (ref $block))
      (local $sp (ref $intern_item))
      (local $code i32)
      (local $header i32) (local $tag i32) (local $size i32)
      (local $len i32) (local $pos i32) (local $ofs i32)
      (local $b (ref $block))
      (local $str (ref $string))
      (local $v (ref eq))
      (local.set $dest (array.new_fixed $block 1 (i31.new (i32.const 0))))
      (local.set $sp
         (struct.new $intern_item
            (local.get $dest) (i32.const 0) (ref.null $intern_item)))
      (local.set $size (struct.get $marshal_header $num_objects (local.get $h)))
      (if (i32.eqz (local.get $size))
         (then
            (struct.set $intern_state $obj_table (local.get $s)
               (array.new $block (i31.new (i32.const 0)) (local.get $len)))))
      (local.set $v (i31.new (i32.const 0))) ;; keep validator happy
      (loop $loop
       (block $done
        (block $read_block
         (block $read_string
          (block $read_double_array
           (block $read_shared
            (local.set $code (call $read8u (local.get $s)))
            (if (i32.ge_u (local.get $code) (global.get $PREFIX_SMALL_INT))
               (then
                  (if (i32.ge_u (local.get $code) (global.get $PREFIX_SMALL_BLOCK))
                     (then
                        ;; Small block
                        (local.set $tag
                           (i32.and (local.get $code) (i32.const 0xF)))
                        (local.set $size
                           (i32.and (i32.shr_u (local.get $code) (i32.const 4))
                              (i32.const 0xF)))
                        (br $read_block))
                     (else
                        ;; Small int
                        (local.set $v
                           (i31.new
                              (i32.and (local.get $code) (i32.const 0x3F))))
                        (br $done))))
               (else
                  (if (i32.ge_u (local.get $code)
                        (global.get $PREFIX_SMALL_STRING))
                     (then
                        (local.set $len
                           (i32.and (local.get $code) (i32.const 0x1F)))
                        (br $read_string))
                     (else
                        (block $INT8
                         (block $INT16
                          (block $INT32
                           (block $INT64
                            (block $SHARED8
                             (block $SHARED16
                              (block $SHARED32
                               (block $BLOCK32
                                (block $STRING8
                                 (block $STRING32
                                  (block $DOUBLE
                                   (block $DOUBLE_ARRAY8
                                    (block $DOUBLE_ARRAY32
                                     (block $CODEPOINTER
                                      (block $CUSTOM
                                       (block $default
                                        (br_table $INT8 $INT16 $INT32 $INT64
                                           $SHARED8 $SHARED16 $SHARED32
                                           $DOUBLE_ARRAY32 $BLOCK32 $STRING8
                                           $STRING32 $DOUBLE $DOUBLE
                                           $DOUBLE_ARRAY8 $DOUBLE_ARRAY8
                                           $DOUBLE_ARRAY32 $CODEPOINTER
                                           $CODEPOINTER $CUSTOM $default $default
                                           $default $default $default
                                           $CUSTOM $CUSTOM $default
                                           (local.get $code)))
                                       ;; default
                                       (call $caml_failwith
                                          (array.new_data $string $ill_formed
                                             (i32.const 0) (i32.const 31)))
                                       (br $done))
                                      ;; CUSTOM
                                      ;; ZZZ
                                      (unreachable))
                                     ;; CODEPOINTER
                                     (call $caml_failwith
                                       (array.new_data $string $code_pointer
                                          (i32.const 0) (i32.const 25)))
                                     (br $done))
                                    ;; DOUBLE_ARRAY32
                                    (local.set $len (call $read32 (local.get $s)))
                                    (br $read_double_array))
                                   ;; DOUBLE_ARRAY8
                                   (local.set $len (call $read8u (local.get $s)))
                                   (br $read_double_array))
                                  ;; DOUBLE
                                  (local.set $v
                                     (call $readfloat
                                        (local.get $s) (local.get $code)))
                                  (call $register_object
                                     (local.get $s) (local.get $v))
                                  (br $done))
                                 ;; STRING32
                                 (local.set $len (call $read32 (local.get $s)))
                                 (br $read_string))
                                ;; STRING8
                                (local.set $len (call $read8u (local.get $s)))
                                (br $read_string))
                               ;; BLOCK32
                               (local.set $header (call $read32 (local.get $s)))
                               (local.set $tag
                                  (i32.and (local.get $header) (i32.const 0xFF)))
                               (local.set $size
                                  (i32.shr_u (local.get $header) (i32.const 10)))
                               (br $read_block))
                              ;; SHARED32
                              (local.set $ofs (call $read32 (local.get $s)))
                              (br $read_shared))
                             ;; SHARED16
                             (local.set $ofs (call $read16u (local.get $s)))
                             (br $read_shared))
                            ;; SHARED8
                            (local.set $ofs (call $read8u (local.get $s)))
                            (br $read_shared))
                           ;; INT64
                           (call $caml_failwith
                              (array.new_data $string $integer_too_large
                                 (i32.const 0) (i32.const 30)))
                           (br $done))
                          ;; INT32
                          (local.set $v (i31.new (call $read32 (local.get $s))))
                          (br $done))
                         ;; INT16
                         (local.set $v (i31.new (call $read16s (local.get $s))))
                         (br $done))
                        ;; INT8
                        (local.set $v (i31.new (call $read8s (local.get $s))))
                        (br $done))
                       ))))
           ;; read_shared
           (local.set $ofs
              (i32.sub
                 (struct.get $intern_state $obj_counter (local.get $s))
                 (local.get $ofs)))
           (local.set $v
              (array.get $block
                 (ref.as_non_null
                    (struct.get $intern_state $obj_table
                       (local.get $s)))
                 (local.get $ofs)))
           (br $done))
          ;; read_double_array
          (local.set $v
             (call $readfloats
                (local.get $s) (local.get $code) (local.get $len)))
          (call $register_object (local.get $s) (local.get $v))
          (br $done))
         ;; read_string
         (local.set $str (array.new $string (i32.const 0) (local.get $len)))
         (call $readblock (local.get $s) (local.get $str))
         (local.set $v (local.get $str))
         (call $register_object (local.get $s) (local.get $v))
         (br $done))
        ;; read_block
        (local.set $b
           (array.new $block (i31.new (i32.const 0))
              (i32.add (local.get $size) (i32.const 1))))
        (array.set $block (local.get $b) (i32.const 0)
           (i31.new (local.get $tag)))
        (if (i32.eqz (local.get $size))
           (then
              (call $register_object (local.get $s) (local.get $b)))
           (else
              (local.set $sp
                 (struct.new $intern_item
                    (local.get $b) (i32.const 1) (local.get $sp)))))
        (local.set $v (local.get $b))
        (br $done))
       ;; done
       (local.set $dest (struct.get $intern_item $dest (local.get $sp)))
       (local.set $pos (struct.get $intern_item $pos (local.get $sp)))
       (array.set $block (local.get $dest) (local.get $pos) (local.get $v))
       (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
       (struct.set $intern_item $pos (local.get $sp) (local.get $pos))
       (block $exit
          (if (i32.eq (local.get $pos) (array.len (local.get $dest)))
             (then
                (local.set $sp
                   (br_on_null $exit
                      (struct.get $intern_item $next (local.get $sp))))))
          (br $loop)))
       (array.get $block (local.get $dest) (i32.const 0)))

   (data $too_large ": object too large to be read back on a 32-bit platform")

   (func $too_large (param $prim (ref $string))
      (call $caml_failwith
         (call $caml_string_cat (local.get $prim)
            (array.new_data $string $too_large (i32.const 0) (i32.const 55)))))

   (data $bad_object ": bad object")

   (func $bad_object (param $prim (ref $string))
      (call $caml_failwith
         (call $caml_string_cat (local.get $prim)
            (array.new_data $string $bad_object (i32.const 0) (i32.const 12)))))

   (data $bad_length ": bad length")

   (func $bad_length (param $prim (ref $string))
      (call $caml_failwith
         (call $caml_string_cat (local.get $prim)
            (array.new_data $string $bad_length (i32.const 0) (i32.const 12)))))

   (type $marshal_header
      (struct
         (field $data_len i32)
         (field $num_objects i32)))

   (func $parse_header
      (param $s (ref $intern_state)) (param $prim (ref $string))
      (result (ref $marshal_header))
      (local $magic i32)
      (local $data_len i32) (local $num_objects i32) (local $whsize i32)
      (local.set $magic (call $read32 (local.get $s)))
      (if (i32.eq (local.get $magic) (global.get $Intext_magic_number_big))
         (then
            (call $too_large (local.get $prim))))
      (if (i32.ne (local.get $magic) (global.get $Intext_magic_number_small))
         (then
            (call $bad_object (local.get $prim))))
      (local.set $data_len (call $read32 (local.get $s)))
      (local.set $num_objects (call $read32 (local.get $s)))
      (drop (call $read32 (local.get $s)))
      (drop (call $read32 (local.get $s)))
      (struct.new $marshal_header
         (local.get $data_len)
         (local.get $num_objects)))

   (data $marshal_data_size "Marshal.data_size")

   (func (export "caml_marshal_data_size")
      (param $buf (ref eq)) (param $ofs (ref eq)) (result (ref eq))
      (local $s (ref $intern_state))
      (local $magic i32)
      (local.set $s
         (call $get_intern_state
            (ref.cast (ref $string) (local.get $buf))
            (i31.get_u (ref.cast (ref i31) (local.get $ofs)))))
      (local.set $magic (call $read32 (local.get $s)))
      (if (i32.eq (local.get $magic) (global.get $Intext_magic_number_big))
         (then
            (call $too_large
               (array.new_data $string $marshal_data_size
                  (i32.const 0) (i32.const 17)))))
      (if (i32.ne (local.get $magic) (global.get $Intext_magic_number_small))
         (then
            (call $bad_object
               (array.new_data $string $marshal_data_size
                  (i32.const 0) (i32.const 17)))))
      (i31.new (call $read32 (local.get $s))))

   (type $output_block
      (struct
         (field $next (mut (ref null $output_block)))
         (field $end (mut i32))
         (field $data (ref $string))))

   (type $extern_state
      (struct
         ;; Flags
         (field $no_sharing i32)
         (field $user_provided_output i32)
         ;; Header information
         (field $obj_counter (mut i32))
         (field $size_32 (mut i32))
         (field $size_64 (mut i32))
         ;; Position of already marshalled objects
         (field $pos_table (ref any))
         ;; Buffers
         (field $buf (mut (ref $string)))
         (field $pos (mut i32))
         (field $limit (mut i32))
         (field $output_first (ref $output_block))
         (field $output_last (mut (ref $output_block)))))

   (func $init_extern_state
      (param $flags (ref eq)) (param $output (ref $output_block))
      (param $pos i32) (param $user_provided_output i32)
      (result (ref $extern_state))
      (local $b (ref $block))
      (local $no_sharing i32)
      (loop $parse_flags
         (drop (block $done (result (ref eq))
            (local.set $b
               (br_on_cast_fail $done (ref eq) (ref $block) (local.get $flags)))
            (if (ref.eq (array.get $block (local.get $b) (i32.const 1))
                   (i31.new (i32.const 0)))
               (then (local.set $no_sharing (i32.const 1))))
            (local.set $flags (array.get $block (local.get $b) (i32.const 2)))
            (br $parse_flags))))
      (struct.new $extern_state
         (local.get $no_sharing)
         (local.get $user_provided_output)
         (i32.const 0)
         (i32.const 0)
         (i32.const 0)
         (call $weak_map_new)
         (struct.get $output_block $data (local.get $output))
         (local.get $pos)
         (struct.get $output_block $end (local.get $output))
         (local.get $output)
         (local.get $output)))

   (data $buffer_overflow "Marshal.to_buffer: buffer overflow")

   (global $SIZE_EXTERN_OUTPUT_BLOCK i32 (i32.const 8100))

   (func $reserve_extern_output
      (param $s (ref $extern_state)) (param $required i32) (result i32)
      (local $last (ref $output_block)) (local $blk (ref $output_block))
      (local $pos i32) (local $extra i32)
      (local $buf (ref $string))
      (local.set $pos (struct.get $extern_state $pos (local.get $s)))
      (if (i32.le_u (i32.add (local.get $pos) (local.get $required))
             (struct.get $extern_state $limit (local.get $s)))
         (then
            (struct.set $extern_state $pos (local.get $s)
               (i32.add (local.get $pos) (local.get $required)))
            (return (local.get $pos))))
      (if (struct.get $extern_state $user_provided_output (local.get $s))
         (then
            (call $caml_failwith
               (array.new_data $string $buffer_overflow
                  (i32.const 0) (i32.const 34)))))
      (local.set $last (struct.get $extern_state $output_last (local.get $s)))
      (struct.set $output_block $end (local.get $last)
         (struct.get $extern_state $pos (local.get $s)))
      (if (i32.gt_s (local.get $required)
             (i32.shr_u (global.get $SIZE_EXTERN_OUTPUT_BLOCK) (i32.const 1)))
         (then
            (local.set $extra (local.get $required))))
      (local.set $buf
         (array.new $string (i32.const 0)
            (i32.add (global.get $SIZE_EXTERN_OUTPUT_BLOCK) (local.get $extra))))
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (i32.const 0)
            (local.get $buf)))
      (struct.set $output_block $next (local.get $last) (local.get $blk))
      (struct.set $extern_state $output_last (local.get $s) (local.get $blk))
      (struct.set $extern_state $buf (local.get $s) (local.get $buf))
      (struct.set $extern_state $pos (local.get $s) (local.get $required))
      (struct.set $extern_state $limit (local.get $s)
         (array.len (local.get $buf)))
      (i32.const 0))

   (func $store16 (param $s (ref $string)) (param $pos i32) (param $n i32)
      (array.set $string (local.get $s) (local.get $pos)
         (i32.shr_u (local.get $n) (i32.const 8)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)) (local.get $n)))

   (func $store32 (param $s (ref $string)) (param $pos i32) (param $n i32)
      (array.set $string (local.get $s) (local.get $pos)
         (i32.shr_u (local.get $n) (i32.const 24)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 1))
         (i32.shr_u (local.get $n) (i32.const 16)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 2))
         (i32.shr_u (local.get $n) (i32.const 8)))
      (array.set $string (local.get $s)
         (i32.add (local.get $pos) (i32.const 3)) (local.get $n)))

   (func $write (param $s (ref $extern_state)) (param $c i32)
      (local $pos i32)
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 1)))
      (array.set $string (struct.get $extern_state $buf (local.get $s))
         (local.get $pos) (local.get $c)))

   (func $writecode8
      (param $s (ref $extern_state)) (param $c i32) (param $v i32)
      (local $pos i32) (local $buf (ref $string))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 2)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (array.set $string (local.get $buf) (local.get $pos) (local.get $c))
      (array.set $string (local.get $buf)
         (i32.add (local.get $pos) (i32.const 1)) (local.get $v)))

   (func $writecode16
      (param $s (ref $extern_state)) (param $c i32) (param $v i32)
      (local $pos i32) (local $buf (ref $string))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 3)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (array.set $string (local.get $buf) (local.get $pos) (local.get $c))
      (call $store16 (local.get $buf) (i32.add (local.get $pos) (i32.const 1))
         (local.get $v)))

   (func $writecode32
      (param $s (ref $extern_state)) (param $c i32) (param $v i32)
      (local $pos i32) (local $buf (ref $string))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 5)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (array.set $string (local.get $buf) (local.get $pos) (local.get $c))
      (call $store32 (local.get $buf) (i32.add (local.get $pos) (i32.const 1))
         (local.get $v)))

   (func $writeblock
      (param $s (ref $extern_state)) (param $str (ref $string))
      (local $len i32) (local $pos i32)
      (local.set $len (array.len (local.get $str)))
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (local.get $len)))
      (array.copy $string $string
         (struct.get $extern_state $buf (local.get $s)) (local.get $pos)
         (local.get $str) (i32.const 0) (local.get $len)))

   (func $writefloat
      (param $s (ref $extern_state)) (param $f f64)
      (local $pos i32) (local $buf (ref $string)) (local $d i64) (local $i i32)
      (local.set $pos
         (call $reserve_extern_output (local.get $s) (i32.const 8)))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (local.set $d (i64.reinterpret_f64 (local.get $f)))
      (loop $loop
         (array.set $string (local.get $buf)
            (i32.add (local.get $pos) (local.get $i))
            (i32.wrap_i64
               (i64.shr_u (local.get $d)
                  (i64.extend_i32_u (i32.shl (local.get $i) (i32.const 3))))))
         (local.set $i (i32.add (local.get $i) (i32.const 1)))
         (br_if $loop (i32.lt_u (local.get $i) (i32.const 8)))))

   (func $writefloats
      (param $s (ref $extern_state)) (param $b (ref $block))
      (local $pos i32) (local $sz i32) (local $buf (ref $string)) (local $d i64)
      (local $i i32) (local $j i32)
      (local.set $sz (i32.sub (array.len (local.get $b)) (i32.const 1)))
      (local.set $pos
         (call $reserve_extern_output
            (local.get $s) (i32.shl (local.get $sz) (i32.const 3))))
      (local.set $buf (struct.get $extern_state $buf (local.get $s)))
      (local.set $j (i32.const 1))
      (loop $loop2
         (if (i32.le_u (local.get $j) (local.get $sz))
            (then
               (local.set $d
                  (i64.reinterpret_f64
                     (struct.get $float 0
                        (ref.cast (ref $float)
                           (array.get $block (local.get $b) (local.get $j))))))
               (local.set $i (i32.const 0))
               (loop $loop
                  (array.set $string (local.get $buf)
                     (i32.add (local.get $pos) (local.get $i))
                     (i32.wrap_i64
                        (i64.shr_u (local.get $d)
                           (i64.extend_i32_u
                              (i32.shl (local.get $i) (i32.const 3))))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br_if $loop (i32.lt_u (local.get $i) (i32.const 8))))
               (local.set $pos (i32.add (local.get $pos) (i32.const 8)))
               (local.set $j (i32.add (local.get $j) (i32.const 1)))
               (br $loop2)))))

   (func $extern_lookup_position
      (param $s (ref $extern_state)) (param $obj (ref eq)) (result i32)
      (block $not_found
         (br_if $not_found (struct.get $extern_state $no_sharing (local.get $s)))
         (return
            (i31.get_s
               (br_on_null $not_found
                  (call $weak_map_get
                     (struct.get $extern_state $pos_table (local.get $s))
                     (local.get $obj))))))
      (i32.const -1))

   (func $extern_record_location
      (param $s (ref $extern_state)) (param $obj (ref eq))
      (local $pos i32)
      (if (struct.get $extern_state $no_sharing (local.get $s))
         (then (return)))
      (local.set $pos (struct.get $extern_state $obj_counter (local.get $s)))
      (struct.set $extern_state $obj_counter (local.get $s)
         (i32.add (local.get $pos) (i32.const 1)))
      (call $weak_map_set
         (struct.get $extern_state $pos_table (local.get $s))
         (local.get $obj) (i31.new (local.get $pos))))

   (func $extern_size
      (param $s (ref $extern_state)) (param $s32 i32) (param $s64 i32)
      (struct.set $extern_state $size_32 (local.get $s)
         (i32.add (struct.get $extern_state $size_32 (local.get $s))
            (i32.add (local.get $s32) (i32.const 1))))
      (struct.set $extern_state $size_64 (local.get $s)
         (i32.add (struct.get $extern_state $size_64 (local.get $s))
            (i32.add (local.get $s64) (i32.const 1)))))

   (func $extern_int (param $s (ref $extern_state)) (param $n i32)
      (if (i32.and (i32.ge_s (local.get $n) (i32.const 0))
             (i32.lt_s (local.get $n) (i32.const 0x40)))
         (then
            (call $write (local.get $s)
               (i32.add (global.get $PREFIX_SMALL_INT) (local.get $n))))
      (else (if (i32.and (i32.ge_s (local.get $n) (i32.const -128))
                   (i32.lt_s (local.get $n) (i32.const 128)))
         (then
            (call $writecode8 (local.get $s) (global.get $CODE_INT8)
               (local.get $n)))
      (else (if (i32.and (i32.ge_s (local.get $n) (i32.const -32768))
                   (i32.lt_s (local.get $n) (i32.const 32768)))
         (then
            (call $writecode16 (local.get $s) (global.get $CODE_INT16)
               (local.get $n)))
      (else
         (call $writecode32 (local.get $s) (global.get $CODE_INT32)
            (local.get $n)))))))))

   (func $extern_shared_reference (param $s (ref $extern_state)) (param $d i32)
      (if (i32.lt_u (local.get $d) (i32.const 0x100))
         (then
            (call $writecode8 (local.get $s) (global.get $CODE_SHARED8)
               (local.get $d)))
      (else (if (i32.lt_u (local.get $d) (i32.const 0x10000))
         (then
            (call $writecode16 (local.get $s) (global.get $CODE_SHARED16)
               (local.get $d)))
      (else
         (call $writecode32 (local.get $s) (global.get $CODE_SHARED32)
            (local.get $d)))))))

   (func $extern_header
      (param $s (ref $extern_state)) (param $sz (i32)) (param $tag i32)
      (if (i32.and (i32.lt_u (local.get $tag) (i32.const 16))
             (i32.lt_u (local.get $sz) (i32.const 8)))
         (then
             (call $write (local.get $s)
                (i32.add (global.get $PREFIX_SMALL_BLOCK)
                   (i32.or (local.get $tag)
                      (i32.shl (local.get $sz) (i32.const 4))))))
         (else
            (call $writecode32 (local.get $s) (global.get $CODE_BLOCK32)
               (i32.or (local.get $tag)
                  (i32.shl (local.get $sz) (i32.const 10)))))))

   (func $extern_string (param $s (ref $extern_state)) (param $v (ref $string))
      (local $len i32)
      (local.set $len (array.len (local.get $v)))
      (if (i32.lt_u (local.get $len) (i32.const 0x20))
         (then
            (call $write (local.get $s)
               (i32.add (global.get $PREFIX_SMALL_STRING) (local.get $len))))
      (else (if (i32.lt_u (local.get $len) (i32.const 0x100))
         (then
            (call $writecode8 (local.get $s) (global.get $CODE_STRING8)
               (local.get $len)))
      (else
         (call $writecode32 (local.get $s) (global.get $CODE_STRING32)
            (local.get $len))))))
      (call $writeblock (local.get $s) (local.get $v)))

   (func $extern_float (param $s (ref $extern_state)) (param $v f64)
      (call $write (local.get $s) (global.get $CODE_DOUBLE_LITTLE))
      (call $writefloat (local.get $s) (local.get $v)))

   (func $extern_float_array
      (param $s (ref $extern_state)) (param $v (ref $block))
      (local $nfloats i32)
      (local.set $nfloats (array.len (local.get $v)))
      (if (i32.lt_u (local.get $nfloats) (i32.const 0x100))
         (then
            (call $writecode8 (local.get $s)
               (global.get $CODE_DOUBLE_ARRAY8_LITTLE) (local.get $nfloats)))
         (else
            (call $writecode32 (local.get $s)
               (global.get $CODE_DOUBLE_ARRAY32_LITTLE) (local.get $nfloats))))
      (call $writefloats (local.get $s) (local.get $v)))

   (data $func_value "output_value: functional value")
   (data $cont_value "output_value: continuation value")
   (data $js_value "output_value: abstract value (JavsScript value)")
   (data $abstract_value "output_value: abstract value")

   (func $extern_rec (param $s (ref $extern_state)) (param $v (ref eq))
      (local $sp (ref null $intern_item))
      (local $item (ref $intern_item))
      (local $b (ref $block)) (local $str (ref $string))
      (local $hd i32) (local $tag i32) (local $sz i32)
      (local $pos i32)
      (loop $loop
         (block $next_item
            (drop (block $not_int (result (ref eq))
               (call $extern_int (local.get $s)
                  (i31.get_s
                     (br_on_cast_fail $not_int (ref eq) (ref i31)
                        (local.get $v))))
               (br $next_item)))
            (drop (block $not_block (result (ref eq))
               (local.set $b
                  (br_on_cast_fail $not_block (ref eq) (ref $block)
                     (local.get $v)))
               (local.set $tag
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block (local.get $b) (i32.const 0)))))
               (local.set $sz (i32.sub (array.len (local.get $b)) (i32.const 1)))
               (if (i32.eq (local.get $sz) (i32.const 0))
                  (then
                     (call $extern_header
                        (local.get $s) (i32.const 0) (local.get $tag))
                     (br $next_item)))
               (local.set $pos
                  (call $extern_lookup_position (local.get $s) (local.get $v)))
               (if (i32.ge_s (local.get $pos) (i32.const 0))
                  (then
                     (call $extern_shared_reference (local.get $s)
                        (i32.sub
                           (struct.get $extern_state $obj_counter (local.get $s))
                           (local.get $pos)))
                     (br $next_item)))
               (call $extern_record_location (local.get $s) (local.get $v))
               (if (i32.eq (local.get $tag) (global.get $double_array_tag))
                  (then
                     (call $extern_float_array (local.get $s) (local.get $b))
                     (call $extern_size (local.get $s)
                        (i32.mul (local.get $sz) (i32.const 2))
                        (local.get $sz))
                     (br $next_item)))
               (call $extern_header
                  (local.get $s) (local.get $sz) (local.get $tag))
               (call $extern_size
                  (local.get $s) (local.get $sz) (local.get $sz))
               (if (i32.gt_u (local.get $sz) (i32.const 1))
                  (then
                     (local.set $sp
                        (struct.new $intern_item
                           (local.get $b)
                           (i32.const 2)
                           (local.get $sp)))))
               (local.set $v (array.get $block (local.get $b) (i32.const 1)))
               (br $loop)))
            (local.set $pos
               (call $extern_lookup_position (local.get $s) (local.get $v)))
            (if (i32.ge_s (local.get $pos) (i32.const 0))
               (then
                  (call $extern_shared_reference (local.get $s)
                     (i32.sub
                        (struct.get $extern_state $obj_counter (local.get $s))
                        (local.get $pos)))
                  (br $next_item)))
            (call $extern_record_location (local.get $s) (local.get $v))
            (drop (block $not_string (result (ref eq))
               (local.set $str
                  (br_on_cast_fail $not_string (ref eq) (ref $string)
                     (local.get $v)))
               (call $extern_string (local.get $s) (local.get $str))
               (local.set $sz (array.len (local.get $str)))
               (call $extern_size (local.get $s)
                  (i32.add (i32.const 1)
                     (i32.shr_u (local.get $sz) (i32.const 2)))
                  (i32.add (i32.const 1)
                     (i32.shr_u (local.get $sz) (i32.const 3))))
               (br $next_item)))
            (drop (block $not_float (result (ref eq))
               (call $extern_float (local.get $s)
                  (struct.get $float 0
                     (br_on_cast_fail $not_float (ref eq) (ref $float)
                        (local.get $v))))
               (call $extern_size (local.get $s) (i32.const 2) (i32.const 1))
               (br $next_item)))
            ;; ZZZ custom object
            (if (ref.test (ref $closure) (local.get $v))
               (then
                  (call $caml_failwith
                     (array.new_data $string $func_value
                        (i32.const 0) (i32.const 30)))))
            (if (call $caml_is_continuation (local.get $v))
               (then
                  (call $caml_failwith
                     (array.new_data $string $cont_value
                        (i32.const 0) (i32.const 32)))))
            (if (ref.test (ref $js) (local.get $v))
               (then
                  (call $caml_failwith
                     (array.new_data $string $js_value
                        (i32.const 0) (i32.const 47)))))
            (call $caml_failwith
               (array.new_data $string $abstract_value
                  (i32.const 0) (i32.const 28)))
         )
         ;; next_item
         (block $done
            (local.set $item (br_on_null $done (local.get $sp)))
            (local.set $b (struct.get $intern_item $dest (local.get $item)))
            (local.set $pos (struct.get $intern_item $pos (local.get $item)))
            (local.set $v (array.get $block (local.get $b) (local.get $pos)))
            (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
            (struct.set $intern_item $pos (local.get $item) (local.get $pos))
            (if (i32.eq (local.get $pos) (array.len (local.get $b)))
               (then
                  (local.set $sp
                     (struct.get $intern_item $next (local.get $item)))))
            (br $loop))))

   (func $extern_output_length
      (param $s (ref $extern_state)) (param $pos i32) (result i32)
      (local $len i32)
      (local $output_block (ref $output_block))
      (if (struct.get $extern_state $user_provided_output (local.get $s))
         (then
            (return
               (i32.sub (struct.get $extern_state $pos (local.get $s))
                  (local.get $pos))))
         (else
            (struct.set $output_block $end
               (struct.get $extern_state $output_last (local.get $s))
               (struct.get $extern_state $pos (local.get $s)))
            (local.set $output_block
               (struct.get $extern_state $output_first (local.get $s)))
            (loop $loop
               (block $done
                  (local.set $len
                     (i32.add (local.get $len)
                        (struct.get $output_block $end
                           (local.get $output_block))))
                  (local.set $output_block
                     (br_on_null $done
                        (struct.get $output_block $next
                           (local.get $output_block))))
                  (br $loop)))
            (return (local.get $len)))))

   (func $extern_value
      (param $flags (ref eq)) (param $output (ref $output_block))
      (param $pos i32) (param $user_provided_output i32) (param $v (ref eq))
      (result i32 (ref $string) (ref $extern_state))
      (local $s (ref $extern_state)) (local $len i32)
      (local $header (ref $string))
      (local.set $s
         (call $init_extern_state
            (local.get $flags) (local.get $output) (local.get $pos)
            (local.get $user_provided_output)))
      (call $extern_rec (local.get $s) (local.get $v))
      (local.set $len
         (call $extern_output_length (local.get $s) (local.get $pos)))
      (local.set $header (array.new $string (i32.const 0) (i32.const 20)))
      (call $store32 (local.get $header) (i32.const 0)
         (global.get $Intext_magic_number_small))
      (call $store32 (local.get $header) (i32.const 4) (local.get $len))
      (call $store32 (local.get $header) (i32.const 8)
         (struct.get $extern_state $obj_counter (local.get $s)))
      (call $store32 (local.get $header) (i32.const 12)
         (struct.get $extern_state $size_32 (local.get $s)))
      (call $store32 (local.get $header) (i32.const 16)
         (struct.get $extern_state $size_64 (local.get $s)))
      (tuple.make (local.get $len) (local.get $header) (local.get $s)))

   (func (export "caml_output_value_to_string")
      (param $v (ref eq)) (param $flags (ref eq)) (result (ref eq))
      (local $r (i32 (ref $string) (ref $extern_state)))
      (local $blk (ref $output_block)) (local $pos i32) (local $len i32)
      (local $res (ref $string))
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (global.get $SIZE_EXTERN_OUTPUT_BLOCK)
            (array.new $string (i32.const 0)
               (global.get $SIZE_EXTERN_OUTPUT_BLOCK))))
      (local.set $r
         (call $extern_value
            (local.get $flags) (local.get $blk)
            (i32.const 0) (i32.const 0) (local.get $v)))
      (local.set $res
         (array.new $string (i32.const 0)
            (i32.add (tuple.extract 0 (local.get $r)) (i32.const 20))))
      (array.copy $string $string
         (local.get $res) (i32.const 0)
         (tuple.extract 1 (local.get $r)) (i32.const 0) (i32.const 20))
      (local.set $pos (i32.const 20))
      (loop $loop
         (block $done
            (local.set $len (struct.get $output_block $end (local.get $blk)))
            (array.copy $string $string
               (local.get $res) (local.get $pos)
               (struct.get $output_block $data (local.get $blk)) (i32.const 0)
               (local.get $len))
            (local.set $pos (i32.add (local.get $pos) (local.get $len)))
            (local.set $blk
               (br_on_null $done
                  (struct.get $output_block $next (local.get $blk))))))
      (local.get $res))

   (func (export "caml_output_value_to_buffer")
      (param $vbuf (ref eq)) (param $vpos (ref eq)) (param $vlen (ref eq))
      (param $v (ref eq)) (param $flags (ref eq)) (result (ref eq))
      (local $buf (ref $string)) (local $pos i32) (local $len i32)
      (local $r (i32 (ref $string) (ref $extern_state)))
      (local $blk (ref $output_block))
      (local.set $buf (ref.cast (ref $string) (local.get $vbuf)))
      (local.set $pos (i31.get_u (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (local.set $blk
         (struct.new $output_block
            (ref.null $output_block)
            (i32.add (local.get $pos) (local.get $len))
            (local.get $buf)))
      (local.set $r
         (call $extern_value
            (local.get $flags)
            (local.get $blk)
            (i32.add (local.get $pos) (i32.const 20))
            (i32.const 1)
            (local.get $v)))
      (array.copy $string $string
         (local.get $buf) (local.get $pos)
         (tuple.extract 1 (local.get $r)) (i32.const 0) (i32.const 20))
      (i31.new (i32.const 0)))

(;
//Provides: UInt8ArrayReader
//Requires: caml_string_of_array, caml_jsbytes_of_string
function UInt8ArrayReader (s, i) { this.s = s; this.i = i; }
UInt8ArrayReader.prototype = {
  read8u:function () { return this.s[this.i++]; },
  read8s:function () { return this.s[this.i++] << 24 >> 24; },
  read16u:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (s[i] << 8) | s[i + 1]
  },
  read16s:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (s[i] << 24 >> 16) | s[i + 1];
  },
  read32u:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return ((s[i] << 24) | (s[i+1] << 16) |
            (s[i+2] << 8) | s[i+3]) >>> 0;
  },
  read32s:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return (s[i] << 24) | (s[i+1] << 16) |
      (s[i+2] << 8) | s[i+3];
  },
  readstr:function (len) {
    var i = this.i;
    this.i = i + len;
    return caml_string_of_array(this.s.subarray(i, i + len));
  },
  readuint8array:function (len) {
    var i = this.i;
    this.i = i + len;
    return this.s.subarray(i, i + len);
  }
}


//Provides: MlStringReader
//Requires: caml_string_of_jsbytes, caml_jsbytes_of_string
function MlStringReader (s, i) { this.s = caml_jsbytes_of_string(s); this.i = i; }
MlStringReader.prototype = {
  read8u:function () { return this.s.charCodeAt(this.i++); },
  read8s:function () { return this.s.charCodeAt(this.i++) << 24 >> 24; },
  read16u:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (s.charCodeAt(i) << 8) | s.charCodeAt(i + 1)
  },
  read16s:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (s.charCodeAt(i) << 24 >> 16) | s.charCodeAt(i + 1);
  },
  read32u:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return ((s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
            (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3)) >>> 0;
  },
  read32s:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return (s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
      (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3);
  },
  readstr:function (len) {
    var i = this.i;
    this.i = i + len;
    return caml_string_of_jsbytes(this.s.substring(i, i + len));
  },
  readuint8array:function (len) {
    var b = new Uint8Array(len);
    var s = this.s;
    var i = this.i;
    for(var j = 0; j < len; j++) {
      b[j] = s.charCodeAt(i + j);
    }
    this.i = i + len;
    return b;
  }
}

//Provides: BigStringReader
//Requires: caml_string_of_array, caml_ba_get_1
function BigStringReader (bs, i) { this.s = bs; this.i = i; }
BigStringReader.prototype = {
  read8u:function () { return caml_ba_get_1(this.s,this.i++); },
  read8s:function () { return caml_ba_get_1(this.s,this.i++) << 24 >> 24; },
  read16u:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (caml_ba_get_1(s,i) << 8) | caml_ba_get_1(s,i + 1)
  },
  read16s:function () {
    var s = this.s, i = this.i;
    this.i = i + 2;
    return (caml_ba_get_1(s,i) << 24 >> 16) | caml_ba_get_1(s,i + 1);
  },
  read32u:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return ((caml_ba_get_1(s,i)   << 24) | (caml_ba_get_1(s,i+1) << 16) |
            (caml_ba_get_1(s,i+2) << 8)  | caml_ba_get_1(s,i+3)         ) >>> 0;
  },
  read32s:function () {
    var s = this.s, i = this.i;
    this.i = i + 4;
    return (caml_ba_get_1(s,i)   << 24) | (caml_ba_get_1(s,i+1) << 16) |
      (caml_ba_get_1(s,i+2) << 8)  | caml_ba_get_1(s,i+3);
  },
  readstr:function (len) {
    var i = this.i;
    var arr = new Array(len)
    for(var j = 0; j < len; j++){
      arr[j] = caml_ba_get_1(this.s, i+j);
    }
    this.i = i + len;
    return caml_string_of_array(arr);
  },
  readuint8array:function (len) {
    var i = this.i;
    var offset = this.offset(i);
    this.i = i + len;
    return this.s.data.subarray(offset, offset + len);
  }
}



//Provides: caml_float_of_bytes
//Requires: caml_int64_float_of_bits, caml_int64_of_bytes
function caml_float_of_bytes (a) {
  return caml_int64_float_of_bits (caml_int64_of_bytes (a));
}

//Provides: caml_input_value_from_string mutable
//Requires: MlStringReader, caml_input_value_from_reader
function caml_input_value_from_string(s,ofs) {
  var reader = new MlStringReader (s, typeof ofs=="number"?ofs:ofs[0]);
  return caml_input_value_from_reader(reader, ofs)
}

//Provides: caml_input_value_from_bytes mutable
//Requires: MlStringReader, caml_input_value_from_reader, caml_string_of_bytes
function caml_input_value_from_bytes(s,ofs) {
  var reader = new MlStringReader (caml_string_of_bytes(s), typeof ofs=="number"?ofs:ofs[0]);
  return caml_input_value_from_reader(reader, ofs)
}

//Provides: caml_int64_unmarshal
//Requires: caml_int64_of_bytes
function caml_int64_unmarshal(reader, size){
  var t = new Array(8);;
  for (var j = 0;j < 8;j++) t[j] = reader.read8u();
  size[0] = 8;
  return caml_int64_of_bytes (t);
}

//Provides: caml_int64_marshal
//Requires: caml_int64_to_bytes
function caml_int64_marshal(writer, v, sizes) {
  var b = caml_int64_to_bytes (v);
  for (var i = 0; i < 8; i++) writer.write (8, b[i]);
  sizes[0] = 8; sizes[1] = 8;
}

//Provides: caml_int32_unmarshal
function caml_int32_unmarshal(reader, size){
  size[0] = 4;
  return reader.read32s ();
}

//Provides: caml_nativeint_unmarshal
//Requires: caml_failwith
function caml_nativeint_unmarshal(reader, size){
  switch (reader.read8u ()) {
  case 1:
    size[0] = 4;
    return reader.read32s ();
  case 2:
    caml_failwith("input_value: native integer value too large");
  default: caml_failwith("input_value: ill-formed native integer");
  }
}

//Provides: caml_custom_ops
//Requires: caml_int64_unmarshal, caml_int64_marshal, caml_int64_compare, caml_int64_hash
//Requires: caml_int32_unmarshal, caml_nativeint_unmarshal
//Requires: caml_ba_serialize, caml_ba_deserialize, caml_ba_compare, caml_ba_hash
var caml_custom_ops =
    {"_j": {
      deserialize : caml_int64_unmarshal,
      serialize  : caml_int64_marshal,
      fixed_length : 8,
      compare : caml_int64_compare,
      hash : caml_int64_hash
    },
     "_i": {
       deserialize : caml_int32_unmarshal,
       fixed_length : 4,
     },
     "_n": {
       deserialize : caml_nativeint_unmarshal,
       fixed_length : 4,
     },
     "_bigarray":{
       deserialize : (function (reader, sz) {return caml_ba_deserialize (reader,sz,"_bigarray")}),
       serialize : caml_ba_serialize,
       compare : caml_ba_compare,
       hash: caml_ba_hash,
     },
     "_bigarr02":{
       deserialize : (function (reader, sz) {return caml_ba_deserialize (reader,sz,"_bigarr02")}),
       serialize : caml_ba_serialize,
       compare : caml_ba_compare,
       hash: caml_ba_hash,
     }
    }

//Provides: caml_input_value_from_reader mutable
//Requires: caml_failwith
//Requires: caml_float_of_bytes, caml_custom_ops
//Requires: zstd_decompress
//Requires: UInt8ArrayReader
function caml_input_value_from_reader(reader, ofs) {
  function readvlq(overflow) {
    var c = reader.read8u();
    var n = c & 0x7F;
    while ((c & 0x80) != 0) {
      c = reader.read8u();
      var n7 = n << 7;
      if (n != n7 >> 7) overflow[0] = true;
      n = n7 | (c & 0x7F);
    }
    return n;
  }
  var magic = reader.read32u ()
  switch(magic){
  case 0x8495A6BE: /* Intext_magic_number_small */
    var header_len = 20;
    var compressed = 0;
    var data_len = reader.read32u ();
    var uncompressed_data_len = data_len;
    var num_objects = reader.read32u ();
    var _size_32 = reader.read32u ();
    var _size_64 = reader.read32u ();
    break
  case 0x8495A6BD: /* Intext_magic_number_compressed */
    var header_len = reader.read8u() & 0x3F;
    var compressed = 1;
    var overflow = [false];
    var data_len = readvlq(overflow);
    var uncompressed_data_len = readvlq(overflow);
    var num_objects = readvlq(overflow);
    var _size_32 = readvlq (overflow);
    var _size_64 = readvlq (overflow);
    if(overflow[0]){
        caml_failwith("caml_input_value_from_reader: object too large to be read back on this platform");
    }
    break
  case 0x8495A6BF: /* Intext_magic_number_big */
    caml_failwith("caml_input_value_from_reader: object too large to be read back on a 32-bit platform");
    break
  default:
    caml_failwith("caml_input_value_from_reader: bad object");
    break;
  }
  var stack = [];
  var intern_obj_table = (num_objects > 0)?[]:null;
  var obj_counter = 0;
  function intern_rec (reader) {
    var code = reader.read8u ();
    if (code >= 0x40 /*cst.PREFIX_SMALL_INT*/) {
      if (code >= 0x80 /*cst.PREFIX_SMALL_BLOCK*/) {
        var tag = code & 0xF;
        var size = (code >> 4) & 0x7;
        var v = [tag];
        if (size == 0) return v;
        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
        stack.push(v, size);
        return v;
      } else
        return (code & 0x3F);
    } else {
      if (code >= 0x20/*cst.PREFIX_SMALL_STRING */) {
        var len = code & 0x1F;
        var v = reader.readstr (len);
        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
        return v;
      } else {
        switch(code) {
        case 0x00: //cst.CODE_INT8:
          return reader.read8s ();
        case 0x01: //cst.CODE_INT16:
          return reader.read16s ();
        case 0x02: //cst.CODE_INT32:
          return reader.read32s ();
        case 0x03: //cst.CODE_INT64:
          caml_failwith("input_value: integer too large");
          break;
        case 0x04: //cst.CODE_SHARED8:
          var offset = reader.read8u ();
          if(compressed == 0) offset = obj_counter - offset;
          return intern_obj_table[offset];
        case 0x05: //cst.CODE_SHARED16:
          var offset = reader.read16u ();
          if(compressed == 0) offset = obj_counter - offset;
          return intern_obj_table[offset];
        case 0x06: //cst.CODE_SHARED32:
          var offset = reader.read32u ();
          if(compressed == 0) offset = obj_counter - offset;
          return intern_obj_table[offset];
        case 0x08: //cst.CODE_BLOCK32:
          var header = reader.read32u ();
          var tag = header & 0xFF;
          var size = header >> 10;
          var v = [tag];
          if (size == 0) return v;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          stack.push(v, size);
          return v;
        case 0x13: //cst.CODE_BLOCK64:
          caml_failwith ("input_value: data block too large");
          break;
        case 0x09: //cst.CODE_STRING8:
          var len = reader.read8u();
          var v = reader.readstr (len);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0A: //cst.CODE_STRING32:
          var len = reader.read32u();
          var v = reader.readstr (len);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0C: //cst.CODE_DOUBLE_LITTLE:
          var t = new Array(8);;
          for (var i = 0;i < 8;i++) t[7 - i] = reader.read8u ();
          var v = caml_float_of_bytes (t);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0B: //cst.CODE_DOUBLE_BIG:
          var t = new Array(8);;
          for (var i = 0;i < 8;i++) t[i] = reader.read8u ();
          var v = caml_float_of_bytes (t);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0E: //cst.CODE_DOUBLE_ARRAY8_LITTLE:
          var len = reader.read8u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
            v[i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x0D: //cst.CODE_DOUBLE_ARRAY8_BIG:
          var len = reader.read8u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            v [i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x07: //cst.CODE_DOUBLE_ARRAY32_LITTLE:
          var len = reader.read32u();
          var v = new Array(len+1);
          v[0] = 254;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          var t = new Array(8);;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
            v[i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x0F: //cst.CODE_DOUBLE_ARRAY32_BIG:
          var len = reader.read32u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            v [i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x10: //cst.CODE_CODEPOINTER:
        case 0x11: //cst.CODE_INFIXPOINTER:
          caml_failwith ("input_value: code pointer");
          break;
        case 0x12: //cst.CODE_CUSTOM:
        case 0x18: //cst.CODE_CUSTOM_LEN:
        case 0x19: //cst.CODE_CUSTOM_FIXED:
          var c, s = "";
          while ((c = reader.read8u ()) != 0) s += String.fromCharCode (c);
          var ops = caml_custom_ops[s];
          var expected_size;
          if(!ops)
            caml_failwith("input_value: unknown custom block identifier");
          switch(code){
          case 0x12: // cst.CODE_CUSTOM (deprecated)
            break;
          case 0x19: // cst.CODE_CUSTOM_FIXED
            if(!ops.fixed_length)
              caml_failwith("input_value: expected a fixed-size custom block");
            expected_size = ops.fixed_length;
            break;
          case 0x18: // cst.CODE_CUSTOM_LEN
            expected_size = reader.read32u ();
            // Skip size64
            reader.read32s(); reader.read32s();
            break;
          }
          var old_pos = reader.i;
          var size = [0];
          var v = ops.deserialize(reader, size);
          if(expected_size != undefined){
            if(expected_size != size[0])
              caml_failwith("input_value: incorrect length of serialized custom block");
          }
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        default:
          caml_failwith ("input_value: ill-formed message");
        }
      }
    }
  }
  if(compressed) {
    var data = reader.readuint8array(data_len);
    var res = new Uint8Array(uncompressed_data_len);
    var res = zstd_decompress(data, res);
    var reader = new UInt8ArrayReader(res, 0);
  }
  var res = intern_rec (reader);
  while (stack.length > 0) {
    var size = stack.pop();
    var v = stack.pop();
    var d = v.length;
    if (d < size) stack.push(v, size);
    v[d] = intern_rec (reader);
  }
  if (typeof ofs!="number") ofs[0] = reader.i;
  return res;
}

//Provides: caml_marshal_header_size
//Version: < 5.1.0
var caml_marshal_header_size = 20

//Provides: caml_marshal_header_size
//Version: >= 5.1.0
var caml_marshal_header_size = 16



//Provides: caml_marshal_data_size mutable
//Requires: caml_failwith, caml_bytes_unsafe_get
//Requires: caml_uint8_array_of_bytes
//Requires: UInt8ArrayReader
//Requires: caml_marshal_header_size
function caml_marshal_data_size (s, ofs) {
  var r = new UInt8ArrayReader(caml_uint8_array_of_bytes(s), ofs);
  function readvlq(overflow) {
    var c = r.read8u();
    var n = c & 0x7F;
    while ((c & 0x80) != 0) {
      c = r.read8u();
      var n7 = n << 7;
      if (n != n7 >> 7) overflow[0] = true;
      n = n7 | (c & 0x7F);
    }
    return n;
  }

  switch(r.read32u()){
  case 0x8495A6BE: /* Intext_magic_number_small */
    var header_len = 20;
    var data_len = r.read32u();
    break;
  case 0x8495A6BD: /* Intext_magic_number_compressed */
    var header_len = r.read8u() & 0x3F;
    var overflow = [false];
    var data_len = readvlq(overflow);
    if(overflow[0]){
      caml_failwith("Marshal.data_size: object too large to be read back on this platform");
    }
    break
  case 0x8495A6BF: /* Intext_magic_number_big */
  default:
    caml_failwith("Marshal.data_size: bad object");
    break
  }
  return header_len - caml_marshal_header_size + data_len;
}

//Provides: MlObjectTable
var MlObjectTable;
if (typeof globalThis.Map === 'undefined') {
  MlObjectTable = function() {
    /* polyfill (using linear search) */
    function NaiveLookup(objs) { this.objs = objs; }
    NaiveLookup.prototype.get = function(v) {
      for (var i = 0; i < this.objs.length; i++) {
        if (this.objs[i] === v) return i;
      }
    };
    NaiveLookup.prototype.set = function() {
      // Do nothing here. [MlObjectTable.store] will push to [this.objs] directly.
    };

    return function MlObjectTable() {
      this.objs = []; this.lookup = new NaiveLookup(this.objs);
    };
  }();
}
else {
  MlObjectTable = function MlObjectTable() {
    this.objs = []; this.lookup = new globalThis.Map();
  };
}

MlObjectTable.prototype.store = function(v) {
  this.lookup.set(v, this.objs.length);
  this.objs.push(v);
}

MlObjectTable.prototype.recall = function(v) {
  var i = this.lookup.get(v);
  return (i === undefined)
    ? undefined : this.objs.length - i;   /* index is relative */
}

//Provides: caml_output_val
//Requires: caml_int64_to_bytes, caml_failwith
//Requires: caml_int64_bits_of_float
//Requires: caml_is_ml_bytes, caml_ml_bytes_length, caml_bytes_unsafe_get
//Requires: caml_is_ml_string, caml_ml_string_length, caml_string_unsafe_get
//Requires: MlObjectTable, caml_list_to_js_array, caml_custom_ops
//Requires: caml_invalid_argument,caml_string_of_jsbytes, caml_is_continuation_tag
var caml_output_val = function (){
  function Writer () { this.chunk = []; }
  Writer.prototype = {
    chunk_idx:20, block_len:0, obj_counter:0, size_32:0, size_64:0,
    write:function (size, value) {
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
    },
    write_at:function (pos, size, value) {
      var pos = pos;
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[pos++] = (value >> i) & 0xFF;
    },
    write_code:function (size, code, value) {
      this.chunk[this.chunk_idx++] = code;
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
    },
    write_shared:function (offset) {
      if (offset < (1 << 8)) this.write_code(8, 0x04 /*cst.CODE_SHARED8*/, offset);
      else if (offset < (1 << 16)) this.write_code(16, 0x05 /*cst.CODE_SHARED16*/, offset);
      else this.write_code(32, 0x06 /*cst.CODE_SHARED32*/, offset);
    },
    pos:function () { return this.chunk_idx },
    finalize:function () {
      this.block_len = this.chunk_idx - 20;
      this.chunk_idx = 0;
      this.write (32, 0x8495A6BE);
      this.write (32, this.block_len);
      this.write (32, this.obj_counter);
      this.write (32, this.size_32);
      this.write (32, this.size_64);
      return this.chunk;
    }
  }
  return function (v, flags) {
    flags = caml_list_to_js_array(flags);

    var no_sharing = (flags.indexOf(0 /*Marshal.No_sharing*/) !== -1),
        closures =  (flags.indexOf(1 /*Marshal.Closures*/) !== -1);
    /* Marshal.Compat_32 is redundant since integers are 32-bit anyway */

    if (closures)
      console.warn("in caml_output_val: flag Marshal.Closures is not supported.");

    var writer = new Writer ();
    var stack = [];
    var intern_obj_table = no_sharing ? null : new MlObjectTable();

    function memo(v) {
      if (no_sharing) return false;
      var existing_offset = intern_obj_table.recall(v);
      if (existing_offset) { writer.write_shared(existing_offset); return true; }
      else { intern_obj_table.store(v); return false; }
    }

    function extern_rec (v) {
      if (v.caml_custom) {
        if (memo(v)) return;
        var name = v.caml_custom;
        var ops = caml_custom_ops[name];
        var sz_32_64 = [0,0];
        if(!ops.serialize)
          caml_invalid_argument("output_value: abstract value (Custom)");
        if(ops.fixed_length == undefined){
          writer.write (8, 0x18 /*cst.CODE_CUSTOM_LEN*/);
          for (var i = 0; i < name.length; i++)
            writer.write (8, name.charCodeAt(i));
          writer.write(8, 0);
          var header_pos = writer.pos ();
          for(var i = 0; i < 12; i++) {
            writer.write(8, 0);
          }
          ops.serialize(writer, v, sz_32_64);
          writer.write_at(header_pos, 32, sz_32_64[0]);
          writer.write_at(header_pos + 4, 32, 0); // zero
          writer.write_at(header_pos + 8, 32, sz_32_64[1]);
        } else {
          writer.write (8, 0x19 /*cst.CODE_CUSTOM_FIXED*/);
          for (var i = 0; i < name.length; i++)
            writer.write (8, name.charCodeAt(i));
          writer.write(8, 0);
          var old_pos = writer.pos();
          ops.serialize(writer, v, sz_32_64);
          if (ops.fixed_length != writer.pos() - old_pos)
            caml_failwith("output_value: incorrect fixed sizes specified by " + name);
        }
        writer.size_32 += 2 + ((sz_32_64[0] + 3) >> 2);
        writer.size_64 += 2 + ((sz_32_64[1] + 7) >> 3);
      }
      else if (v instanceof Array && v[0] === (v[0]|0)) {
        if (v[0] == 251) {
          caml_failwith("output_value: abstract value (Abstract)");
        }
        if (caml_is_continuation_tag(v[0]))
          caml_invalid_argument("output_value: continuation value");
        if (v.length > 1 && memo(v)) return;
        if (v[0] < 16 && v.length - 1 < 8)
          writer.write (8, 0x80 /*cst.PREFIX_SMALL_BLOCK*/ + v[0] + ((v.length - 1)<<4));
        else
          writer.write_code(32, 0x08 /*cst.CODE_BLOCK32*/, ((v.length-1) << 10) | v[0]);
        writer.size_32 += v.length;
        writer.size_64 += v.length;
        if (v.length > 1) stack.push (v, 1);
      } else if (caml_is_ml_bytes(v)) {
        if(!(caml_is_ml_bytes(caml_string_of_jsbytes("")))) {
          caml_failwith("output_value: [Bytes.t] cannot safely be marshaled with [--enable use-js-string]");
        }
        if (memo(v)) return;
        var len = caml_ml_bytes_length(v);
        if (len < 0x20)
          writer.write (8, 0x20 /*cst.PREFIX_SMALL_STRING*/ + len);
        else if (len < 0x100)
          writer.write_code (8, 0x09/*cst.CODE_STRING8*/, len);
        else
          writer.write_code (32, 0x0A /*cst.CODE_STRING32*/, len);
        for (var i = 0;i < len;i++)
          writer.write (8, caml_bytes_unsafe_get(v,i));
        writer.size_32 += 1 + (((len + 4) / 4)|0);
        writer.size_64 += 1 + (((len + 8) / 8)|0);
      } else if (caml_is_ml_string(v)) {
        if (memo(v)) return;
        var len = caml_ml_string_length(v);
        if (len < 0x20)
          writer.write (8, 0x20 /*cst.PREFIX_SMALL_STRING*/ + len);
        else if (len < 0x100)
          writer.write_code (8, 0x09/*cst.CODE_STRING8*/, len);
        else
          writer.write_code (32, 0x0A /*cst.CODE_STRING32*/, len);
        for (var i = 0;i < len;i++)
          writer.write (8, caml_string_unsafe_get(v,i));
        writer.size_32 += 1 + (((len + 4) / 4)|0);
        writer.size_64 += 1 + (((len + 8) / 8)|0);
      } else {
        if (v != (v|0)){
          var type_of_v = typeof v;
          //
          // If a float happens to be an integer it is serialized as an integer
          // (Js_of_ocaml cannot tell whether the type of an integer number is
          // float or integer.) This can result in unexpected crashes when
          // unmarshalling using the standard runtime. It seems better to
          // systematically fail on marshalling.
          //
          //          if(type_of_v != "number")
          caml_failwith("output_value: abstract value ("+type_of_v+")");
          //          var t = caml_int64_to_bytes(caml_int64_bits_of_float(v));
          //          writer.write (8, 0x0B /*cst.CODE_DOUBLE_BIG*/);
          //          for(var i = 0; i<8; i++){writer.write(8,t[i])}
        }
        else if (v >= 0 && v < 0x40) {
          writer.write (8, 0X40 /*cst.PREFIX_SMALL_INT*/ + v);
        } else {
          if (v >= -(1 << 7) && v < (1 << 7))
            writer.write_code(8, 0x00 /*cst.CODE_INT8*/, v);
          else if (v >= -(1 << 15) && v < (1 << 15))
            writer.write_code(16, 0x01 /*cst.CODE_INT16*/, v);
          else
            writer.write_code(32, 0x02 /*cst.CODE_INT32*/, v);
        }
      }
    }
    extern_rec (v);
    while (stack.length > 0) {
      var i = stack.pop ();
      var v = stack.pop ();
      if (i + 1 < v.length) stack.push (v, i + 1);
      extern_rec (v[i]);
    }
    if (intern_obj_table) writer.obj_counter = intern_obj_table.objs.length;
    writer.finalize();
    return writer.chunk;
  }
} ();

//Provides: caml_output_value_to_string mutable
//Requires: caml_output_val, caml_string_of_array
function caml_output_value_to_string (v, flags) {
  return caml_string_of_array (caml_output_val (v, flags));
}

//Provides: caml_output_value_to_bytes mutable
//Requires: caml_output_val, caml_bytes_of_array
function caml_output_value_to_bytes (v, flags) {
  return caml_bytes_of_array (caml_output_val (v, flags));
}

//Provides: caml_output_value_to_buffer
//Requires: caml_output_val, caml_failwith, caml_blit_bytes
function caml_output_value_to_buffer (s, ofs, len, v, flags) {
  var t = caml_output_val (v, flags);
  if (t.length > len) caml_failwith ("Marshal.to_buffer: buffer overflow");
  caml_blit_bytes(t, 0, s, ofs, t.length);
  return 0;
}
;)
)

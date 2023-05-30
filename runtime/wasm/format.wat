(module
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))

   (type $string (array (mut i8)))

   (data $format_error "format_int: bad format")

   (func (export "parse_int_format")
      (param $s (ref $string)) (result i32 i32 i32 i32 i32)
      (local $i i32) (local $len i32) (local $c i32)
      (local $sign_style i32) (local $alternate i32) (local $base i32)
      (local $signed $i32) (local $uppercase i32)
      (local.set $len (array.len (local.get $s)))
      (local.set $i (i32.const 1))
      (block $return
         (block $bad_format
            (br_if $bad_format (i32.lt_u (local.get $len) (i32.const 2)))
            (br_if $bad_format
               (i32.ne (array.get $string (local.get $s) (i32.const 0))
                       (i32.const 37))) ;; '%'
            (local.set $c (array.get $string (local.get $s) (i32.const 1)))
            (if (i32.eq (local.get $c) (i32.const 43)) ;; '+'
               (then
                  (local.set $sign_style (i32.const 1))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))))
            (if (i32.eq (local.get $c) (i32.const 32)) ;; ' '
               (then
                  (local.set $sign_style (i32.const 2)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1))))
            (if (i32.eq (local.get $c) (i32.const 35)) ;; '#'
               (then
                  (local.set $alternate (i32.const 1)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1))))
            (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
            (local.set $c (array.get $string (local.get $s) (i32.const 1)))
            (if (i32.or (i32.or (i32.eq (local.get $c) (i32.const 76)) ;; 'L'
                                (i32.eq (local.get $c) (i32.const 108))) ;; 'l'
                        (i32.eq (local.get $c) (i32.const 110))) ;; 'n'
               (then
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br_if $bad_format (i32.eq (local.get $i) (local.get $len)))
                  (local.set $c
                     (array.get $string (local.get $s) (i32.const 1)))))
            (br_if $bad_format
              (i32.ne (i32.add (local.get $i) (i32.const 1)) (local.get $len)))
            (if (i32.or (i32.eq (local.get $c) (i32.const 100)) ;; 'd'
                        (i32.eq (local.get $c) (i32.const 105))) ;; 'i'
               (then
                  (local.set $base (i32.const 10))
                  (local.set $signed (i32.const 1)))
            (else (if (i32.eq (local.get $c) (i32.const 117)) ;; 'u'
               (then
                  (local.set $base (i32.const 10)))
            (else (if (i32.eq (local.get $c) (i32.const 120)) ;; 'x'
               (then
                  (local.set $base (i32.const 16)))
            (else (if (i32.eq (local.get $c) (i32.const 120)) ;; 'X'
               (then
                  (local.set $base (i32.const 16))
                  (local.set $uppercase (i32.const 1)))
            (else (if (i32.eq (local.get $c) (i32.const 111)) ;; 'o'
               (then
                  (local.set $base (i32.const 8)))
            (else
               (br $bad_format)))))))))))
            (br $return))
         (call $caml_invalid_argument
            (array.new_data $string $format_error
               (i32.const 0) (i32.const 22))))
      (tuple.make
         (local.get $sign_style)
         (local.get $alternate)
         (local.get $signed)
         (local.get $base)
         (local.get $uppercase)))
)

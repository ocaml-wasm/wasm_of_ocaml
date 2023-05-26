(module
   (type $string (array (mut i8)))

   (data $format_error "format_int: format too long")

(; ZZZ We need to handle just this format: %[+ #][Lln]?[dixXou] ;)

(;
   (func (export "parse_format") (param (ref eq)))
      (local $s (ref $string))
      (local $justify i32) (local $signstyle i32) (local $filler i32)
      (local $alternate i32) (local $base i32) (local $signedconv i32)
      (local $width i32) (local $uppercase i32) (local $sign i32)
      (local $prec i32) (local $conv i32)
      (local $len i32) (local $i i32)
      (local.set $justify (i32.const 43)) ;; '+'
      (local.set $signstyle (i32.const 45)) ;; '-'
      (local.set $filler (i32.const 32) ;; ' '
      (local.set $alternate (i32.const 0)) ;; false
      (local.set $base (i32.const 0))
      (local.set $signedconv (i32.const 0)) ;; false
      (local.set $width (i32.const 0))
      (local.set $uppercase (i32.const 0)) ;; false
      (local.set $sign (i32.const 1))
      (local.set $prec (i32.const -1))
      (local.set $conv (i32.const 102)) ;; 'f'
      (local.set $s (ref.cast $string (local.get 0)))
      (local.set $len (array.len (local.get $s)))
      (local.set $i 0)
      (if (i32.gt_u (local.get $len) (i32.const 31))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $format_error
                  (i32.const 0) (i32.const 27)))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (local.set $c (array.get $string (local.get $s) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (if (i32.eq (local.get $c) (i32.const 45)) ;; '-'
                  (then
                     (local.set $justify (local.get $c))
                     (br $loop)))
               (if (i32.or (i32.eq (local.get $c) (i32.const 43))  ;; '+'
                           (i32.eq (local.get $c) (i32.const 32))) ;; ' '
                  (then
                     (local.set $signstyle (local.get $c))
                     (br $loop)))
               (if (i32.eq (local.get $c) (i32.const 48)) ;; '0'
                  (then
                     (local.set $filler (local.get $c))
                     (br $loop)))
               (if (i32.eq (local.get $c) (i32.const 35)) ;; '#'
                  (then
                     (local.set $alternate (i32.const 1) ;; true
                     (br $loop)))
               (if (i32.or (i32.eq (local.get $c) (i32.const 100)) ;; 'd'
                           (i32.eq (local.get $c) (i32.const 105)) ;; 'i'
                  (then
                     (local.set $alternate (i32.const 1) ;; true
                     (br $loop)))
               (if (i32.and (i32.ge_u (local.get $c) (i32.const 48))  ;; '0'
                            (i32.le_u (local.get $c) (i32.const 57))) ;; '9'
                  (then
                     (local.set $width (i32.const 0))
                     (loop $width
                        (local.set $width
                           (i32.add (i32.mul (local.get $width) (i32.const 10))
                                    (i32.sub (local.get $c) (i32.const 48))))
                        (local.set $c
                           (array.get $string (local.get $s) (local.get $i)))
                        (if (i32.lt_u (local.get $i) (local.get $len))

                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (local.set $c 
;)


(;
if n = 0 => "0" (except if %+d / % d)

if n < 0 => set first char to -1 / add 1 to position
compute length
write digits from right to left
add 
;)
)

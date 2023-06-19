(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param $arg (ref eq))))

   (type $string (array (mut i8)))
   (type $block (array (mut (ref eq))))

   (type $char_table (array i8))
   (type $int_array (array (mut i32)))

   (global $re_word_letters (ref $char_table)
      (array.new_fixed $char_table
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0x00) (i32.const 0x00)   ;; 0x00-0x1F: none
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0xFF) (i32.const 0x03)   ;; 0x20-0x3F: digits 0-9
         (i32.const 0xFE) (i32.const 0xFF)
         (i32.const 0xFF) (i32.const 0x87)   ;; 0x40-0x5F: A to Z, _
         (i32.const 0xFE) (i32.const 0xFF)
         (i32.const 0xFF) (i32.const 0x07)   ;; 0x60-0x7F: a to z
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0x00) (i32.const 0x00)   ;; 0x80-0x9F: none
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0x00) (i32.const 0x00)   ;; 0xA0-0xBF: none
         (i32.const 0xFF) (i32.const 0xFF)   ;; 0xC0-0xDF:
         (i32.const 0x7F) (i32.const 0xFF)   ;; Latin-1 accented uppercase
         (i32.const 0xFF) (i32.const 0xFF)   ;; 0xE0-0xFF:
         (i32.const 0x7F) (i32.const 0xFF))) ;; Latin-1 accented lowercase

   (rec (type $stack (struct (field (ref null $stack)))))
   (type $pos
      (sub $stack
         (struct
            (field $pos_previous (ref null $stack))
            (field $pc i32)
            (field $pos i32))))
   (type $undo
      (sub $stack
         (struct
            (field $undo_previous (ref null $stack))
            (field $tbl (ref $int_array))
            (field $idx i32)
            (field $val i32))))

   (func $is_word_letter (param $c i32) (result i32)
      (i32.and (i32.const 1)
         (i32.shr_u
            (array.get_u $char_table (global.get $re_word_letters)
               (i32.shr_u (local.get $c) (i32.const 3)))
            (i32.and (local.get $c) (i32.const 7)))))

   (func $in_bitset (param $s (ref $string)) (param $c i32) (result i32)
      (i32.and (i32.const 1)
         (i32.shr_u
            (array.get_u $string (local.get $s)
               (i32.shr_u (local.get $c) (i32.const 3)))
            (i32.and (local.get $c) (i32.const 7)))))

   (func $re_match
      (param $vre (ref eq)) (param $s (ref $string)) (param $pos i32)
      (param $accept_partial_match i32) (result (ref eq))
      (local $res (ref $block))
      (local $s' (ref $string)) (local $set (ref $string))
      (local $len i32) (local $instr i32) (local $arg i32) (local $i i32)
      (local $j i32) (local $l i32)
      (local $re (ref $block))
      (local $prog (ref $block))
      (local $cpool (ref $block))
      (local $normtable (ref $string))
      (local $numgroups i32)
      (local $numregisters i32)
      (local $group_start (ref $int_array))
      (local $group_end (ref $int_array))
      (local $re_register (ref $int_array))
      (local $pc i32)
      (local $stack (ref null $stack))
      (local $u (ref $undo))
      (local $p (ref $pos))
      (local.set $len (array.len (local.get $s)))
      (local.set $re (ref.cast $block (local.get $vre)))
      (local.set $prog
         (ref.cast $block (array.get $block (local.get $re) (i32.const 1))))
      (local.set $cpool
         (ref.cast $block (array.get $block (local.get $re) (i32.const 2))))
      (local.set $normtable
         (ref.cast $string (array.get $block (local.get $re) (i32.const 3))))
      (local.set $numgroups
         (i31.get_s
            (ref.cast i31 (array.get $block (local.get $re) (i32.const 4)))))
      (local.set $numregisters
         (i31.get_s
            (ref.cast i31 (array.get $block (local.get $re) (i32.const 5)))))
      (local.set $group_start
         (array.new $int_array (i32.const -1) (local.get $numgroups)))
      (local.set $group_end
         (array.new $int_array (i32.const -1) (local.get $numgroups)))
      (local.set $re_register
         (array.new $int_array (i32.const -1) (local.get $numregisters)))
      (local.set $pc (i32.const 1))
      (array.set $int_array (local.get $group_start) (i32.const 0)
         (local.get $pos))
      (block $reject
       (block $ACCEPT
        (loop $continue
         (block $backtrack
          (block $prefix_match
           (block $CHECKPROGRESS
            (block $SETMARK
             (block $PUSHBACK
              (block $GOTO
               (block $SIMPLEPLUS
                (block $SIMPLESTAR
                 (block $SIMPLEOPT
                  (block $REFGROUP
                   (block $ENDGROUP
                    (block $BEGGROUP
                     (block $WORDBOUNDARY
                      (block $EOL
                       (block $BOL
                        (block $CHARCLASS
                         (block $STRINGNORM
                          (block $STRING
                           (block $CHARNORM
                            (block $CHAR
                             (local.set $instr
                                (i31.get_s
                                   (ref.cast i31
                                      (array.get $block (local.get $prog)
                                         (local.get $pc)))))
                             (local.set $pc
                                (i32.add (local.get $pc) (i32.const 1)))
                             (br_table
                                $CHAR $CHARNORM $STRING $STRINGNORM $CHARCLASS
                                $BOL $EOL $WORDBOUNDARY $BEGGROUP $ENDGROUP
                                $REFGROUP $ACCEPT $SIMPLEOPT $SIMPLESTAR
                                $SIMPLEPLUS $GOTO $PUSHBACK $SETMARK
                                $CHECKPROGRESS
                                (i32.and (local.get $instr) (i32.const 0xff))))
                            ;; CHAR
;;                            (call $log_js (string.const "CHAR"))
                            (br_if $prefix_match
                               (i32.eq (local.get $pos) (local.get $len)))
                            (local.set $arg
                               (i32.shr_u (local.get $instr) (i32.const 8)))
                            (br_if $backtrack
                               (i32.ne (local.get $arg)
                                  (array.get_u $string
                                     (local.get $s) (local.get $pos))))
                            (local.set $pos
                               (i32.add (local.get $pos) (i32.const 1)))
                            (br $continue))
                           ;; CHARNORM
;;                           (call $log_js (string.const "CHARNORM"))
                           (br_if $prefix_match
                              (i32.eq (local.get $pos) (local.get $len)))
                           (local.set $arg
                              (i32.shr_u (local.get $instr) (i32.const 8)))
                           (br_if $backtrack
                              (i32.ne (local.get $arg)
                                 (array.get_u $string
                                    (local.get $normtable)
                                    (array.get_u $string
                                       (local.get $s) (local.get $pos)))))
                           (local.set $pos
                              (i32.add (local.get $pos) (i32.const 1)))
                           (br $continue))
                          ;; STRING
;;                          (call $log_js (string.const "STRING"))
                          (local.set $arg
                             (i32.shr_u (local.get $instr) (i32.const 8)))
                          (local.set $s'
                             (ref.cast $string
                                (array.get $block (local.get $cpool)
                                   (i32.add (local.get $arg) (i32.const 1)))))
                          (local.set $i (i32.const 0))
                          (local.set $l (array.len (local.get $s')))
                          (loop $loop
                             (if (i32.lt_u (local.get $i) (local.get $l))
                                (then
                                   (br_if $prefix_match
                                      (i32.eq
                                         (local.get $pos) (local.get $len)))
                                   (br_if $backtrack
                                      (i32.ne
                                         (array.get_u $string (local.get $s')
                                            (local.get $i))
                                         (array.get_u $string (local.get $s)
                                            (local.get $pos))))
                                   (local.set $pos
                                      (i32.add (local.get $pos) (i32.const 1)))
                                   (local.set $i
                                      (i32.add (local.get $i) (i32.const 1)))
                                   (br $loop))))
                          (br $continue))
                         ;; STRINGNORM
;;                         (call $log_js (string.const "STRINGNORM"))
                         (local.set $arg
                            (i32.shr_u (local.get $instr) (i32.const 8)))
                         (local.set $s'
                            (ref.cast $string
                               (array.get $block (local.get $cpool)
                                  (i32.add (local.get $arg) (i32.const 1)))))
                         (local.set $i (i32.const 0))
                         (local.set $l (array.len (local.get $s')))
                         (loop $loop
                            (if (i32.lt_u (local.get $i) (local.get $l))
                               (then
                                  (br_if $prefix_match
                                     (i32.eq
                                        (local.get $pos) (local.get $len)))
                                  (br_if $backtrack
                                     (i32.ne
                                        (array.get_u $string (local.get $s')
                                           (local.get $i))
                                        (array.get_u $string
                                           (local.get $normtable)
                                           (array.get_u $string (local.get $s)
                                              (local.get $pos)))))
                                  (local.set $pos
                                     (i32.add (local.get $pos) (i32.const 1)))
                                  (local.set $i
                                     (i32.add (local.get $i) (i32.const 1)))
                                  (br $loop))))
                         (br $continue))
                        ;; CHARCLASS
;;                        (call $log_js (string.const "CHARCLASS"))
                        (br_if $prefix_match
                           (i32.eq (local.get $pos) (local.get $len)))
                        (local.set $arg
                           (i32.shr_u (local.get $instr) (i32.const 8)))
                        (br_if $backtrack
                           (i32.eqz
                              (call $in_bitset
                                 (ref.cast $string
                                    (array.get $block (local.get $cpool)
                                       (i32.add (local.get $arg)
                                          (i32.const 1))))
                                 (array.get_u $string (local.get $s)
                                    (local.get $pos)))))
                        (local.set $pos
                           (i32.add (local.get $pos) (i32.const 1)))
                        (br $continue))
                       ;; BOL
;;                       (call $log_js (string.const "BOL"))
                       (br_if $continue (i32.eqz (local.get $pos)))
                       (br_if $continue
                          (i32.eq (i32.const 10) ;; '\n'
                             (array.get_u $string (local.get $s)
                                (i32.sub (local.get $pos) (i32.const 1)))))
                       (br $backtrack))
                      ;; EOL
;;                      (call $log_js (string.const "EOL"))
                      (br_if $continue
                         (i32.eq (local.get $pos) (local.get $len)))
                      (br_if $continue
                         (i32.eq (i32.const 10) ;; '\n'
                            (array.get_u $string (local.get $s)
                               (local.get $pos))))
                      (br $backtrack))
                     ;; WORDBOUNDARY
;;                     (call $log_js (string.const "WORDBOUNDARY"))
                     (if (i32.eqz (local.get $pos))
                        (then
                           (br_if $prefix_match
                              (i32.eq (local.get $pos) (local.get $len)))
                           (br_if $continue
                              (call $is_word_letter
                                 (array.get_u $string (local.get $s)
                                    (local.get $pos))))
                           (br $backtrack))
                        (else
                           (if (i32.eq (local.get $pos) (local.get $len))
                              (then
                                 (br_if $continue
                                    (call $is_word_letter
                                       (array.get_u $string (local.get $s)
                                          (i32.sub (local.get $pos)
                                             (i32.const 1)))))
                                 (br $backtrack))
                              (else
                                 (br_if $continue
                                    (i32.ne
                                       (call $is_word_letter
                                          (array.get_u $string (local.get $s)
                                             (i32.sub (local.get $pos)
                                                (i32.const 1))))
                                       (call $is_word_letter
                                          (array.get_u $string (local.get $s)
                                             (local.get $pos)))))
                                 (br $backtrack))))))
                    ;; BEGGROUP
;;                    (call $log_js (string.const "BEGGROUP"))
                    (local.set $arg
                       (i32.shr_u (local.get $instr) (i32.const 8)))
                    (local.set $stack
                       (struct.new $undo
                          (local.get $stack)
                          (local.get $group_start)
                          (local.get $arg)
                          (array.get $int_array
                             (local.get $group_start) (local.get $arg))))
                    (array.set $int_array (local.get $group_start)
                       (local.get $arg) (local.get $pos))
                    (br $continue))
                   ;; ENDGROUP
;;                   (call $log_js (string.const "ENDGROUP"))
                   (local.set $arg
                      (i32.shr_u (local.get $instr) (i32.const 8)))
                    (local.set $stack
                       (struct.new $undo
                          (local.get $stack)
                          (local.get $group_end)
                          (local.get $arg)
                          (array.get $int_array
                             (local.get $group_end) (local.get $arg))))
                   (array.set $int_array (local.get $group_end)
                      (local.get $arg) (local.get $pos))
                   (br $continue))
                  ;; REFGROUP
;;                  (call $log_js (string.const "REFGROUP"))
                  (local.set $arg
                     (i32.shr_u (local.get $instr) (i32.const 8)))
                  (local.set $i
                     (array.get $int_array (local.get $group_start)
                        (local.get $arg)))
                  (local.set $j
                     (array.get $int_array (local.get $group_end)
                        (local.get $arg)))
                  (br_if $backtrack
                     (i32.or (i32.lt_s (local.get $i) (i32.const 0))
                         (i32.lt_s (local.get $j) (i32.const 0))))
                  (loop $loop
                     (if (i32.lt_u (local.get $i) (local.get $j))
                        (then
                           (br_if $prefix_match
                              (i32.eq (local.get $pos) (local.get $len)))
                           (br_if $backtrack
                              (i32.ne
                                 (array.get_u $string (local.get $s)
                                    (local.get $i))
                                 (array.get_u $string (local.get $s)
                                    (local.get $pos))))
                           (local.set $pos
                              (i32.add (local.get $pos) (i32.const 1)))
                           (local.set $i
                              (i32.add (local.get $i) (i32.const 1)))
                           (br $loop))))
                  (br $continue))
                 ;; SIMPLEOPT
;;                 (call $log_js (string.const "SIMPLEOPT"))
                 (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
                 (if (i32.lt_u (local.get $pos) (local.get $len))
                    (then
                       (if (call $in_bitset
                              (ref.cast $string
                                 (array.get $block (local.get $cpool)
                                    (i32.add (local.get $arg) (i32.const 1))))
                              (array.get_u $string (local.get $s)
                                 (local.get $pos)))
                          (then
                             (local.set $pos
                                (i32.add (local.get $pos) (i32.const 1)))))))
                 (br $continue))
                ;; SIMPLESTAR
;;                (call $log_js (string.const "SIMPLESTAR"))
                (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
                (local.set $set
                   (ref.cast $string
                      (array.get $block (local.get $cpool)
                      (i32.add (local.get $arg) (i32.const 1)))))
                (loop $loop
                   (if (i32.lt_u (local.get $pos) (local.get $len))
                      (then
                         (if (call $in_bitset (local.get $set)
                                (array.get_u $string (local.get $s)
                                   (local.get $pos)))
                            (then
                               (local.set $pos
                                  (i32.add (local.get $pos) (i32.const 1)))
                               (br $loop))))))
                (br $continue))
               ;; SIMPLEPLUS
;;               (call $log_js (string.const "SIMPLEPLUS"))
               (br_if $prefix_match (i32.eq (local.get $pos) (local.get $len)))
               (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
               (local.set $set
                  (ref.cast $string
                     (array.get $block (local.get $cpool)
                     (i32.add (local.get $arg) (i32.const 1)))))
               (br_if $backtrack
                  (i32.eqz
                     (call $in_bitset (local.get $set)
                        (array.get_u $string (local.get $s) (local.get $pos)))))
               (loop $loop
                  (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
                  (if (i32.lt_u (local.get $pos) (local.get $len))
                     (then
                        (br_if $loop
                           (call $in_bitset (local.get $set)
                              (array.get_u $string (local.get $s)
                                 (local.get $pos)))))))
               (br $continue))
              ;; GOTO
;;              (call $log_js (string.const "GOTO"))
              (local.set $pc
                 (i32.add
                    (local.get $pc)
                    (i32.shr_s (local.get $instr) (i32.const 8))))
              (br $continue))
             ;; PUSHBACK
;;             (call $log_js (string.const "PUSHBACK"))
             (local.set $stack
                (struct.new $pos
                   (local.get $stack)
                   (i32.add (local.get $pc)
                      (i32.shr_s (local.get $instr) (i32.const 8)))
                   (local.get $pos)))
             (br $continue))
            ;; SETMARK
;;            (call $log_js (string.const "SETMARK"))
            (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
            (local.set $stack
               (struct.new $undo
                  (local.get $stack)
                  (local.get $re_register)
                  (local.get $arg)
                  (array.get $int_array
                     (local.get $re_register) (local.get $arg))))
            (array.set $int_array (local.get $re_register) (local.get $arg)
               (local.get $pos))
            (br $continue))
           ;; CHECKPROGRESS
;;           (call $log_js (string.const "CHECKPROGRESS"))
           (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
           (br_if $backtrack
              (i32.eq (local.get $pos)
                 (array.get $int_array (local.get $re_register)
                    (local.get $arg))))
           (br $continue))
          ;; prefix_match
;;          (call $log_js (string.const "prefix_match"))
          (br_if $ACCEPT (local.get $accept_partial_match)))
         ;; backtrack
;;         (call $log_js (string.const "backtrack"))
         (loop $loop
            (local.set $u
               (ref.cast $undo
                  (block $undo (result (ref $stack))
                     (local.set $p
                        (br_on_cast_fail $undo $pos
                           (br_on_null $reject (local.get $stack))))
                     (local.set $pc (struct.get $pos $pc (local.get $p)))
                     (local.set $pos (struct.get $pos $pos (local.get $p)))
                     (local.set $stack
                        (struct.get $pos $pos_previous (local.get $p)))
                     (br $continue))))
            (array.set $int_array (struct.get $undo $tbl (local.get $u))
                (struct.get $undo $idx (local.get $u))
                (struct.get $undo $val (local.get $u)))
            (local.set $stack (struct.get $undo $undo_previous (local.get $u)))
            (br $loop))))
       ;; ACCEPT
;;       (call $log_js (string.const "ACCEPT"))
       (array.set $int_array
          (local.get $group_end) (i32.const 0) (local.get $pos))
       (local.set $res
          (array.new $block (i31.new (i32.const 0))
             (i32.add (i32.shl (local.get $numgroups) (i32.const 1))
                (i32.const 1))))
       (local.set $i (i32.const 0))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $numgroups))
             (then
                (local.set $j (i32.shl (local.get $i) (i32.const 1)))
                (if (i32.or
                       (i32.lt_s
                          (array.get $int_array (local.get $group_start)
                             (local.get $i))
                          (i32.const 0))
                       (i32.lt_s
                          (array.get $int_array (local.get $group_end)
                             (local.get $i))
                          (i32.const 0)))
                   (then
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 1))
                         (i31.new (i32.const -1)))
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 2))
                         (i31.new (i32.const -1))))
                   (else
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 1))
                         (i31.new
                            (array.get $int_array (local.get $group_start)
                               (local.get $i))))
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 2))
                         (i31.new
                            (array.get $int_array (local.get $group_end)
                               (local.get $i))))))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $loop))))
       (return (local.get $res)))
      ;; reject
;;      (call $log_js (string.const "reject"))
      (i31.new (i32.const 0)))

   (data $search_forward "Str.search_forward")

   (func (export "re_search_forward")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
      (local $s (ref $string))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (ref.cast $string (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast i31 (local.get $vpos))))
      (local.set $len (array.len (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $search_forward
                  (i32.const 0) (i32.const 18)))))
      (loop $loop
         (local.set $res
            (call $re_match
               (local.get $re) (local.get $s) (local.get $pos) (i32.const 0)))
         (if (ref.test $block (local.get $res))
            (then
               (return (local.get $res))))
         (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
         (br_if $loop (i32.le_u (local.get $pos) (local.get $len))))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (data $search_backward "Str.search_backward")

   (func (export "re_search_backward")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
      (local $s (ref $string))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (ref.cast $string (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast i31 (local.get $vpos))))
      (local.set $len (array.len (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $search_backward
                  (i32.const 0) (i32.const 19)))))
      (loop $loop
         (local.set $res
            (call $re_match
               (local.get $re) (local.get $s) (local.get $pos) (i32.const 0)))
         (if (ref.test $block (local.get $res))
            (then
               (return (local.get $res))))
         (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
         (br_if $loop (i32.ge_s (local.get $pos) (i32.const 0))))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (data $string_match "Str.string_match")

   (func (export "re_string_match")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
      (local $s (ref $string))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (ref.cast $string (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast i31 (local.get $vpos))))
      (local.set $len (array.len (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $string_match
                  (i32.const 0) (i32.const 16)))))
      (local.set $res
         (call $re_match
            (local.get $re) (local.get $s) (local.get $pos) (i32.const 0)))
       (if (ref.test $block (local.get $res))
          (then
            (return (local.get $res))))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (data $string_partial_match "Str.string_partial_match")

   (func (export "re_partial_match")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
      (local $s (ref $string))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (ref.cast $string (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast i31 (local.get $vpos))))
      (local.set $len (array.len (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $string_partial_match
                  (i32.const 0) (i32.const 24)))))
      (local.set $res
         (call $re_match
            (local.get $re) (local.get $s) (local.get $pos) (i32.const 1)))
       (if (ref.test $block (local.get $res))
          (then
            (return (local.get $res))))
      (array.new_fixed $block (i31.new (i32.const 0))))

   (func (export "re_replacement_text")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (array.new_fixed $string))
(;
//Provides: re_replacement_text
//Requires: caml_jsbytes_of_string, caml_string_of_jsbytes
//Requires: caml_array_get
//Requires: caml_failwith
// external re_replacement_text: string -> int array -> string -> string
function re_replacement_text(repl,groups,orig) {
  var repl = caml_jsbytes_of_string(repl);
  var len = repl.length;
  var orig = caml_jsbytes_of_string(orig);
  var res = ""; //result
  var n = 0; // current position
  var cur; //current char
  var start, end, c;
  while(n < len){
    cur = repl.charAt(n++);
    if(cur != '\\'){
      res += cur;
    }
    else {
      if(n == len) caml_failwith("Str.replace: illegal backslash sequence");
      cur = repl.charAt(n++);
      switch(cur){
      case '\\':
        res += cur;
        break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c = +cur;
        if (c*2 >= groups.length - 1 )
          caml_failwith("Str.replace: reference to unmatched group" );
        start = caml_array_get(groups,c*2);
        end = caml_array_get(groups, c*2 +1);
        if (start == -1)
          caml_failwith("Str.replace: reference to unmatched group");
        res+=orig.slice(start,end);
        break;
      default:
        res += ('\\'  + cur);
      }
    }
  }
  return caml_string_of_jsbytes(res); }
;)
)

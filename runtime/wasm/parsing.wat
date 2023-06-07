(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (func $get (param $a (ref eq)) (param $i i32) (result i32)
      (local $s (ref $string))
      (local.set $s (ref.cast $string (local.get $a)))
      (local.set $i (i32.add (local.get $i) (local.get $i)))
      (i32.extend16_s
         (i32.or (array.get_u $string (local.get $s) (local.get $i))
            (i32.shl
               (array.get_u $string (local.get $s)
                  (i32.add (local.get $i) (i32.const 1)))
               (i32.const 8)))))

   (global $caml_parser_trace (mut i32) (i32.const 0))

   (global $ERRCODE i32 (i32.const 256))

   (global $START i32 (i32.const 0))
   (global $TOKEN_READ i32 (i32.const 1))
   (global $STACKS_GROWN_1 i32 (i32.const 2))
   (global $STACKS_GROWN_2 i32 (i32.const 3))
   (global $SEMANTIC_ACTION_COMPUTED i32 (i32.const 4))
   (global $ERROR_DETECTED i32 (i32.const 5))
   (global $loop i32 (i32.const 6))
   (global $testshift i32 (i32.const 7))
   (global $shift i32 (i32.const 8))
   (global $shift_recover i32 (i32.const 9))
   (global $reduce i32 (i32.const 10))

   (global $READ_TOKEN i32 (i32.const 0))
   (global $RAISE_PARSE_ERROR i32 (i32.const 1))
   (global $GROW_STACKS_1 i32 (i32.const 2))
   (global $GROW_STACKS_2 i32 (i32.const 3))
   (global $COMPUTE_SEMANTIC_ACTION i32 (i32.const 4))
   (global $CALL_ERROR_FUNCTION i32 (i32.const 5))

   (global $env_s_stack i32 (i32.const 1))
   (global $env_v_stack i32 (i32.const 2))
   (global $env_symb_start_stack i32 (i32.const 3))
   (global $env_symb_end_stack i32 (i32.const 4))
   (global $env_stacksize i32 (i32.const 5))
   (global $env_stackbase i32 (i32.const 6))
   (global $env_curr_char i32 (i32.const 7))
   (global $env_lval i32 (i32.const 8))
   (global $env_symb_start i32 (i32.const 9))
   (global $env_symb_end i32 (i32.const 10))
   (global $env_asp i32 (i32.const 11))
   (global $env_rule_len i32 (i32.const 12))
   (global $env_rule_number i32 (i32.const 13))
   (global $env_sp i32 (i32.const 14))
   (global $env_state i32 (i32.const 15))
   (global $env_errflag i32 (i32.const 16))

   (global $tbl_transl_const i32 (i32.const 2))
   (global $tbl_transl_block i32 (i32.const 3))
   (global $tbl_lhs i32 (i32.const 4))
   (global $tbl_len i32 (i32.const 5))
   (global $tbl_defred i32 (i32.const 6))
   (global $tbl_dgoto i32 (i32.const 7))
   (global $tbl_sindex i32 (i32.const 8))
   (global $tbl_rindex i32 (i32.const 9))
   (global $tbl_gindex i32 (i32.const 10))
   (global $tbl_tablesize i32 (i32.const 11))
   (global $tbl_table i32 (i32.const 12))
   (global $tbl_check i32 (i32.const 13))
   (global $tbl_names_const i32 (i32.const 15))
   (global $tbl_names_block i32 (i32.const 16))

   (func (export "caml_parse_engine")
      (param $vtables (ref eq)) (param $venv (ref eq)) (param $vcmd (ref eq))
      (param $varg (ref eq)) (result (ref eq))
      (local $res i32) (local $n i32) (local $n1 i32) (local $n2 i32)
      (local $m i32)
      (local $state1 i32) (local $sp i32) (local $asp i32) (local $state i32)
      (local $errflag i32)
      (local $tables (ref $block)) (local $env (ref $block)) (local $cmd i32)
      (local $arg (ref $block))
      (local $tbl_defred (ref $string))
      (local $tbl_sindex (ref $string))
      (local $tbl_check (ref $string))
      (local $tbl_rindex (ref $string))
      (local $tbl_table (ref $string))
      (local $tbl_len (ref $string))
      (local $tbl_lhs (ref $string))
      (local $tbl_gindex (ref $string))
      (local $tbl_dgoto (ref $string))
      (local.set $tables (ref.cast $block (local.get $vtables)))
      (local.set $tbl_defred
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_defred))))
      (local.set $tbl_sindex
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_sindex))))
      (local.set $tbl_check
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_check))))
      (local.set $tbl_rindex
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_rindex))))
      (local.set $tbl_table
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_table))))
      (local.set $tbl_len
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_len))))
      (local.set $tbl_lhs
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_lhs))))
      (local.set $tbl_gindex
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_gindex))))
      (local.set $tbl_dgoto
         (ref.cast $string
            (array.get $block (local.get $tables) (global.get $tbl_dgoto))))
      (local.set $env (ref.cast $block (local.get $venv)))
      (local.set $cmd (i31.get_s (ref.cast i31 (local.get $vcmd))))
      (local.set $sp
         (i31.get_s
            (ref.cast i31
               (array.get $block (local.get $env) (global.get $env_sp)))))
      (local.set $state
         (i31.get_s
            (ref.cast i31
               (array.get $block (local.get $env) (global.get $env_state)))))
      (local.set $errflag
         (i31.get_s
            (ref.cast i31
               (array.get $block (local.get $env) (global.get $env_errflag)))))
      (block $exit
       (loop $next
        (block $default
         (block $SEMANTIC_ACTION_COMPUTED
          (block $STACKS_GROWN_2
           (block $reduce
            (block $STACKS_GROWN_1
             (block $shift_recover
              (block $shift
               (block $ERROR_DETECTED
                (block $testshift
                 (block $TOKEN_READ
                  (block $loop
                   (block $START
                    (br_table $START $TOKEN_READ $STACKS_GROWN_1 $STACKS_GROWN_2
                       $SEMANTIC_ACTION_COMPUTED $ERROR_DETECTED $loop
                       $testshift $shift $shift_recover $reduce $default
                       (local.get $cmd)))
                   ;; START:
                   (local.set $state (i32.const 0))
                   (local.set $errflag (i32.const 0))
                   ;; Fall through
                  )
                  ;; loop:
                  (local.set $n
                     (call $get (local.get $tbl_defred) (local.get $state)))
                  (if (i32.ne (local.get $n) (i32.const 0))
                     (then (local.set $cmd (global.get $reduce)) (br $next)))
                  (if (i32.ge_s
                         (i31.get_s
                            (ref.cast i31
                               (array.get $block (local.get $env)
                                  (global.get $env_curr_char))))
                                  (i32.const 0))
                     (then
                        (local.set $cmd (global.get $testshift))
                        (br $next)))
                  (local.set $res (global.get $READ_TOKEN))
                  (br $exit))
                 ;; TOKEN_READ:
                 (block $cont
                    (drop (block $not_block (result (ref eq))
                       (local.set $arg
                          (br_on_cast_fail $not_block $block (local.get $varg)))
                       (array.set $block (local.get $env)
                          (global.get $env_curr_char)
                          (array.get $block
                             (ref.cast $block
                                (array.get $block (local.get $tables)
                                   (global.get $tbl_transl_block)))
                             (i32.add
                                (i31.get_u
                                   (ref.cast i31
                                      (array.get $block
                                         (local.get $arg) (i32.const 0))))
                                (i32.const 1))))
                       (array.set $block (local.get $env) (global.get $env_lval)
                          (array.get $block (local.get $arg) (i32.const 1)))
                       (br $cont)))
                    (array.set $block (local.get $env)
                       (global.get $env_curr_char)
                       (array.get $block
                          (ref.cast $block
                             (array.get $block (local.get $tables)
                                (global.get $tbl_transl_const)))
                          (i32.add
                             (i31.get_u (ref.cast i31 (local.get $varg)))
                             (i32.const 1))))
                    (array.set $block (local.get $env) (global.get $env_lval)
                       (i31.new (i32.const 0)))))
(;ZZZ
                 (if (global.get $caml_parser_trace)
                    (then (call $print_token
                       (local.get $state) (local.get,$varg))))
;)
                 ;; Fall through
                ;; testshift:
                (local.set $n1
                   (call $get (local.get $tbl_sindex) (local.get $state)))
                (local.set $n2
                    (i32.add (local.get $n1)
                       (i31.get_s
                          (ref.cast i31
                             (array.get $block (local.get $env)
                                (global.get $env_curr_char))))))
                (if (i32.and
                       (i32.ne (local.get $n1) (i32.const 0))
                       (i32.ge_s (local.get $n2) (i32.const 0)))
                   (then
                      (if (i32.le_s (local.get $n2)
                             (i31.get_s
                                (ref.cast i31
                                   (array.get $block (local.get $tables)
                                      (global.get $tbl_tablesize)))))
                         (then
                            (if (ref.eq
                                   (i31.new
                                      (call $get (local.get $tbl_check)
                                         (local.get $n2)))
                                   (array.get $block (local.get $env)
                                      (global.get $env_curr_char)))
                               (then
                                  (local.set $cmd (global.get $shift))
                                  (br $next)))))))
                (local.set $n1
                   (call $get (local.get $tbl_rindex) (local.get $state)))
                (local.set $n2
                   (i32.add (local.get $n1)
                      (i31.get_s
                         (ref.cast i31
                            (array.get $block (local.get $env)
                               (global.get $env_curr_char))))))
                (if (i32.and
                       (i32.ne (local.get $n1) (i32.const 0))
                       (i32.ge_s (local.get $n2) (i32.const 0)))
                   (then
                      (if (i32.le_s (local.get $n2)
                             (i31.get_s
                                (ref.cast i31
                                   (array.get $block (local.get $tables)
                                      (global.get $tbl_tablesize)))))
                         (then
                            (if (ref.eq
                                   (i31.new
                                      (call $get (local.get $tbl_check)
                                         (local.get $n2)))
                                   (array.get $block (local.get $env)
                                      (global.get $env_curr_char)))
                               (then
                                  (local.set $n
                                     (call $get (local.get $tbl_table)
                                        (local.get $n2)))
                                  (local.set $cmd (global.get $reduce)
                                  (br $next))))))))
                (if (i32.le_s (local.get $errflag) (i32.const 0))
                   (then
                      (local.set $res (global.get $CALL_ERROR_FUNCTION))
                      (br $exit))))
                ;; Fall through
               ;; ERROR_DETECTED:
               (if (i32.lt_s (local.get $errflag) (i32.const 3))
                  (then
                     (local.set $errflag (i32.const 3))
                     (loop $loop2
                       (local.set $state1
                          (i31.get_s
                             (ref.cast i31
                                (array.get $block
                                   (ref.cast $block
                                      (array.get $block (local.get $env)
                                         (global.get $env_s_stack)))
                                   (i32.add (local.get $sp) (i32.const 1))))))
                       (local.set $n1
                          (call $get (local.get $tbl_sindex)
                             (local.get $state1)))
                       (local.set $n2
                          (i32.add (local.get $n1) (global.get $ERRCODE)))
                       (if (i32.and
                              (i32.ne (local.get $n1) (i32.const 0))
                              (i32.ge_s (local.get $n2) (i32.const 0)))
                          (then
                             (if (i32.le_s (local.get $n2)
                                    (i31.get_s
                                       (ref.cast i31
                                          (array.get $block (local.get $tables)
                                             (global.get $tbl_tablesize)))))
                                (then
                                   (if (i32.eq
                                          (call $get (local.get $tbl_check)
                                             (local.get $n2))
                                          (global.get $ERRCODE))
                                      (then
(;ZZZZ
                                         (if (global.get $caml_parser_trace)
                                            (then
                                               (call $print (
                                         ;;  log("Recovering in state " + state1);
;)
                                         (local.set $cmd
                                            (global.get $shift_recover))
                                         (br $next)))))))
                       ;;if (caml_parser_trace)
                       ;;  log("Discarding state " + state1);
                       (if (i32.le_s (local.get $sp)
                              (i31.get_s
                                 (ref.cast i31
                                    (array.get $block (local.get $env)
                                       (global.get $env_stackbase)))))
                          (then
                             ;;if (caml_parser_trace)
                             ;;  log("No more states to discard");
                             (return (i31.new (global.get $RAISE_PARSE_ERROR)))))
                       (local.set $sp (i32.sub (local.get $sp) (i32.const 1)))
                       (br $loop2)))
                  (else
                     (if (ref.eq
                            (array.get $block (local.get $env)
                               (global.get $env_curr_char))
                            (i31.new (i32.const 0)))
                        (then
                           (return (i31.new (global.get $RAISE_PARSE_ERROR)))))
                     ;;if (caml_parser_trace)
                     ;;  log("Discarding last token read");
                     (array.set $block (local.get $env)
                        (global.get $env_curr_char)
                        (i31.new (i32.const -1)))
                     (local.set $cmd (global.get $loop))
                     (br $next))))
              ;; shift:
              (array.set $block (local.get $env) (global.get $env_curr_char)
                 (i31.new (i32.const -1)))
              (if (i32.gt_s (local.get $errflag) (i32.const 0))
                 (then
                    (local.set $errflag
                       (i32.sub (local.get $errflag) (i32.const 1))))))
              ;; Fall through
             ;; shift_recover:
             ;; if (caml_parser_trace)
             ;;   log("State " + state + ": shift to state " + tables.table[n2]);
             (local.set $state
                (call $get (local.get $tbl_table) (local.get $n2)))
             (local.set $sp (i32.add (local.get $sp) (i32.const 1)))
             (if (i32.ge_s (local.get $sp)
                    (i31.get_s
                       (ref.cast i31
                           (array.get $block (local.get $env)
                              (global.get $env_stacksize)))))
                (then
                   (local.set $res (global.get $GROW_STACKS_1))
                   (br $exit))))
             ;; Fall through
            ;; STACKS_GROWN_1:
            (array.set $block
               (ref.cast $block
                  (array.get $block (local.get $env) (global.get $env_s_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (i31.new (local.get $state)))
            (array.set $block
               (ref.cast $block
                  (array.get $block (local.get $env) (global.get $env_v_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (array.get $block (local.get $env) (global.get $env_lval)))
            (array.set $block
               (ref.cast $block
                  (array.get $block (local.get $env)
                     (global.get $env_symb_start_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (array.get $block (local.get $env) (global.get $env_symb_start)))
            (array.set $block
               (ref.cast $block
                  (array.get $block (local.get $env)
                     (global.get $env_symb_end_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (array.get $block (local.get $env) (global.get $env_symb_end)))
            (local.set $cmd (global.get $loop))
            (br $next))
           ;; reduce:
           ;;if (caml_parser_trace)
           ;;  log("State " + state + ": reduce by rule " + n);
           (local.set $m (call $get (local.get $tbl_len) (local.get $n)))
           (array.set $block (local.get $env) (global.get $env_asp)
              (i31.new (local.get $sp)))
           (array.set $block (local.get $env) (global.get $env_rule_number)
              (i31.new (local.get $n)))
           (array.set $block (local.get $env) (global.get $env_rule_len)
              (i31.new (local.get $m)))
           (local.set $sp
              (i32.add (local.get $sp) (i32.sub (i32.const 1) (local.get $m))))
           (local.set $m (call $get (local.get $tbl_lhs) (local.get $n)))
           (local.set $state1
              (i31.get_s
                 (ref.cast i31
                    (array.get $block
                       (ref.cast $block
                          (array.get $block (local.get $env)
                             (global.get $env_s_stack)))
                       (local.get $sp)))))
           (local.set $n1 (call $get (local.get $tbl_gindex) (local.get $m)))
           (local.set $n2 (i32.add (local.get $n1) (local.get $state1)))
           (block $cont
              (if (i32.and
                     (i32.ne (local.get $n1) (i32.const 0))
                     (i32.ge_s (local.get $n2) (i32.const 0)))
                 (then
                    (if (i32.le_s (local.get $n2)
                           (i31.get_s
                              (ref.cast i31
                                 (array.get $block (local.get $tables)
                                    (global.get $tbl_tablesize)))))
                       (then
                          (if (i32.eq
                                 (call $get (local.get $tbl_check)
                                    (local.get $n2))
                                 (local.get $state1))
                             (then
                                (local.set $state
                                   (call $get (local.get $tbl_table)
                                      (local.get $n2)))
                                (br $cont)))))))
              (local.set $state
                 (call $get (local.get $tbl_dgoto) (local.get $m))))
           (if (i32.ge_s (local.get $sp)
                  (i31.get_s
                     (ref.cast i31
                        (array.get $block (local.get $env)
                           (global.get $env_stacksize)))))
              (then
                 (local.set $res (global.get $GROW_STACKS_2))
                 (br $exit))))
           ;; Fall through
          ;; STACKS_GROWN_2:
          (local.set $res (global.get $COMPUTE_SEMANTIC_ACTION))
          (br $exit))
         ;; SEMANTIC_ACTION_COMPUTED:
         (array.set $block
            (ref.cast $block
               (array.get $block (local.get $env) (global.get $env_s_stack)))
            (i32.add (local.get $sp) (i32.const 1))
            (i31.new (local.get $state)))
         (array.set $block
            (ref.cast $block
               (array.get $block (local.get $env) (global.get $env_v_stack)))
            (i32.add (local.get $sp) (i32.const 1))
            (local.get $varg))
         (local.set $asp
            (i31.get_s
               (ref.cast i31
                  (array.get $block (local.get $env) (global.get $env_asp)))))
         (array.set $block
            (ref.cast $block
               (array.get $block (local.get $env)
                  (global.get $env_symb_end_stack)))
            (i32.add (local.get $sp) (i32.const 1))
            (array.get $block
               (ref.cast $block
                  (array.get $block (local.get $env)
                     (global.get $env_symb_end_stack)))
               (i32.add (local.get $asp) (i32.const 1))))
         (if (i32.gt_s (local.get $sp) (local.get $asp))
            (then
            ;; This is an epsilon production. Take symb_start equal to symb_end.
               (array.set $block
                  (ref.cast $block
                     (array.get $block (local.get $env)
                        (global.get $env_symb_start_stack)))
                  (i32.add (local.get $sp) (i32.const 1))
                  (array.get $block
                     (ref.cast $block
                        (array.get $block (local.get $env)
                           (global.get $env_symb_end_stack)))
                     (i32.add (local.get $asp) (i32.const 1))))))
         (local.set $cmd (global.get $loop))
         (br $next))
        ;; default:
        (return (i31.new (global.get $RAISE_PARSE_ERROR)))))
      ;; SAVE
      (array.set $block (local.get $env) (global.get $env_sp)
         (i31.new (local.get $sp)))
      (array.set $block (local.get $env) (global.get $env_state)
         (i31.new (local.get $state)))
      (array.set $block (local.get $env) (global.get $env_errflag)
         (i31.new (local.get $errflag)))
      (i31.new (local.get $res)))

   (func (export "caml_set_parser_trace") (param (ref eq)) (result (ref eq))
      (local $oldflag i32)
      (local.set $oldflag (global.get $caml_parser_trace))
      (global.set $caml_parser_trace (i31.get_s (ref.cast i31 (local.get 0))))
      (i31.new (local.get $oldflag)))
)

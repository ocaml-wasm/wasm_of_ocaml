(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "bindings" "open"
      (func $open (param anyref) (param i32) (param i32) (result i32)))
   (import "bindings" "close" (func $close (param i32)))
   (import "bindings" "write"
      (func $write
         (param i32) (param (ref extern)) (param i32) (param i32) (result i32)))
   (import "bindings" "read"
      (func $read
         (param i32) (param (ref extern)) (param i32) (param i32) (result i32)))
   (import "bindings" "ta_new" (func $ta_new (param i32) (result (ref extern))))
   (import "bindings" "ta_copy"
      (func $ta_copy (param (ref extern)) (param i32) (param i32) (param i32)))
   (import "bindings" "ta_set_ui8"
      (func $ta_set_ui8 (param (ref extern)) (param i32) (param i32))) ;; ZZZ ??
   (import "bindings" "ta_get_ui8"
      (func $ta_get_ui8 (param (ref extern)) (param i32) (result i32)))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))

   (global $IO_BUFFER_SIZE i32 (i32.const 65536))

   (type $channel
      (struct
         (field (mut i32)) ;; fd
         (field (mut i64)) ;; offset
         (field (mut (ref extern))) ;; buffer
         (field (mut i32)) ;; current position in buffer
         (field (mut i32)) ;; logical end of the buffer (for input)
         (field (mut i32)) ;; buffer size
         (field (mut i32)))) ;; flags

   (type $open_flags (array i8))
   ;;  1 O_RDONLY
   ;;  2 O_WRONLY
   ;;  4 O_APPEND
   ;;  8 O_CREAT
   ;; 16 O_TRUNC
   ;; 32 O_EXCL
   ;; 64 O_NONBLOCK
   (global $sys_open_flags (ref $open_flags)
      (array.new_fixed $open_flags
         (i32.const 1) (i32.const 2) (i32.const 6) (i32.const 8) (i32.const 16)
         (i32.const 32) (i32.const 0) (i32.const 0) (i32.const 64)))

   (func $convert_flag_list (param $vflags (ref eq)) (result i32)
      (local $flags i32)
      (local $cons (ref $block))
      (loop $loop
         (drop (block $done (result (ref eq))
            (local.set $cons (br_on_cast_fail $done $block (local.get $vflags)))
            (local.set $flags
               (i32.or (local.get $flags)
                  (array.get_u $open_flags (global.get $sys_open_flags)
                     (i31.get_u
                        (ref.cast i31
                           (array.get $block
                              (local.get $cons) (i32.const 1)))))))
            (local.set $vflags
               (array.get $block (local.get $cons) (i32.const 2)))
            (br $loop))))
      (local.get $flags))

   (func (export "caml_sys_open")
      (param $path (ref eq)) (param $flags (ref eq)) (param $perm (ref eq))
      (result (ref eq))
      (i31.new
         (call $open
            (call $unwrap (call $caml_jsstring_of_string (local.get $path)))
            (call $convert_flag_list (local.get $flags))
            (i31.get_u (ref.cast i31 (local.get $perm))))))

   (func (export "caml_sys_close") (param (ref eq)) (result (ref eq))
      (call $close (i31.get_u (ref.cast i31 (local.get 0))))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_set_channel_name")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_out_channels_list")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;; (call $log_js (string.const "caml_ml_out_channels_list"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_in")
      (param $fd (ref eq)) (result (ref eq))
      (struct.new $channel
         (i31.get_u (ref.cast i31 (local.get $fd)))
         (i64.const 0) ;; ZZZ lseek?
         (call $ta_new (global.get $IO_BUFFER_SIZE))
         (i32.const 0)
         (i32.const 0)
         (global.get $IO_BUFFER_SIZE)
         (i32.const 0)))

   (func (export "caml_ml_open_descriptor_out")
      (param $fd (ref eq)) (result (ref eq))
      (struct.new $channel
         (i31.get_u (ref.cast i31 (local.get $fd)))
         (i64.const 0) ;; ZZZ
         (call $ta_new (global.get $IO_BUFFER_SIZE))
         (i32.const 0)
         (i32.const -1)
         (global.get $IO_BUFFER_SIZE)
         (i32.const 0)))

   (func (export "caml_ml_close_channel")
      (param (ref eq)) (result (ref eq))
      (local $ch (ref $channel))
      (local $fd i32)
      (local.set $ch (ref.cast $channel (local.get 0)))
      (struct.set $channel 3 (local.get $ch) (i32.const 0))
      (struct.set $channel 4 (local.get $ch) (i32.const 0))
      (struct.set $channel 5 (local.get $ch) (i32.const 0))
      (local.set $fd (struct.get $channel 0 (local.get $ch)))
      (if (i32.ne (local.get $fd) (i32.const -1))
         (then
            (struct.set $channel 0 (local.get $ch) (i32.const -1))
            (call $close (local.get $fd))))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input")
      (param $vch (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $ch (ref $channel)) (local $s (ref $string))
      (local $pos i32) (local $len i32) (local $curr i32)
      (local $i i32) (local $avail i32) (local $nread $i32)
      (local $buf (ref extern))
      (local.set $ch (ref.cast $channel (local.get $vch)))
      (local.set $s (ref.cast $string (local.get $vs)))
      (local.set $pos (i31.get_u (ref.cast i31 (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast i31 (local.get $vlen))))
      (local.set $buf (struct.get $channel 2 (local.get $ch)))
      (local.set $curr (struct.get $channel 3 (local.get $ch)))
      (local.set $avail
         (i32.sub (struct.get $channel 4 (local.get $ch)) (local.get $curr)))
      (if (i32.gt_u (local.get $len) (local.get $avail))
         (then
            (if (i32.gt_u (local.get $avail) (i32.const 0))
               (then
                  (local.set $len (local.get $avail)))
               (else
                  (local.set $nread
                     (call $read
                        (struct.get $channel 0 (local.get $ch))
                        (local.get $buf)
                        (i32.const 0)
                        (struct.get $channel 5 (local.get $ch))))
                  (struct.set $channel 1 (local.get $ch)
                     (i64.add (struct.get $channel 1 (local.get $ch))
                        (i64.extend_i32_u (local.get $nread))))
                  (struct.set $channel 4 (local.get $ch) (local.get $nread))
                  (local.set $curr (i32.const 0))
                  (if (i32.gt_u (local.get $len) (local.get $nread))
                     (then (local.set $len (local.get $nread))))))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s)
                  (i32.add (local.get $pos) (local.get $i))
                  (call $ta_get_ui8 (local.get $buf)
                     (i32.add (local.get $curr) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.set $channel 3 (local.get $ch)
         (i32.add (local.get $curr) (local.get $len)))
      (i31.new (local.get $len)))

   (func (export "caml_input_value") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_input_value"))
      (unreachable))

   (func (export "caml_ml_input_char")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_char"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_int")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_int"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_pos_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_pos_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_in_64")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_in_64"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_seek_out")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_seek_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input_scan_line")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input_scan_line"))
      (i31.new (i32.const 0)))

   (func $caml_ml_flush (export "caml_ml_flush") (param $ch (ref eq)) (result (ref eq))
      (loop $loop
         (br_if $loop
            (i32.eqz
               (call $caml_flush_partial (ref.cast $channel (local.get $ch))))))
      (i31.new (i32.const 0)))

   (func $caml_flush_partial (param $ch (ref $channel)) (result i32)
      (local $towrite i32) (local $written i32)
      (local $buf (ref extern))
      (local.set $towrite (struct.get $channel 3 (local.get $ch)))
      (if (i32.gt_u (local.get $towrite) (i32.const 0))
         (then
            (local.set $buf (struct.get $channel 2 (local.get $ch)))
            (local.set $written
               (call $write
                  (struct.get $channel 0 (local.get $ch))
                  (local.get $buf)
                  (i32.const 0)
                  (local.get $towrite)))
            (struct.set $channel 1 (local.get $ch)
               (i64.add (struct.get $channel 1 (local.get $ch))
                  (i64.extend_i32_u (local.get $written))))
            (local.set $towrite
               (i32.sub (local.get $towrite) (local.get $written)))
            (if (i32.gt_u (local.get $towrite) (i32.const 0))
               (then
                  (call $ta_copy (local.get $buf)
                     (i32.const 0) (local.get $written) (local.get $towrite))))
            (struct.set $channel 3 (local.get $ch) (local.get $towrite))))
      (i32.eqz (local.get $towrite)))

   (func $caml_putblock
      (param $ch (ref $channel)) (param $s (ref $string)) (param $pos i32)
      (param $len i32) (result i32)
      (local $free i32) (local $curr i32) (local $i i32)
      (local $buf (ref extern))
      (local.set $curr (struct.get $channel 3 (local.get $ch)))
      (local.set $free
         (i32.sub (struct.get $channel 5 (local.get $ch)) (local.get $curr)))
      (if (i32.ge_u (local.get $len) (local.get $free))
         (then (local.set $len (local.get $free))))
      (local.set $buf (struct.get $channel 2 (local.get $ch)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (call $ta_set_ui8 (local.get $buf)
                  (i32.add (local.get $curr) (local.get $i))
                  (array.get_u $string (local.get $s)
                     (i32.add (local.get $pos) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.set $channel 3 (local.get $ch)
         (i32.add (local.get $curr) (local.get $len)))
      (if (i32.ge_u (local.get $len) (local.get $free))
         (then (drop (call $caml_flush_partial (local.get $ch)))))
      (local.get $len))

   (export "caml_ml_output_bytes" (func $caml_ml_output))
   (func $caml_ml_output (export "caml_ml_output")
      (param $ch (ref eq)) (param $s (ref eq)) (param $vpos (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $pos i32) (local $len i32) (local $written i32)
      (local.set $pos (i31.get_u (ref.cast i31 (local.get $vpos))))
      (local.set $len (i31.get_u (ref.cast i31 (local.get $vlen))))
      (loop $loop
         (if (i32.gt_u (local.get $len) (i32.const 0))
            (then
               (local.set $written
                  (call $caml_putblock (ref.cast $channel (local.get $ch))
                     (ref.cast $string (local.get $s))
                     (local.get $pos) (local.get $len)))
               (local.set $pos (i32.add (local.get $pos) (local.get $written)))
               (local.set $len (i32.sub (local.get $len) (local.get $written)))
               (br $loop))))
      (drop (call $caml_ml_flush (local.get 0))) ;; ZZZ
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_char")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;;(call $log_js (string.const "caml_ml_output_char"))
      (return_call $caml_ml_output (local.get 0)
         (array.new $string
            (i31.get_u (ref.cast i31 (local.get 1))) (i32.const 1))
         (i31.new (i32.const 0)) (i31.new (i32.const 1))))

   (func (export "caml_output_value")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_output_value"))
      (unreachable))

   (func (export "caml_ml_output_int")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_int"))
      (i31.new (i32.const 0)))
)

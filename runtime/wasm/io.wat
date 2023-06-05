(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "bindings" "write" (func $write (param (ref string))))

   (type $string (array (mut i8)))

   (func (export "caml_sys_open")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_open"))
      (call $log_js (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_close")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_close"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_set_channel_name")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_set_channel_name"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_out_channels_list")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;; (call $log_js (string.const "caml_ml_out_channels_list"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_in")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;; (call $log_js (string.const "caml_ml_open_descriptor_in"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_open_descriptor_out")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;; (call $log_js (string.const "caml_ml_open_descriptor_out"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_close_channel")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_close_channel"))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_input")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_input"))
      (i31.new (i32.const 0)))

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

   (func (export "caml_ml_flush") (param (ref eq)) (result (ref eq))
      ;; ZZZ
      ;; (call $log_js (string.const "caml_ml_flush"))
      (i31.new (i32.const 0)))

   (func $caml_ml_output (export "caml_ml_output")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      ;; (call $log_js (string.const "caml_ml_output"))
      (local $s (ref $string))
      (local.set $s (ref.cast $string (local.get 1)))
      (call $write
         (string.new_wtf8_array replace (local.get $s)
            (i31.get_u (ref.cast i31 (local.get 2)))
            (i31.get_u (ref.cast i31 (local.get 3)))))
      (i31.new (i32.const 0)))

   (func (export "caml_ml_output_bytes")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_ml_output_bytes"))
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

(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (type $string (array (mut i8)))

   (func (export "caml_md5_string")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_string"))
      (array.new $string (i32.const 0) (i32.const 16)))

   (func (export "caml_md5_chan")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_md5_chan"))
      (array.new $string (i32.const 0) (i32.const 16)))
)

(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (func (export "caml_parse_engine")
      (param (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_parse_engine"))
      (i31.new (i32.const 0)))
)

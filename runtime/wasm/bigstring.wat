(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (func (export "caml_bigstring_blit_bytes_to_ba")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_blit_bytes_to_ba"))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_bytes")
     (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_blit_ba_to_bytes"))
      (i31.new (i32.const 0)))

   (func (export "caml_bigstring_blit_ba_to_ba")
      (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_bigstring_blit_ba_to_ba"))
      (i31.new (i32.const 0)))
)

(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "getcwd" (func $getcwd (result anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))

   (type $string (array (mut i8)))

   (func (export "caml_sys_getcwd")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_string_of_jsstring (call $wrap (call $getcwd))))

   (func (export "caml_sys_mkdir")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_mkdir"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_read_directory")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_read_directory"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_remove")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_remove"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_rename")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_rename"))
      (i31.new (i32.const 0)))

   (func (export "caml_sys_file_exists")
      (param (ref eq)) (result (ref eq))
      ;; ZZZ
      (call $log_js (string.const "caml_sys_file_exists"))
      (i31.new (i32.const 0)))

   (func (export "caml_fs_init") (result (ref eq))
      (i31.new (i32.const 0)))
)

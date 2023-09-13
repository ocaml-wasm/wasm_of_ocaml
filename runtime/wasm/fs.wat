(module
   (import "bindings" "log" (func $log_js (param anyref)))
   (import "bindings" "getcwd" (func $getcwd (result anyref)))
   (import "bindings" "chdir" (func $chdir (param anyref)))
   (import "bindings" "mkdir" (func $mkdir (param anyref) (param i32)))
   (import "bindings" "unlink" (func $unlink (param anyref)))
   (import "bindings" "readdir"
      (func $readdir (param anyref) (result (ref extern))))
   (import "bindings" "file_exists"
      (func $file_exists (param anyref) (result (ref eq))))
   (import "bindings" "rename" (func $rename (param anyref) (param anyref)))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_js_to_string_array"
      (func $caml_js_to_string_array (param $a (ref extern)) (result (ref eq))))
   (import "fail" "caml_raise_sys_error"
      (func $caml_raise_sys_error (param (ref eq))))
   (import "io" "caml_ml_open_descriptor_in"
      (func $caml_ml_open_descriptor_in (param (ref eq)) (result (ref eq))))
   (import "io" "caml_ml_channel_size"
      (func $caml_ml_channel_size (param (ref eq)) (result (ref eq))))
   (import "io" "caml_really_getblock"
      (func $caml_really_getblock
         (param (ref eq)) (param (ref $string)) (param i32) (param i32)))
   (import "io" "caml_sys_open"
      (func $caml_sys_open
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "sys" "caml_handle_sys_error"
      (func $caml_handle_sys_error (param externref)))

   (type $string (array (mut i8)))
   (type $block (array (mut (ref eq))))

   (func (export "caml_sys_getcwd")
      (param (ref eq)) (result (ref eq))
      (return_call $caml_string_of_jsstring (call $wrap (call $getcwd))))

   (func (export "caml_sys_chdir")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $chdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_mkdir")
      (param $name (ref eq)) (param $perm (ref eq)) (result (ref eq))
      (try
         (do
            (call $mkdir
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))
               (i31.get_u (ref.cast (ref i31) (local.get $perm)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_read_directory")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (return
               (call $caml_js_to_string_array
                  (call $readdir
                     (call $unwrap
                        (call $caml_jsstring_of_string (local.get $name)))))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))
            (return (i31.new (i32.const 0))))))

   (func (export "caml_sys_remove")
      (param $name (ref eq)) (result (ref eq))
      (try
         (do
            (call $unlink
               (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_rename")
      (param $o (ref eq)) (param $n (ref eq)) (result (ref eq))
      (try
         (do
            (call $rename
               (call $unwrap (call $caml_jsstring_of_string (local.get $o)))
               (call $unwrap (call $caml_jsstring_of_string (local.get $n)))))
         (catch $javascript_exception
            (call $caml_handle_sys_error (pop externref))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_sys_file_exists")
      (param $name (ref eq)) (result (ref eq))
      (return_call $file_exists
         (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))

   (data $no_such_file ": No such file or directory")

   (func $caml_raise_no_such_file (param $vname (ref eq))
      (local $name (ref $string)) (local $msg (ref $string))
      (local $len i32)
      (local.set $name (ref.cast (ref $string) (local.get $vname)))
      (local.set $len (array.len (local.get $name)))
      (local.set $msg
         (array.new $string (i32.const 0)
            (i32.add (local.get $len) (i32.const 27))))
      (array.copy $string $string
         (local.get $msg) (i32.const 0)
         (local.get $name) (i32.const 0)
         (local.get $len))
      (array.init_data $string $no_such_file
         (local.get $msg) (local.get $len) (i32.const 0) (i32.const 27))
      (call $caml_raise_sys_error (local.get $msg)))

   (func (export "caml_read_file_content")
      (param $name (ref eq)) (result (ref eq))
      (local $ch (ref eq)) (local $len i32) (local $res (ref $string))
      (call $log_js (string.const "caml_read_file_content"))
      ;; ZZZ
      (call $caml_raise_no_such_file (local.get $name))
      (local.set $ch
         (call $caml_ml_open_descriptor_in
            (call $caml_sys_open (local.get $name)
               (array.new_fixed $block 3 (ref.i31 (i32.const 0))
                  (ref.i31 (i32.const 0)) ;; Open_rdonly
                  (ref.i31 (i32.const 0)))
               (ref.i31 (i32.const 438))))) ;; 0o666
      (local.set $len
         (i31.get_u
            (ref.cast (ref i31) (call $caml_ml_channel_size (local.get $ch)))))
      (local.set $res (array.new $string (i32.const 0) (local.get $len)))
      (call $caml_really_getblock
         (local.get $ch) (local.get $res) (i32.const 0) (local.get $len))
      (local.get $res))

   (func (export "caml_fs_init") (result (ref eq))
      (ref.i31 (i32.const 0)))
)

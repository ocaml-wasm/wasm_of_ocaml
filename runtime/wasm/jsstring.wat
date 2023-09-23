(module
   (import "bindings" "log" (func $log_js (param anyref)))

   (import "bindings" "read_string"
      (func $read_string (param i32) (param i32) (result anyref)))
   (import "bindings" "write_string"
      (func $write_string (param anyref) (result i32)))

   (type $string (array (mut i8)))
   (type $js (struct (field anyref)))

   (memory (export "caml_buffer") 1)

(;
   (func $jsstring_of_substring (export "jsstring_of_substring")
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (result (ref eq))
      (struct.new $js
         (string.new_lossy_utf8_array (local.get $s) (local.get $pos)
            (i32.add (local.get $pos) (local.get $len)))))
;)
   (func $jsstring_of_substring (export "jsstring_of_substring")
      (param $s (ref $string)) (param $pos i32) (param $len i32)
      (result (ref eq))
      (local $i i32)
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (i32.store8 (local.get $i)
                  (array.get_u $string (local.get $s)
                     (i32.add (local.get $pos) (local.get $i))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (struct.new $js
         (call $read_string (local.get $len) (i32.const 0))))

(;
   (func $string_of_jsstring (export "string_of_jsstring")
      (param $s (ref string)) (param $ofs i32) (result (ref $string))
      (local $l i32)
      (local $s' (ref $string))
      (local.set $l (string.measure_wtf8 (local.get $s)))
      (local.set $s'
         (array.new $string
            (i32.const 0) (i32.add (local.get $l) (local.get $ofs))))
      (drop (string.encode_lossy_utf8_array
               (local.get $s) (local.get $s') (local.get $ofs)))
      (local.get $s'))
;)
   (func $string_of_jsstring (export "string_of_jsstring")
      (param $s (ref string)) (param $ofs i32) (result (ref $string))
      (local $i i32) (local $len i32)
      (local $s' (ref $string))
      (local.set $len (call $write_string (local.get $s)))
      (local.set $s'
         (array.new $string
            (i32.const 0) (i32.add (local.get $len) (local.get $ofs))))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $len))
            (then
               (array.set $string (local.get $s')
                  (i32.add (local.get $ofs) (local.get $i))
                  (i32.load8_u (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $s'))
)

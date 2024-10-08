let read_file ic = really_input_string ic (in_channel_length ic)

let () =
  let () = set_binary_mode_out stdout true in
  Format.printf
    "let wasm_runtime = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(1))));
  Format.printf
    "let js_runtime = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(2))));
  Format.printf
    "let dependencies = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(3))))

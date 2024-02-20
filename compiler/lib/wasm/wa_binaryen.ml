open! Stdlib

let command cmdline =
  let cmdline = String.concat ~sep:" " cmdline in
  let res = Sys.command cmdline in
  if res = 127 then raise (Sys_error cmdline);
  assert (res = 0)
(*ZZZ better error message *)

let remove_file file = try Sys.remove file with Sys_error _ -> ()

let gen_file file f =
  let f_tmp =
    Filename.temp_file_name
      ~temp_dir:(Filename.dirname file)
      (Filename.basename file)
      ".tmp"
  in
  try
    f f_tmp;
    remove_file file;
    Sys.rename f_tmp file
  with exc ->
    remove_file file;
    raise exc

let write_file ~name ~contents =
  let ch = open_out name in
  output_string ch contents;
  close_out ch

let with_intermediate_file ?(keep = false) name f =
  match f name with
  | res ->
      if not keep then remove_file name;
      res
  | exception e ->
      remove_file name;
      raise e

(***)

let common_options ~debuginfo =
  let l =
    [ "--enable-gc"
    ; "--enable-multivalue"
    ; "--enable-exception-handling"
    ; "--enable-reference-types"
    ; "--enable-tail-call"
    ; "--enable-bulk-memory"
    ; "--enable-nontrapping-float-to-int"
    ; "--enable-strings"
    ]
  in
  if debuginfo then "-g" :: l else l

let link ~debuginfo ~runtime_files ~input_files ~output_file =
  command
    ("wasm-merge"
    :: (common_options ~debuginfo
       @ List.flatten
           (List.map
              ~f:(fun runtime_file -> [ Filename.quote runtime_file; "env" ])
              runtime_files)
       @ List.flatten
           (List.map
              ~f:(fun input_file -> [ Filename.quote input_file; "OCaml" ])
              input_files)
       @ [ "-o"; Filename.quote output_file ]))

let generate_dependencies ~dependencies primitives =
  Yojson.Basic.to_string
    (`List
      (StringSet.fold
         (fun nm s ->
           `Assoc
             [ "name", `String ("js:" ^ nm)
             ; "import", `List [ `String "js"; `String nm ]
             ]
           :: s)
         primitives
         (Yojson.Basic.Util.to_list (Yojson.Basic.from_string dependencies))))

let filter_unused_primitives primitives usage_file =
  let ch = open_in usage_file in
  let s = ref primitives in
  (try
     while true do
       let l = input_line ch in
       match String.drop_prefix ~prefix:"unused: js:" l with
       | Some nm -> s := StringSet.remove nm !s
       | None -> ()
     done
   with End_of_file -> ());
  !s

let dead_code_elimination ~debuginfo ~dependencies ~input_file ~output_file =
  with_intermediate_file (Filename.temp_file "deps" ".json")
  @@ fun deps_file ->
  with_intermediate_file (Filename.temp_file "usage" ".txt")
  @@ fun usage_file ->
  let primitives = Linker.get_provided () in
  write_file ~name:deps_file ~contents:(generate_dependencies ~dependencies primitives);
  command
    ("wasm-metadce"
    :: (common_options ~debuginfo
       @ [ "--graph-file"
         ; Filename.quote deps_file
         ; Filename.quote input_file
         ; "-o"
         ; Filename.quote output_file
         ; ">"
         ; Filename.quote usage_file
         ]));
  filter_unused_primitives primitives usage_file

let optimization_options =
  [| [ "-O2"; "--skip-pass=inlining-optimizing" ]
   ; [ "-O2"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
   ; [ "-O3"; "--traps-never-happen" ]
  |]

let optimize ~debuginfo ~profile ~input_file ~output_file =
  let level =
    match profile with
    | None -> 1
    | Some p -> fst (List.find ~f:(fun (_, p') -> Poly.equal p p') Driver.profiles)
  in
  command
    ("wasm-opt"
    :: (common_options ~debuginfo
       @ optimization_options.(level - 1)
       @ [ Filename.quote input_file; "-o"; Filename.quote output_file ]))

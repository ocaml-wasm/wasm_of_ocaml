open! Stdlib

let debug = Debug.find "binaryen"

let command cmdline =
  let cmdline = String.concat ~sep:" " cmdline in
  if debug () then Format.eprintf "+ %s@." cmdline;
  let res = Sys.command cmdline in
  if res <> 0 then failwith ("the following command terminated unsuccessfully: " ^ cmdline)

let common_options () =
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
  if Config.Flag.pretty () then "-g" :: l else l

let link ~enable_source_maps ~runtime_files ~input_files ~output_file =
  command
    ("wasm-merge"
    :: (common_options ()
       @ List.flatten
           (List.map
              ~f:(fun runtime_file -> [ Filename.quote runtime_file; "env" ])
              runtime_files)
       @ List.flatten
           (List.map
              ~f:(fun input_file -> [ Filename.quote input_file; "OCaml" ])
              input_files)
       @ [ "-o"; Filename.quote output_file ]
       @
       if enable_source_maps
       then [ "--output-source-map"; Filename.quote (output_file ^ ".map") ]
       else []))

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

let dead_code_elimination ~dependencies ~enable_source_maps ~input_file ~output_file =
  Fs.with_intermediate_file (Filename.temp_file "deps" ".json")
  @@ fun deps_file ->
  Fs.with_intermediate_file (Filename.temp_file "usage" ".txt")
  @@ fun usage_file ->
  let primitives = Linker.get_provided () in
  Fs.write_file ~name:deps_file ~contents:(generate_dependencies ~dependencies primitives);
  command
    ("wasm-metadce"
    :: (common_options ()
       @ [ "--graph-file"; Filename.quote deps_file; Filename.quote input_file ]
       @ (if enable_source_maps
          then [ "--input-source-map"; Filename.quote (input_file ^ ".map") ]
          else [])
       @ [ "-o"; Filename.quote output_file ]
       @ (if enable_source_maps
          then [ "--output-source-map"; Filename.quote (output_file ^ ".map") ]
          else [])
       @ [ ">"; Filename.quote usage_file ]));
  filter_unused_primitives primitives usage_file

let optimization_options =
  [| [ "-O2"; "--traps-never-happen" ]
   ; [ "-O2"; "--traps-never-happen" ]
   ; [ "-O3"; "--traps-never-happen" ]
  |]

let optimize
    ~profile
    ?input_sourcemap_file
    ?output_sourcemap_file
    ?sourcemap_url
    ~input_file
    ~output_file
    () =
  let level =
    match profile with
    | None -> 1
    | Some p -> fst (List.find ~f:(fun (_, p') -> Poly.equal p p') Driver.profiles)
  in
  let sourcemap_url =
    match sourcemap_url with
    | Some url -> Some url
    | None -> output_sourcemap_file
  in
  command
    ("wasm-opt"
     :: (common_options ()
        @ optimization_options.(level - 1)
        @ [ Filename.quote input_file; "-o"; Filename.quote output_file ])
    @ (match input_sourcemap_file with
      | Some sourcemap_file -> [ "--input-source-map"; Filename.quote sourcemap_file ]
      | None -> [])
    @ (match output_sourcemap_file with
      | Some sourcemap_file -> [ "--output-source-map"; Filename.quote sourcemap_file ]
      | None -> [])
    @
    match sourcemap_url with
    | Some sourcemap_url -> [ "--output-source-map-url"; Filename.quote sourcemap_url ]
    | None -> [])

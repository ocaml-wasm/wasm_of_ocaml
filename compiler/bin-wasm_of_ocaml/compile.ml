(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let () = Sys.catch_break true

let command cmdline =
  let cmdline = String.concat ~sep:" " cmdline in
  let res = Sys.command cmdline in
  if res = 127 then raise (Sys_error cmdline);
  assert (res = 0)
(*ZZZ*)

let gen_file file f =
  let f_tmp =
    Filename.temp_file_name
      ~temp_dir:(Filename.dirname file)
      (Filename.basename file)
      ".tmp"
  in
  try
    f f_tmp;
    (try Sys.remove file with Sys_error _ -> ());
    Sys.rename f_tmp file
  with exc ->
    (try Sys.remove f_tmp with Sys_error _ -> ());
    raise exc

let write_file name contents =
  let ch = open_out name in
  output_string ch contents;
  close_out ch

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename with Sys_error _msg -> ()

let with_intermediate_file ?(keep = false) name f =
  match f name with
  | res ->
      if not keep then remove_file name;
      res
  | exception e ->
      remove_file name;
      raise e

let output_gen output_file f =
  Code.Var.set_pretty true;
  Code.Var.set_stable (Config.Flag.stable_var ());
  Filename.gen_file output_file f

let common_binaryen_options () =
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

let link ~enable_source_maps runtime_files input_file output_file =
  command
    ("wasm-merge"
    :: (common_binaryen_options ()
       @ List.flatten
           (List.map
              ~f:(fun runtime_file -> [ Filename.quote runtime_file; "env" ])
              runtime_files)
       @ [ Filename.quote input_file; "exec"; "-o"; Filename.quote output_file ]
       @
       if enable_source_maps
       then [ "--output-source-map"; Filename.quote (output_file ^ ".map") ]
       else []))

let generate_dependencies primitives =
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
         (Yojson.Basic.Util.to_list (Yojson.Basic.from_string Wa_runtime.dependencies))))

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

let dead_code_elimination ~enable_source_maps in_file out_file =
  with_intermediate_file (Filename.temp_file "deps" ".json")
  @@ fun deps_file ->
  with_intermediate_file (Filename.temp_file "usage" ".txt")
  @@ fun usage_file ->
  let primitives = Linker.get_provided () in
  write_file deps_file (generate_dependencies primitives);
  command
    ("wasm-metadce"
    :: (common_binaryen_options ()
       @ [ "--graph-file"; Filename.quote deps_file; Filename.quote in_file ]
       @ (if enable_source_maps
          then [ "--input-source-map"; Filename.quote (in_file ^ ".map") ]
          else [])
       @ [ "-o"; Filename.quote out_file ]
       @ (if enable_source_maps
          then [ "--output-source-map"; Filename.quote (out_file ^ ".map") ]
          else [])
       @ [ ">"; Filename.quote usage_file ]));
  filter_unused_primitives primitives usage_file

let optimization_options =
  [| [ "-O2"; "--skip-pass=inlining-optimizing" ]
   ; [ "-O2"; "--skip-pass=inlining-optimizing"; "--traps-never-happen" ]
   ; [ "-O3"; "--traps-never-happen" ]
  |]

let optimize ~profile ?sourcemap_file in_file out_file =
  let level =
    match profile with
    | None -> 1
    | Some p -> fst (List.find ~f:(fun (_, p') -> Poly.equal p p') Driver.profiles)
  in
  command
    ("wasm-opt"
     :: (common_binaryen_options ()
        @ optimization_options.(level - 1)
        @ [ Filename.quote in_file; "-o"; Filename.quote out_file ])
    @
    match sourcemap_file with
    | Some sourcemap_file ->
        [ "--input-source-map"
        ; Filename.quote (in_file ^ ".map")
        ; "--output-source-map"
        ; Filename.quote sourcemap_file
        ; "--output-source-map-url"
        ; Filename.quote sourcemap_file
        ]
    | None -> [])

let link_and_optimize ~profile ?sourcemap_file runtime_wasm_files wat_file output_file =
  let sourcemap_file =
    (* Check that Binaryen supports the necessary sourcemaps options (requires
       version >= 118) *)
    match sourcemap_file with
    | Some _ when Sys.command "wasm-merge -osm foo 2> /dev/null" <> 0 -> None
    | Some _ | None -> sourcemap_file
  in
  let enable_source_maps = Option.is_some sourcemap_file in
  with_intermediate_file (Filename.temp_file "runtime" ".wasm")
  @@ fun runtime_file ->
  write_file runtime_file Wa_runtime.wasm_runtime;
  with_intermediate_file (Filename.temp_file "wasm-merged" ".wasm")
  @@ fun temp_file ->
  link ~enable_source_maps (runtime_file :: runtime_wasm_files) wat_file temp_file;
  with_intermediate_file (Filename.temp_file "wasm-dce" ".wasm")
  @@ fun temp_file' ->
  let primitives = dead_code_elimination ~enable_source_maps temp_file temp_file' in
  optimize ~profile ?sourcemap_file temp_file' output_file;
  (* Add source file contents to source map *)
  Option.iter sourcemap_file ~f:(fun sourcemap_file ->
      let open Source_map in
      let source_map, mappings = Source_map_io.of_file_no_mappings sourcemap_file in
      assert (List.is_empty (Option.value source_map.sources_content ~default:[]));
      let sources_content =
        Some
          (List.map source_map.sources ~f:(fun file ->
               if Sys.file_exists file && not (Sys.is_directory file)
               then Some (Fs.read_file file)
               else None))
      in
      let source_map = { source_map with sources_content } in
      Source_map_io.to_file ?mappings source_map ~file:sourcemap_file);
  primitives

let escape_string s =
  let l = String.length s in
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    (* https://github.com/ocsigen/js_of_ocaml/issues/898 *)
    | '/' when i > 0 && Char.equal s.[i - 1] '<' -> Buffer.add_string b "\\/"
    | '\000' .. '\031' | '\127' ->
        Buffer.add_string b "\\x";
        Buffer.add_char_hex b c
    | '"' ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let build_js_runtime primitives (strings, fragments) wasm_file output_file =
  let always_required_js, primitives =
    let l =
      StringSet.fold
        (fun nm l ->
          let id = Utf8_string.of_string_exn nm in
          Javascript.Property (PNI id, EVar (S { name = id; var = None; loc = N })) :: l)
        primitives
        []
    in
    match
      List.split_last
      @@ Driver.link_and_pack [ Javascript.Return_statement (Some (EObj l)), N ]
    with
    | Some x -> x
    | None -> assert false
  in
  let b = Buffer.create 1024 in
  let f = Pretty_print.to_buffer b in
  Pretty_print.set_compact f (not (Config.Flag.pretty ()));
  ignore (Js_output.program f always_required_js);
  let b' = Buffer.create 1024 in
  let f = Pretty_print.to_buffer b' in
  Pretty_print.set_compact f (not (Config.Flag.pretty ()));
  ignore (Js_output.program f [ primitives ]);
  let b'' = Buffer.create 1024 in
  let f = Pretty_print.to_buffer b'' in
  Pretty_print.set_compact f (not (Config.Flag.pretty ()));
  ignore
    (Js_output.program
       f
       [ ( Javascript.Expression_statement
             (EArr
                (List.map
                   ~f:(fun s -> Javascript.Element (EStr (Utf8_string.of_string_exn s)))
                   strings))
         , Javascript.N )
       ]);
  let fragment_buffer = Buffer.create 1024 in
  let f = Pretty_print.to_buffer fragment_buffer in
  Pretty_print.set_compact f (not (Config.Flag.pretty ()));
  ignore
    (Js_output.program
       f
       [ ( Javascript.Expression_statement
             (EObj
                (List.map
                   ~f:(fun (nm, f) ->
                     let id = Utf8_string.of_string_exn nm in
                     Javascript.Property (PNI id, f))
                   fragments))
         , Javascript.N )
       ]);
  let s = Wa_runtime.js_runtime in
  let rec find pat i =
    if String.equal (String.sub s ~pos:i ~len:(String.length pat)) pat
    then i
    else find pat (i + 1)
  in
  let i = find "CODE" 0 in
  let j = find "PRIMITIVES" 0 in
  let k = find "STRINGS" 0 in
  let l = find "FRAGMENTS" 0 in
  let rec trim_semi s =
    let l = String.length s in
    if l = 0
    then s
    else
      match s.[l - 1] with
      | ';' | '\n' -> trim_semi (String.sub s ~pos:0 ~len:(l - 1))
      | _ -> s
  in
  gen_file output_file
  @@ fun tmp_output_file ->
  write_file
    tmp_output_file
    (Buffer.contents b
    ^ String.sub s ~pos:0 ~len:i
    ^ escape_string (Filename.basename wasm_file)
    ^ String.sub s ~pos:(i + 4) ~len:(j - i - 4)
    ^ trim_semi (Buffer.contents b')
    ^ String.sub s ~pos:(j + 10) ~len:(k - j - 10)
    ^ trim_semi (Buffer.contents b'')
    ^ String.sub s ~pos:(k + 7) ~len:(l - k - 7)
    ^ trim_semi (Buffer.contents fragment_buffer)
    ^ String.sub s ~pos:(l + 9) ~len:(String.length s - l - 9))

let run
    { Cmd_arg.common
    ; profile
    ; runtime_files
    ; input_file
    ; output_file
    ; enable_source_maps
    ; params
    } =
  Jsoo_cmdline.Arg.eval common;
  Wa_generate.init ();
  let output_file = fst output_file in
  if debug_mem () then Debug.start_profiling output_file;
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  let t = Timer.make () in
  let include_dirs = List.filter_map [ "+stdlib/" ] ~f:(fun d -> Findlib.find [] d) in
  let runtime_wasm_files, runtime_js_files =
    List.partition runtime_files ~f:(fun name ->
        List.exists
          ~f:(fun s -> Filename.check_suffix name s)
          [ ".wasm"; ".wat"; ".wast" ])
  in
  let runtime_js_files, builtin =
    List.partition_map runtime_js_files ~f:(fun name ->
        match Builtins.find name with
        | Some t -> `Snd t
        | None -> `Fst name)
  in
  let t1 = Timer.make () in
  let builtin = Js_of_ocaml_compiler_runtime_files.runtime @ builtin in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments
        ~ignore_always_annotation:true
        ~target_env:Target_env.Isomorphic
        ~filename
        runtimes);
  Linker.load_files
    ~ignore_always_annotation:true
    ~target_env:Target_env.Isomorphic
    runtime_js_files;
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = Config.Flag.debuginfo () in
  let output (one : Parse_bytecode.one) ~standalone ch =
    let code = one.code in
    let live_vars, in_cps, p, debug =
      Driver.f
        ~target:Wasm
        ~standalone
        ?profile
        ~linkall:false
        ~wrap_with_fun:`Iife
        one.debug
        code
    in
    let strings = Wa_generate.f ch ~debug ~live_vars ~in_cps p in
    if times () then Format.eprintf "compilation: %a@." Timer.print t;
    strings
  in
  (let kind, ic, close_ic, include_dirs =
     let ch = open_in_bin input_file in
     let res = Parse_bytecode.from_channel ch in
     let include_dirs = Filename.dirname input_file :: include_dirs in
     res, ch, (fun () -> close_in ch), include_dirs
   in
   (match kind with
   | `Exe ->
       let t1 = Timer.make () in
       (* The OCaml compiler can generate code using the
          "caml_string_greaterthan" primitive but does not use it
          itself. This is (was at some point at least) the only primitive
          in this case.  Ideally, Js_of_ocaml should parse the .mli files
          for primitives as well as marking this primitive as potentially
          used. But the -linkall option is probably good enough. *)
       let code =
         Parse_bytecode.from_exe
           ~target:`Wasm
           ~includes:include_dirs
           ~include_cmis:false
           ~link_info:false
           ~linkall:false
           ~debug:need_debug
           ic
       in
       if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
       gen_file (Filename.chop_extension output_file ^ ".wat")
       @@ fun wat_file ->
       let wasm_file =
         if Filename.check_suffix output_file ".wasm.js"
         then Filename.chop_extension output_file
         else Filename.chop_extension output_file ^ ".wasm"
       in
       gen_file wasm_file
       @@ fun tmp_wasm_file ->
       let strings = output_gen wat_file (output code ~standalone:true) in
       let primitives =
         link_and_optimize
           ~profile
           ?sourcemap_file:
             (if enable_source_maps then Some (wasm_file ^ ".map") else None)
           runtime_wasm_files
           wat_file
           tmp_wasm_file
       in
       build_js_runtime primitives strings wasm_file output_file
   | `Cmo _ | `Cma _ -> assert false);
   close_ic ());
  Debug.stop_profiling ()

let info name =
  Info.make
    ~name
    ~doc:"Wasm_of_ocaml compiler"
    ~description:"Wasm_of_ocaml is a compiler from OCaml bytecode to WebAssembly."

let term = Cmdliner.Term.(const run $ Cmd_arg.options)

let command =
  let t = Cmdliner.Term.(const run $ Cmd_arg.options) in
  Cmdliner.Cmd.v (info "compile") t

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

let link_and_optimize ~profile runtime_wasm_files wat_files output_file =
  let debuginfo = Config.Flag.pretty () in
  Wa_binaryen.with_intermediate_file (Filename.temp_file "runtime" ".wasm")
  @@ fun runtime_file ->
  Wa_binaryen.write_file ~name:runtime_file ~contents:Wa_runtime.wasm_runtime;
  Wa_binaryen.with_intermediate_file (Filename.temp_file "wasm-merged" ".wasm")
  @@ fun temp_file ->
  Wa_binaryen.link
    ~debuginfo
    ~runtime_files:(runtime_file :: runtime_wasm_files)
    ~input_files:wat_files
    ~output_file:temp_file;
  Wa_binaryen.with_intermediate_file (Filename.temp_file "wasm-dce" ".wasm")
  @@ fun temp_file' ->
  let primitives =
    Wa_binaryen.dead_code_elimination
      ~debuginfo
      ~dependencies:Wa_runtime.dependencies
      ~input_file:temp_file
      ~output_file:temp_file'
  in
  Wa_binaryen.optimize ~debuginfo ~profile ~input_file:temp_file' ~output_file;
  primitives

let link_runtime ~profile runtime_wasm_files output_file =
  let debuginfo = Config.Flag.pretty () in
  Wa_binaryen.with_intermediate_file (Filename.temp_file "runtime" ".wasm")
  @@ fun runtime_file ->
  Wa_binaryen.write_file ~name:runtime_file ~contents:Wa_runtime.wasm_runtime;
  Wa_binaryen.with_intermediate_file (Filename.temp_file "wasm-merged" ".wasm")
  @@ fun temp_file ->
  Wa_binaryen.link
    ~debuginfo
    ~runtime_files:(runtime_file :: runtime_wasm_files)
    ~input_files:[]
    ~output_file:temp_file;
  Wa_binaryen.optimize ~debuginfo ~profile ~input_file:temp_file ~output_file

let generate_prelude ~out_file =
  Filename.gen_file out_file
  @@ fun ch ->
  let code, uinfo = Parse_bytecode.predefined_exceptions ~target:`Wasm in
  let context = Wa_generate.start () in
  let live_vars, in_cps, p =
    Driver.f ~target:Wasm (Parse_bytecode.Debug.create ~include_cmis:false false) code
  in
  let _ = Wa_generate.f ~context ~unit_name:(Some "globals") ~live_vars ~in_cps p in
  Wa_generate.output ch ~context;
  uinfo.provides

let build_prelude z =
  Wa_binaryen.with_intermediate_file (Filename.temp_file "prelude" ".wasm")
  @@ fun prelude_file ->
  Wa_binaryen.with_intermediate_file (Filename.temp_file "prelude_file" ".wasm")
  @@ fun tmp_prelude_file ->
  let predefined_exceptions = generate_prelude ~out_file:prelude_file in
  Wa_binaryen.optimize
    ~debuginfo:false
    ~profile:(Driver.profile 1)
    ~input_file:prelude_file
    ~output_file:tmp_prelude_file;
  Zip.add_file z ~name:"prelude.wasm" ~file:tmp_prelude_file;
  predefined_exceptions

let run
    { Cmd_arg.common
    ; profile
    ; runtime_files
    ; input_file
    ; output_file
    ; params
    ; runtime_only
    } =
  Jsoo_cmdline.Arg.eval common;
  Wa_generate.init ();
  if debug_mem () then Debug.start_profiling (fst output_file);
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
  let compile ~context ~unit_name (one : Parse_bytecode.one) =
    let standalone = Option.is_none unit_name in
    let code = one.code in
    let live_vars, in_cps, p = Driver.f ~target:Wasm ?profile one.debug code in
    let toplevel_name, js_code = Wa_generate.f ~context ~unit_name ~live_vars ~in_cps p in
    if standalone then Wa_generate.add_start_function ~context toplevel_name;
    js_code
  in
  let output (one : Parse_bytecode.one) ~unit_name ch =
    let context = Wa_generate.start () in
    let js_code = compile ~context ~unit_name one in
    Wa_generate.output ch ~context;
    if times () then Format.eprintf "compilation: %a@." Timer.print t;
    js_code
  in
  (if runtime_only
   then (
     Wa_binaryen.gen_file (fst output_file)
     @@ fun tmp_output_file ->
     Wa_binaryen.with_intermediate_file (Filename.temp_file "wasm" ".wasm")
     @@ fun tmp_wasm_file ->
     link_runtime ~profile runtime_wasm_files tmp_wasm_file;
     let primitives =
       tmp_wasm_file
       |> (fun file -> Wa_link.Wasm_binary.read_imports ~file)
       |> List.filter_map ~f:(fun { Wa_link.Wasm_binary.module_; name; _ } ->
              if String.equal module_ "js" then Some name else None)
       |> StringSet.of_list
     in
     let js_runtime =
       Wa_link.build_js_runtime ~js_launcher:Wa_runtime.js_runtime ~primitives ()
     in
     let z = Zip.open_out tmp_output_file in
     Zip.add_file z ~name:"runtime.wasm" ~file:tmp_wasm_file;
     Zip.add_entry z ~name:"runtime.js" ~contents:js_runtime;
     let predefined_exceptions = build_prelude z in
     Wa_link.add_info
       z
       ~predefined_exceptions
       ~build_info:(Build_info.create `Runtime)
       ~unit_data:[]
       ();
     Zip.close_out z)
   else
     let kind, ic, close_ic, include_dirs =
       let input_file =
         match input_file with
         | None -> assert false
         | Some f -> f
       in
       let ch = open_in_bin input_file in
       let res = Parse_bytecode.from_channel ch in
       let include_dirs = Filename.dirname input_file :: include_dirs in
       res, ch, (fun () -> close_in ch), include_dirs
     in
     let compile_cmo z cmo =
       let t1 = Timer.make () in
       let code =
         Parse_bytecode.from_cmo
           ~target:`Wasm
           ~includes:include_dirs
           ~debug:need_debug
           cmo
           ic
       in
       let unit_info = Unit_info.of_cmo cmo in
       let unit_name = StringSet.choose unit_info.provides in
       if times () then Format.eprintf "  parsing: %a (%s)@." Timer.print t1 unit_name;
       Wa_binaryen.with_intermediate_file (Filename.temp_file unit_name ".wat")
       @@ fun wat_file ->
       Wa_binaryen.with_intermediate_file (Filename.temp_file unit_name ".wasm")
       @@ fun tmp_wasm_file ->
       let strings, fragments =
         Filename.gen_file wat_file (output code ~unit_name:(Some unit_name))
       in
       Wa_binaryen.optimize
         ~debuginfo:(Config.Flag.pretty ())
         ~profile
         ~input_file:wat_file
         ~output_file:tmp_wasm_file;
       Zip.add_file z ~name:(unit_name ^ ".wasm") ~file:tmp_wasm_file;
       { Wa_link.unit_info; strings; fragments }
     in
     (match kind with
     | `Exe ->
         let t1 = Timer.make () in
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
         let output_file = fst output_file in
         Wa_binaryen.gen_file (Filename.chop_extension output_file ^ ".wat")
         @@ fun wat_file ->
         let wasm_file = Wa_link.associated_wasm_file ~js_output_file:output_file in
         Wa_binaryen.gen_file wasm_file
         @@ fun tmp_wasm_file ->
         let generated_js =
           [ None, Filename.gen_file wat_file (output code ~unit_name:None) ]
         in
         let primitives =
           link_and_optimize ~profile runtime_wasm_files [ wat_file ] tmp_wasm_file
         in
         let js_runtime =
           let missing_primitives =
             let l = Wa_link.Wasm_binary.read_imports ~file:tmp_wasm_file in
             List.filter_map
               ~f:(fun { Wa_link.Wasm_binary.module_; name; _ } ->
                 if String.equal module_ "env" then Some name else None)
               l
           in
           Wa_link.build_js_runtime
             ~js_launcher:Wa_runtime.js_runtime
             ~primitives
             ~runtime_arguments:
               (Wa_link.build_runtime_arguments
                  ~missing_primitives
                  ~wasm_file
                  ~generated_js
                  ())
             ()
         in
         Wa_binaryen.gen_file output_file
         @@ fun tmp_output_file ->
         Wa_binaryen.write_file ~name:tmp_output_file ~contents:js_runtime
     | `Cmo cmo ->
         let output_file = Filename.chop_extension (fst output_file) ^ ".wasm" in
         Wa_binaryen.gen_file output_file
         @@ fun tmp_output_file ->
         let z = Zip.open_out tmp_output_file in
         let unit_data = [ compile_cmo z cmo ] in
         Wa_link.add_info z ~build_info:(Build_info.create `Cmo) ~unit_data ();
         Zip.close_out z
     | `Cma cma ->
         let output_file = Filename.chop_extension (fst output_file) ^ ".wasm" in
         Wa_binaryen.gen_file output_file
         @@ fun tmp_output_file ->
         let z = Zip.open_out tmp_output_file in
         let unit_data = List.map ~f:(fun cmo -> compile_cmo z cmo) cma.lib_units in
         Wa_link.add_info z ~build_info:(Build_info.create `Cma) ~unit_data ();
         Zip.close_out z);

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

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

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let () = Sys.catch_break true

let output_gen output_file f =
  Code.Var.set_pretty true;
  Code.Var.set_stable (Config.Flag.stable_var ());
  Filename.gen_file output_file f

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename with Sys_error _msg -> ()

let check_result res =
  match res with
  | Unix.WEXITED 0 -> ()
  | _ -> assert false
(*ZZZ*)

let link wat_file output_file =
  let common_options =
    [| "--enable-gc"
     ; "--enable-multivalue"
     ; "--enable-exception-handling"
     ; "--enable-reference-types"
     ; "--enable-tail-call"
     ; "--enable-bulk-memory"
     ; "--enable-nontrapping-float-to-int"
     ; "--enable-strings"
     ; "-g"
    |]
  in
  let temp_file = Filename.temp_file "wasm-merged" ".wasm" in
  let ch =
    Unix.open_process_args_out
      "wasm-merge"
      (Array.concat
         [ [| "wasm-merge" |]
         ; common_options
         ; [| "-"; "env"; wat_file; "exec"; "-o"; temp_file |]
         ])
  in
  output_string ch Wa_runtime.runtime;
  check_result (Unix.close_process_out ch);
  let wasm_file = fst output_file in
  let ch =
    Unix.open_process_args_out
      "wasm-opt"
      (Array.concat
         [ [| "wasm-opt" |]
         ; common_options
         ; [| "-O3"; "--gufa"; "-O3"; temp_file; "-o"; wasm_file |]
         ])
  in
  check_result (Unix.close_process_out ch);
  remove_file temp_file

let run { Cmd_arg.common; profile; input_file; output_file; params } =
  Wa_generate.init ();
  Jsoo_cmdline.Arg.eval common;
  (match output_file with
  | name, _ when debug_mem () -> Debug.start_profiling name
  | _, _ -> ());
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  let t = Timer.make () in
  let include_dirs = List.filter_map [ "+stdlib/" ] ~f:(fun d -> Findlib.find [] d) in
  let t1 = Timer.make () in
  let builtin = Js_of_ocaml_compiler_runtime_files.runtime in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments ~target_env:Target_env.Isomorphic ~filename runtimes);
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = Config.Flag.debuginfo () in
  let output (one : Parse_bytecode.one) ~standalone ch =
    let code = one.code in
    let _ =
      Driver.f
        ~target:(`Wasm ch)
        ~standalone
        ?profile
        ~linkall:false
        ~wrap_with_fun:`Iife
        one.debug
        code
    in
    if times () then Format.eprintf "compilation: %a@." Timer.print t
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
       let wat_file = Filename.chop_extension (fst output_file) ^ ".wat" in
       output_gen wat_file (output code ~standalone:true);
       link wat_file output_file
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

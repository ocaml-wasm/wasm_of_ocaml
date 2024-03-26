(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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
open! Stdlib

let times = Debug.find "times"

module Wasm_binary = struct
  let header = "\000asm\001\000\000\000"

  let check_header file ch =
    let s = really_input_string ch 8 in
    if not (String.equal s header)
    then failwith (file ^ " is not a Wasm binary file (bad magic)")

  type t =
    { ch : in_channel
    ; limit : int
    }

  let open_in f =
    let ch = open_in_bin f in
    check_header f ch;
    { ch; limit = in_channel_length ch }

  let from_channel ~name ch pos len =
    seek_in ch pos;
    check_header name ch;
    { ch; limit = pos + len }

  let rec read_uint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 16);
    if i < 128 then i else i - 128 + (read_uint ~n:(n - 1) ch lsl 7)

  let rec read_sint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 8 || (i > 120 && i < 128));
    if i < 64
    then i
    else if i < 128
    then i - 128
    else i - 128 + (read_sint ~n:(n - 1) ch lsl 7)

  type section =
    { id : int
    ; size : int
    }

  let next_section ch =
    if pos_in ch.ch = ch.limit
    then None
    else
      let id = input_byte ch.ch in
      let size = read_uint ch.ch in
      Some { id; size }

  let skip_section ch { size; _ } = seek_in ch.ch (pos_in ch.ch + size)

  let vec f ch =
    let rec loop acc n = if n = 0 then List.rev acc else loop (f ch :: acc) (n - 1) in
    loop [] (read_uint ch)

  let name ch =
    let n = read_uint ch in
    really_input_string ch n

  let heaptype ch = ignore (read_sint ch)

  let reftype' i ch =
    match i with
    | 0x6a | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 -> ()
    | 0x63 | 0x64 -> heaptype ch
    | _ ->
        Format.eprintf "Unknown reftype %x@." i;
        assert false

  let reftype ch = reftype' (input_byte ch) ch

  let valtype ch =
    let i = read_uint ch in
    match i with
    | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f -> ()
    | _ -> reftype' i ch

  let limits ch =
    match input_byte ch with
    | 0 -> ignore (read_uint ch)
    | 1 ->
        ignore (read_uint ch);
        ignore (read_uint ch)
    | _ -> assert false

  let memtype = limits

  let tabletype ch =
    reftype ch;
    limits ch

  type import =
    { module_ : string
    ; name : string
    }

  let import ch =
    let module_ = name ch in
    let name = name ch in
    let d = read_uint ch in
    let _ =
      match d with
      | 0 -> ignore (read_uint ch)
      | 1 -> tabletype ch
      | 2 -> memtype ch
      | 3 ->
          let _typ = valtype ch in
          let _mut = input_byte ch in
          ()
      | 4 ->
          assert (read_uint ch = 0);
          ignore (read_uint ch)
      | _ ->
          Format.eprintf "Unknown import %x@." d;
          assert false
    in
    { module_; name }

  let export ch =
    let name = name ch in
    let d = read_uint ch in
    if d > 4
    then (
      Format.eprintf "Unknown export %x@." d;
      assert false);
    ignore (read_uint ch);
    name

  let read_imports ~file =
    let ch = open_in file in
    let rec find_section () =
      match next_section ch with
      | None -> false
      | Some s ->
          s.id = 2
          ||
          (skip_section ch s;
           find_section ())
    in
    let res = if find_section () then vec import ch.ch else [] in
    close_in ch.ch;
    res

  type interface =
    { imports : import list
    ; exports : string list
    }

  let read_interface ch =
    let rec find_sections i =
      match next_section ch with
      | None -> i
      | Some s ->
          if s.id = 2
          then find_sections { i with imports = vec import ch.ch }
          else if s.id = 7
          then { i with exports = vec export ch.ch }
          else (
            skip_section ch s;
            find_sections i)
    in
    find_sections { imports = []; exports = [] }
end

type unit_data =
  { unit_info : Unit_info.t
  ; strings : string list
  ; fragments : (string * Javascript.expression) list
  }

let info_to_json ~build_info ~unit_data =
  let js_to_string e =
    let b = Buffer.create 128 in
    let f = Pretty_print.to_buffer b in
    Pretty_print.set_compact f true;
    ignore (Js_output.program f [ Javascript.Expression_statement e, Javascript.N ]);
    let rec trim_semi s =
      let l = String.length s in
      if l = 0
      then s
      else
        match s.[l - 1] with
        | ';' | '\n' -> trim_semi (String.sub s ~pos:0 ~len:(l - 1))
        | _ -> s
    in
    trim_semi (Buffer.contents b)
  in
  let add nm skip v rem = if skip then rem else (nm, v) :: rem in
  `Assoc
    ([]
    |> add
         "units"
         (List.is_empty unit_data)
         (`List
           (List.map
              ~f:(fun { unit_info; strings; fragments } ->
                `Assoc
                  ([]
                  |> add
                       "strings"
                       (List.is_empty strings)
                       (`List (List.map ~f:(fun s -> `String s) strings))
                  |> add
                       "fragments"
                       (List.is_empty fragments)
                       (`Assoc
                         (List.map
                            ~f:(fun (nm, e) -> nm, `String (js_to_string e))
                            fragments))
                  |> add "unit_info" false (Unit_info.to_json unit_info)))
              unit_data))
    |> add "build_info" false (Build_info.to_json build_info))

let info_from_json info =
  let open Yojson.Basic.Util in
  let build_info = info |> member "build_info" |> Build_info.from_json in
  let unit_data =
    info
    |> member "units"
    |> to_option to_list
    |> Option.value ~default:[]
    |> List.map ~f:(fun u ->
           let unit_info = u |> member "unit_info" |> Unit_info.from_json in
           let strings =
             u
             |> member "strings"
             |> to_option to_list
             |> Option.value ~default:[]
             |> List.map ~f:to_string
           in
           let fragments =
             u
             |> member "fragments"
             |> to_option to_assoc
             |> Option.value ~default:[]
             |> List.map ~f:(fun (nm, e) ->
                    ( nm
                    , let lex = Parse_js.Lexer.of_string (to_string e) in
                      Parse_js.parse_expr lex ))
           in
           { unit_info; strings; fragments })
  in
  build_info, unit_data

let add_info z ~build_info ~unit_data =
  Zip.add_entry
    z
    ~name:"info.json"
    ~contents:(Yojson.Basic.to_string (info_to_json ~build_info ~unit_data))

let read_info z =
  info_from_json (Yojson.Basic.from_string (Zip.read_entry z ~name:"info.json"))

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

let generate_start_function ~to_link ~out_file =
  Filename.gen_file out_file
  @@ fun ch ->
  let context = Wa_generate.start () in
  Wa_generate.add_init_function ~context ~to_link:("globals" :: to_link);
  Wa_generate.output ch ~context

let associated_wasm_file ~js_output_file =
  if Filename.check_suffix js_output_file ".wasm.js"
  then Filename.chop_extension js_output_file
  else Filename.chop_extension js_output_file ^ ".wasm"

let report_missing_primitives missing =
  if not (List.is_empty missing)
  then (
    warn "There are some missing Wasm primitives@.";
    warn "Dummy implementations (raising an exception) ";
    warn "will be provided.@.";
    warn "Missing primitives:@.";
    List.iter ~f:(fun nm -> warn "  %s@." nm) missing)

let build_js_runtime
    ~js_launcher
    ~prelude
    ~primitives
    ~generated_js
    ~tmp_wasm_file
    ?(separate_compilation = false)
    ?missing_primitives
    wasm_file
    output_file =
  let missing_primitives =
    match missing_primitives with
    | Some l -> l
    | None ->
        let l = Wasm_binary.read_imports ~file:tmp_wasm_file in
        List.filter_map
          ~f:(fun { Wasm_binary.module_; name; _ } ->
            if String.equal module_ "env" then Some name else None)
          l
  in
  let missing_primitives = if Config.Flag.genprim () then missing_primitives else [] in
  if not separate_compilation then report_missing_primitives missing_primitives;
  let obj l =
    Javascript.EObj
      (List.map
         ~f:(fun (nm, v) ->
           let id = Utf8_string.of_string_exn nm in
           Javascript.Property (PNS id, v))
         l)
  in
  let generated_js =
    List.concat
    @@ List.map
         ~f:(fun (unit_name, (strings, fragments)) ->
           let name s =
             match unit_name with
             | None -> s
             | Some nm -> nm ^ "." ^ s
           in
           let strings =
             if List.is_empty strings
             then []
             else
               [ ( name "strings"
                 , Javascript.EArr
                     (List.map
                        ~f:(fun s ->
                          Javascript.Element (EStr (Utf8_string.of_string_exn s)))
                        strings) )
               ]
           in
           let fragments =
             if List.is_empty fragments then [] else [ name "fragments", obj fragments ]
           in
           strings @ fragments)
         generated_js
  in
  let generated_js =
    if not (List.is_empty missing_primitives)
    then
      ( "env"
      , obj
          (List.map
             ~f:(fun nm ->
               ( nm
               , Javascript.EArrow
                   ( Javascript.fun_
                       []
                       [ ( Throw_statement
                             (ENew
                                ( EVar
                                    (Javascript.ident (Utf8_string.of_string_exn "Error"))
                                , Some
                                    [ Arg
                                        (EStr
                                           (Utf8_string.of_string_exn
                                              (nm ^ " not implemented")))
                                    ] ))
                         , N )
                       ]
                       N
                   , AUnknown ) ))
             missing_primitives) )
      :: generated_js
    else generated_js
  in
  let init_fun =
    match Parse_js.parse (Parse_js.Lexer.of_string js_launcher) with
    | [ (Expression_statement f, _) ] -> f
    | _ -> assert false
  in
  let generated_js =
    if List.is_empty generated_js
    then obj generated_js
    else
      let var ident e =
        Javascript.variable_declaration [ Javascript.ident ident, (e, N) ], Javascript.N
      in
      Javascript.call
        (EArrow
           ( Javascript.fun_
               [ Javascript.ident Constant.global_object_ ]
               [ var
                   Constant.old_global_object_
                   (EVar (Javascript.ident Constant.global_object_))
               ; var
                   Constant.exports_
                   (EBin
                      ( Or
                      , EDot
                          ( EDot
                              ( EVar (Javascript.ident Constant.global_object_)
                              , ANullish
                              , Utf8_string.of_string_exn "module" )
                          , ANullish
                          , Utf8_string.of_string_exn "export" )
                      , EVar (Javascript.ident Constant.global_object_) ))
               ; Return_statement (Some (obj generated_js)), N
               ]
               N
           , AUnknown ))
        [ EVar (Javascript.ident Constant.global_object_) ]
        N
  in
  let launcher =
    Code.Var.reset ();
    Code.Var.set_pretty true;
    Code.Var.set_stable (Config.Flag.stable_var ());
    let b = Buffer.create 1024 in
    let f = Pretty_print.to_buffer b in
    Pretty_print.set_compact f (not (Config.Flag.pretty ()));
    let js =
      [ ( Javascript.Expression_statement
            (Javascript.call
               init_fun
               [ ENum (Javascript.Num.of_int32 (if separate_compilation then 1l else 0l))
               ; EStr (Utf8_string.of_string_exn (Filename.basename wasm_file))
               ; primitives
               ; generated_js
               ]
               N)
        , Javascript.N )
      ]
    in
    List.iter ~f:Var_printer.add_reserved [ "joo_global_object"; "jsoo_exports" ];
    let traverse = new Js_traverse.free in
    let js = traverse#program js in
    let free = traverse#get_free in
    Javascript.IdentSet.iter
      (fun x ->
        match x with
        | V _ -> assert false
        | S { name = Utf8 x; _ } -> Var_printer.add_reserved x)
      free;
    let js =
      if Config.Flag.shortvar ()
      then (
        let t5 = Timer.make () in
        let js = (new Js_traverse.rename_variable)#program js in
        if times () then Format.eprintf "    shorten vars: %a@." Timer.print t5;
        js)
      else js
    in
    let js = (new Js_traverse.simpl)#program js in
    let js = (new Js_traverse.clean)#program js in
    let js = Js_assign.program js in
    ignore (Js_output.program f js);
    Buffer.contents b
  in
  Wa_binaryen.gen_file output_file
  @@ fun tmp_output_file ->
  Wa_binaryen.write_file ~name:tmp_output_file ~contents:(prelude ^ launcher)

let link_to_archive ~set_to_link ~prelude_file ~files ~start_file ~tmp_wasm_file =
  Wa_binaryen.with_intermediate_file (Filename.temp_file "prelude_file" ".wasm")
  @@ fun tmp_prelude_file ->
  Wa_binaryen.optimize
    ~debuginfo:false
    ~profile:(Driver.profile 1)
    ~input_file:prelude_file
    ~output_file:tmp_prelude_file;
  Wa_binaryen.with_intermediate_file (Filename.temp_file "start_file" ".wasm")
  @@ fun tmp_start_file ->
  Wa_binaryen.optimize
    ~debuginfo:false
    ~profile:(Driver.profile 1)
    ~input_file:start_file
    ~output_file:tmp_start_file;
  let ch = Zip.open_out tmp_wasm_file in
  let read_interface z ~name =
    Wasm_binary.read_interface
      (let ch, pos, len = Zip.get_entry z ~name in
       Wasm_binary.from_channel ~name ch pos len)
  in
  let z = Zip.open_in (fst (List.hd files)) in
  let runtime_intf = read_interface z ~name:"runtime.wasm" in
  Zip.copy_file z ch ~src_name:"runtime.wasm" ~dst_name:"runtime.wasm";
  Zip.close_in z;
  Zip.add_file ch ~name:"prelude.wasm" ~file:tmp_prelude_file;
  let intfs = ref [] in
  List.iter
    ~f:(fun (file, (_, units)) ->
      let z = Zip.open_in file in
      List.iter
        ~f:(fun { unit_info; _ } ->
          let unit_name = StringSet.choose unit_info.provides in
          if StringSet.mem unit_name set_to_link
          then (
            let name = unit_name ^ ".wasm" in
            intfs := read_interface z ~name :: !intfs;
            Zip.copy_file z ch ~src_name:name ~dst_name:name))
        units;
      Zip.close_in z)
    files;
  Zip.add_file ch ~name:"start.wasm" ~file:tmp_start_file;
  Zip.close_out ch;
  runtime_intf, List.rev !intfs

let compute_missing_primitives (runtime_intf, intfs) =
  let provided_primitives = StringSet.of_list runtime_intf.Wasm_binary.exports in
  StringSet.elements
  @@ List.fold_left
       ~f:(fun s { Wasm_binary.imports; _ } ->
         List.fold_left
           ~f:(fun s { Wasm_binary.module_; name; _ } ->
             if String.equal module_ "env" && not (StringSet.mem name provided_primitives)
             then StringSet.add name s
             else s)
           ~init:s
           imports)
       ~init:StringSet.empty
       intfs

let link ~js_launcher ~output_file ~linkall ~files =
  if times () then Format.eprintf "linking@.";
  let t = Timer.make () in
  let files =
    List.map files ~f:(fun file ->
        let z = Zip.open_in file in
        let info = read_info z in
        Zip.close_in z;
        file, info)
  in
  (match files with
  | [] -> ()
  | (file, (bi, _)) :: r ->
      Build_info.configure bi;
      ignore
        (List.fold_left
           ~init:bi
           ~f:(fun bi (file', (bi', _)) -> Build_info.merge file bi file' bi')
           r));
  if times () then Format.eprintf "    reading information: %a@." Timer.print t;
  let t1 = Timer.make () in
  let missing, to_link =
    List.fold_right
      files
      ~init:(StringSet.empty, [])
      ~f:(fun (_file, (build_info, units)) acc ->
        let cmo_file =
          match Build_info.kind build_info with
          | `Cmo -> true
          | `Cma | `Exe | `Runtime | `Unknown -> false
        in
        List.fold_right units ~init:acc ~f:(fun { unit_info; _ } (requires, to_link) ->
            if (not (Config.Flag.auto_link ()))
               || cmo_file
               || linkall
               || unit_info.force_link
               || not (StringSet.is_empty (StringSet.inter requires unit_info.provides))
            then
              ( StringSet.diff
                  (StringSet.union unit_info.requires requires)
                  unit_info.provides
              , StringSet.elements unit_info.provides @ to_link )
            else requires, to_link))
  in
  let set_to_link = StringSet.of_list to_link in
  let files =
    if linkall
    then files
    else
      List.filter
        ~f:(fun (_file, (build_info, units)) ->
          (match Build_info.kind build_info with
          | `Cma | `Exe | `Unknown -> false
          | `Cmo | `Runtime -> true)
          || List.exists
               ~f:(fun { unit_info; _ } ->
                 StringSet.exists
                   (fun nm -> StringSet.mem nm set_to_link)
                   unit_info.provides)
               units)
        files
  in
  Wa_binaryen.with_intermediate_file (Filename.temp_file "prelude" ".wasm")
  @@ fun prelude_file ->
  let predefined_exceptions = generate_prelude ~out_file:prelude_file in
  Wa_binaryen.with_intermediate_file (Filename.temp_file "start" ".wasm")
  @@ fun start_file ->
  generate_start_function ~to_link ~out_file:start_file;
  let missing = StringSet.diff missing predefined_exceptions in
  if not (StringSet.is_empty missing)
  then
    failwith
      (Printf.sprintf
         "Could not find compilation unit for %s"
         (String.concat ~sep:", " (StringSet.elements missing)));
  if times () then Format.eprintf "    finding what to link: %a@." Timer.print t1;
  if times () then Format.eprintf "  scan: %a@." Timer.print t;
  let t = Timer.make () in
  let wasm_file = associated_wasm_file ~js_output_file:output_file in
  Wa_binaryen.gen_file wasm_file
  @@ fun tmp_wasm_file ->
  let interfaces =
    link_to_archive ~set_to_link ~prelude_file ~files ~start_file ~tmp_wasm_file
  in
  let missing_primitives = compute_missing_primitives interfaces in
  if times () then Format.eprintf "    copy wasm files: %a@." Timer.print t;
  let t1 = Timer.make () in
  let prelude, primitives =
    (*ZZZ The first file should exist and be a runtime; other files
      should be cmas or cmos *)
    match files with
    | (file, _) :: _ ->
        let z = Zip.open_in file in
        let prelude = Zip.read_entry z ~name:"prelude.js" in
        let primitives : Javascript.expression =
          Marshal.from_string (Zip.read_entry z ~name:"primitives.js-marshalled") 0
        in
        Zip.close_in z;
        prelude, primitives
    | _ -> assert false
  in
  let generated_js =
    List.concat
    @@ List.map files ~f:(fun (_, (_, units)) ->
           List.map units ~f:(fun { unit_info; strings; fragments } ->
               Some (StringSet.choose unit_info.provides), (strings, fragments)))
  in
  build_js_runtime
    ~js_launcher
    ~prelude
    ~primitives
    ~generated_js
    ~tmp_wasm_file
    ~separate_compilation:true
    ~missing_primitives
    wasm_file
    output_file;
  if times () then Format.eprintf "    build JS runtime: %a@." Timer.print t1;
  if times () then Format.eprintf "  emit: %a@." Timer.print t

let link ~js_launcher ~output_file ~linkall ~files =
  try link ~js_launcher ~output_file ~linkall ~files
  with Build_info.Incompatible_build_info { key; first = f1, v1; second = f2, v2 } ->
    let string_of_v = function
      | None -> "<empty>"
      | Some v -> v
    in
    failwith
      (Printf.sprintf
         "Incompatible build info detected while linking.\n - %s: %s=%s\n - %s: %s=%s"
         f1
         key
         (string_of_v v1)
         f2
         key
         (string_of_v v2))

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
    then failwith (file ^ "is not a Wasm binary file (bad magic)")

  let open_in f =
    let ch = open_in f in
    check_header f ch;
    ch

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
    let id = input_byte ch in
    let size = read_uint ch in
    { id; size }

  let skip_section ch { size; _ } = seek_in ch (pos_in ch + size)

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

  let read_imports ~file =
    let ch = open_in file in
    let rec find_section () =
      let s = next_section ch in
      if s.id <> 2
      then (
        skip_section ch s;
        find_section ())
    in
    let res =
      match find_section () with
      | exception End_of_file -> []
      | _ -> vec import ch
    in
    close_in ch;
    res
end

module Custom_section = struct
  let with_open_out s ~flags f =
    let oc = open_out_gen (Open_wronly :: Open_creat :: Open_binary :: flags) 0o666 s in
    Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

  let with_open_in s f =
    let oc = open_in_bin s in
    Fun.protect ~finally:(fun () -> close_in_noerr oc) (fun () -> f oc)

  let read_int_32 ch =
    let res = ref 0 in
    for i = 0 to 3 do
      res := !res lor (input_byte ch lsl (8 * i))
    done;
    !res

  let write_int_32 ch d =
    let d = ref d in
    for _ = 0 to 3 do
      output_byte ch !d;
      d := !d lsr 8
    done

  let rec uint32 i =
    if i >= 128 || i < 0
    then Char.chr (i lor 128 land 255) :: uint32 (i lsr 7)
    else [ Char.chr i ]

  let uint32 i = uint32 (i land 0xffffffff)

  let section_name = "\005ocaml"

  let read_infos f ch =
    try
      let len = in_channel_length ch in
      seek_in ch (len - 4);
      let n = read_int_32 ch in
      let leb128_n = uint32 n in
      let leb128_len = List.length leb128_n in
      let l = n + 1 + leb128_len in
      if l > len then raise Exit;
      seek_in ch (len - l);
      let s = really_input_string ch l in
      List.iteri ~f:(fun i c -> if Char.(s.[i] <> c) then raise Exit) ('\000' :: leb128_n);
      let name_length = String.length section_name in
      if not
           (String.equal
              (String.sub s ~pos:(leb128_len + 1) ~len:name_length)
              section_name)
      then raise Exit;
      String.split_on_char
        (String.sub s ~pos:(leb128_len + 1 + name_length) ~len:(n - name_length - 4))
        ~sep:'\n'
    with Exit -> failwith ("file " ^ f ^ " contains not link information")

  let write_infos ch l =
    let len = out_channel_length ch in
    seek_out ch len;
    let s = String.concat ~sep:"" l in
    let n = String.length s + String.length section_name + 4 in
    let leb128_n = uint32 n in
    output_byte ch 0;
    List.iter ~f:(fun c -> output_char ch c) leb128_n;
    output_string ch section_name;
    output_string ch s;
    write_int_32 ch n

  type fragments = (string * Javascript.expression) list

  let js_prefix = "//# js:"

  let write ~file ~build_info ?js_runtime ~unit_data () =
    let build_info = Build_info.to_string build_info in
    let unit_data =
      List.map
        ~f:(fun (unit_info, (str, frag)) ->
          [ Unit_info.to_string unit_info
          ; js_prefix
            ^ Yojson.Safe.to_string
                (`Assoc
                  [ "strings", `List (List.map ~f:(fun s -> `String s) str)
                  ; "fragments", `String (Marshal.to_string (frag : fragments) [])
                  ])
            ^ "\n"
          ])
        unit_data
    in
    let js_runtime =
      match js_runtime with
      | None -> []
      | Some (prelude, primitives) ->
          [ js_prefix
            ^ Yojson.Safe.to_string
                (`Assoc [ "prelude", `String prelude; "primitives", `String primitives ])
            ^ "\n"
          ]
    in
    with_open_out file ~flags:[ Open_append ] (fun ch ->
        write_infos ch ((build_info :: js_runtime) @ List.concat unit_data))

  let read ~file =
    let rec parse acc uinfo l =
      match l with
      | [] | [ "" ] -> List.rev acc
      | s :: rem -> (
          match Unit_info.parse uinfo s with
          | Some uinfo -> parse acc uinfo rem
          | None ->
              let js_code =
                match String.drop_prefix ~prefix:js_prefix s with
                | None -> assert false
                | Some s ->
                    let open Yojson.Safe.Util in
                    let info = Yojson.Safe.from_string s in
                    ( info |> member "strings" |> to_list |> List.map ~f:to_string
                    , (info
                       |> member "fragments"
                       |> to_string
                       |> fun s -> Marshal.from_string s 0
                        : fragments) )
              in
              parse ((uinfo, js_code) :: acc) Unit_info.empty rem)
    in
    match
      with_open_in file (fun ch ->
          Wasm_binary.check_header file ch;
          read_infos file ch)
    with
    | [] -> assert false
    | build_info :: l ->
        let l, js =
          match l with
          | [] -> l, None
          | s :: rem -> (
              match String.drop_prefix ~prefix:js_prefix s with
              | None -> l, None
              | Some s ->
                  let data = Yojson.Safe.from_string s in
                  let open Yojson.Safe.Util in
                  ( rem
                  , Some
                      ( data |> member "prelude" |> to_string
                      , data |> member "primitives" |> to_string ) ))
        in
        ( (match Build_info.parse build_info with
          | Some bi -> bi
          | None -> assert false)
        , js
        , parse [] Unit_info.empty l )
end

let generate_start_function ~to_link ~out_file =
  Filename.gen_file out_file
  @@ fun ch ->
  let code, uinfo = Parse_bytecode.predefined_exceptions ~target:`Wasm in
  let context = Wa_generate.start () in
  let toplevel_name, _ =
    Driver.f
      ~target:(Wasm { unit_name = None; context })
      (Parse_bytecode.Debug.create ~include_cmis:false false)
      code
  in
  Wa_generate.add_start_function ~context ~to_link toplevel_name;
  Wa_generate.output ch ~context;
  uinfo.provides

let associated_wasm_file ~js_output_file =
  if Filename.check_suffix js_output_file ".wasm.js"
  then Filename.chop_extension js_output_file
  else Filename.chop_extension js_output_file ^ ".wasm"

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
    wasm_file
    output_file =
  (*ZZZ
    let init_fun =
      match Parse_js.parse (Parse_js.Lexer.of_string Wa_runtime.js_runtime) with
      | [ (Expression_statement f, _) ] -> f
      | _ -> assert false
    in
  *)
  let missing_primitives =
    if Config.Flag.genprim ()
    then
      let l = Wasm_binary.read_imports ~file:tmp_wasm_file in
      List.filter_map
        ~f:(fun { Wasm_binary.module_; name; _ } ->
          if String.equal module_ "env" then Some name else None)
        l
    else []
  in
  report_missing_primitives missing_primitives;
  let generated_buffer = Buffer.create 1024 in
  let f = Pretty_print.to_buffer generated_buffer in
  Pretty_print.set_compact f (not (Config.Flag.pretty ()));
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
  ignore
    (Js_output.program
       f
       [ Javascript.Expression_statement (obj generated_js), Javascript.N ]);
  let s = js_launcher in
  let rec find pat i =
    if String.equal (String.sub s ~pos:i ~len:(String.length pat)) pat
    then i
    else find pat (i + 1)
  in
  let marker = "INSERT_HERE" in
  let i = find marker 0 in
  let rec trim_semi s =
    let l = String.length s in
    if l = 0
    then s
    else
      match s.[l - 1] with
      | ';' | '\n' -> trim_semi (String.sub s ~pos:0 ~len:(l - 1))
      | _ -> s
  in
  Wa_binaryen.gen_file output_file
  @@ fun tmp_output_file ->
  Wa_binaryen.write_file
    ~name:tmp_output_file
    ~contents:
      (prelude
      ^ String.sub s ~pos:0 ~len:i
      ^ "'"
      ^ escape_string (Filename.basename wasm_file)
      ^ "',"
      ^ trim_semi primitives
      ^ ","
      ^ trim_semi (Buffer.contents generated_buffer)
      ^ String.sub
          s
          ~pos:(i + String.length marker)
          ~len:(String.length s - i - String.length marker))

let link ~js_launcher ~output_file ~linkall ~files =
  let t = Timer.make () in
  let files = List.map files ~f:(fun file -> file, Custom_section.read ~file) in
  (match files with
  | [] -> ()
  | (file, (bi, _, _)) :: r ->
      ignore
        (List.fold_left
           ~init:bi
           ~f:(fun bi (file', (bi', _, _)) -> Build_info.merge file bi file' bi')
           r));
  let missing, to_link =
    List.fold_right
      files
      ~init:(StringSet.empty, [])
      ~f:(fun (_file, (build_info, _, units)) acc ->
        let cmo_file =
          match Build_info.kind build_info with
          | `Cmo -> true
          | `Cma | `Exe | `Runtime | `Unknown -> false
        in
        List.fold_right
          units
          ~init:acc
          ~f:(fun ((info : Unit_info.t), _) (requires, to_link) ->
            if (not (Config.Flag.auto_link ()))
               || cmo_file
               || linkall
               || info.force_link
               || not (StringSet.is_empty (StringSet.inter requires info.provides))
            then
              ( StringSet.diff (StringSet.union info.requires requires) info.provides
              , StringSet.elements info.provides @ to_link )
            else requires, to_link))
  in
  Wa_binaryen.with_intermediate_file (Filename.temp_file "start" ".wasm")
  @@ fun start_file ->
  let predefined_exceptions = generate_start_function ~to_link ~out_file:start_file in
  let missing = StringSet.diff missing predefined_exceptions in
  if not (StringSet.is_empty missing)
  then
    failwith
      (Printf.sprintf
         "Could not find compilation unit for %s"
         (String.concat ~sep:", " (StringSet.elements missing)));
  if times () then Format.eprintf "  scan: %a@." Timer.print t;
  let t = Timer.make () in
  let wasm_file = associated_wasm_file ~js_output_file:output_file in
  Wa_binaryen.gen_file wasm_file
  @@ fun tmp_wasm_file ->
  let runtime, other_files =
    List.partition
      ~f:(fun (_, (bi, _, _)) ->
        match Build_info.kind bi with
        | `Runtime -> true
        | `Cmo | `Cma | `Exe | `Unknown -> false)
      files
  in
  Wa_binaryen.with_intermediate_file (Filename.temp_file "dummy" ".wasm")
  @@ fun dummy_file ->
  Wa_binaryen.write_file ~name:dummy_file ~contents:"(module)";
  (* We put first a module with no custom section; otherwise, the
     custom section from the first file is copied to the output *)
  Wa_binaryen.link
    ~debuginfo:true
    ~runtime_files:(dummy_file :: List.map ~f:fst runtime)
    ~input_files:(List.map ~f:fst other_files @ [ start_file ])
    ~output_file:tmp_wasm_file;
  let prelude, primitives =
    match List.find_map ~f:(fun (_, (_, js, _)) -> js) files with
    | None -> failwith "not runtime found"
    | Some js -> js
  in
  let generated_js =
    List.concat
    @@ List.map files ~f:(fun (_, (_, _, units)) ->
           List.map units ~f:(fun ((info : Unit_info.t), js_code) ->
               Some (StringSet.choose info.provides), js_code))
  in
  build_js_runtime
    ~js_launcher
    ~prelude
    ~primitives
    ~generated_js
    ~tmp_wasm_file
    wasm_file
    output_file;
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

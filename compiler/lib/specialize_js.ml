(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 * Copyright (C) 2013 Hugo Heuzard
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
open Code
open Flow

let specialize_instr ~target info i =
  match i, target with
  | Let (x, Prim (Extern "caml_format_int", [ y; z ])), `JavaScript -> (
      (* We can implement the special case where the format string is "%s" in JavaScript
         in a concise and efficient way with [""+x]. It does not make as much sense in
         Wasm to have a special case for this. *)
      match the_string_of ~target info y with
      | Some "%d" -> (
          match the_int ~target info z with
          | Some i -> Let (x, Constant (String (Targetint.to_string i)))
          | None -> Let (x, Prim (Extern "%caml_format_int_special", [ z ])))
      | _ -> i)
  | Let (x, Prim (Extern "%caml_format_int_special", [ z ])), `JavaScript -> (
      match the_int ~target info z with
      | Some i -> Let (x, Constant (String (Targetint.to_string i)))
      | None -> i)
  (* inline the String constant argument so that generate.ml can attempt to parse it *)
  | ( Let
        ( x
        , Prim
            ( Extern (("caml_js_var" | "caml_js_expr" | "caml_pure_js_expr") as prim)
            , [ (Pv _ as y) ] ) )
    , target ) -> (
      match the_string_of ~target info y with
      | Some s -> Let (x, Prim (Extern prim, [ Pc (String s) ]))
      | _ -> i)
  | Let (x, Prim (Extern ("caml_register_named_value" as prim), [ y; z ])), _ -> (
      match the_string_of ~target info y with
      | Some s when Primitive.need_named_value s ->
          Let (x, Prim (Extern prim, [ Pc (String s); z ]))
      | Some _ -> Let (x, Constant (Int Targetint.zero))
      | None -> i)
  | Let (x, Prim (Extern "caml_js_call", [ f; o; a ])), _ -> (
      match the_def_of info a with
      | Some (Block (_, a, _, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_call", f :: o :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_fun_call", [ f; a ])), _ -> (
      match the_def_of info a with
      | Some (Block (_, a, _, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_fun_call", f :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_meth_call", [ o; m; a ])), _ -> (
      match the_string_of ~target info m with
      | Some m when Javascript.is_ident m -> (
          match the_def_of info a with
          | Some (Block (_, a, _, _)) ->
              let a = Array.map a ~f:(fun x -> Pv x) in
              Let
                ( x
                , Prim
                    ( Extern "%caml_js_opt_meth_call"
                    , o
                      :: Pc (NativeString (Native_string.of_string m))
                      :: Array.to_list a ) )
          | _ -> i)
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_new", [ c; a ])), _ -> (
      match the_def_of info a with
      | Some (Block (_, a, _, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_new", c :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_object", [ a ])), _ -> (
      try
        let a =
          match the_def_of info a with
          | Some (Block (_, a, _, _)) -> a
          | _ -> raise Exit
        in
        let a =
          Array.map a ~f:(fun x ->
              match the_def_of info (Pv x) with
              | Some (Block (_, [| k; v |], _, _)) ->
                  let k =
                    match the_string_of ~target info (Pv k) with
                    | Some s when String.is_valid_utf_8 s ->
                        Pc (NativeString (Native_string.of_string s))
                    | Some _ | None -> raise Exit
                  in
                  [ k; Pv v ]
              | Some (Constant (Tuple (0, [| String k; v |], (NotArray | Unknown))))
                when String.is_valid_utf_8 k ->
                  [ Pc (NativeString (Native_string.of_string k)); Pc v ]
              | Some _ | None -> raise Exit)
        in
        Let (x, Prim (Extern "%caml_js_opt_object", List.flatten (Array.to_list a)))
      with Exit -> i)
  | Let (x, Prim (Extern "caml_js_get", [ o; (Pv _ as f) ])), _ -> (
      match the_native_string_of ~target info f with
      | Some s -> Let (x, Prim (Extern "caml_js_get", [ o; Pc (NativeString s) ]))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_set", [ o; (Pv _ as f); v ])), _ -> (
      match the_native_string_of ~target info f with
      | Some s -> Let (x, Prim (Extern "caml_js_set", [ o; Pc (NativeString s); v ]))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_delete", [ o; (Pv _ as f) ])), _ -> (
      match the_native_string_of ~target info f with
      | Some s -> Let (x, Prim (Extern "caml_js_delete", [ o; Pc (NativeString s) ]))
      | _ -> i)
  | Let (x, Prim (Extern ("caml_jsstring_of_string" | "caml_js_from_string"), [ y ])), _
    -> (
      match the_string_of ~target info y with
      | Some s when String.is_valid_utf_8 s ->
          Let (x, Constant (NativeString (Native_string.of_string s)))
      | Some _ | None -> i)
  | Let (x, Prim (Extern "caml_jsbytes_of_string", [ y ])), _ -> (
      match the_string_of ~target info y with
      | Some s -> Let (x, Constant (NativeString (Native_string.of_bytestring s)))
      | None -> i)
  | Let (x, Prim (Extern "%int_mul", [ y; z ])), `JavaScript -> (
      let limit = Targetint.of_int_exn 0x200000 in
      (* Using * to multiply integers in JavaScript yields a float; and if the
           float is large enough, some bits can be lost. So, in the general case,
           we have to use Math.imul. There is no such issue in Wasm. *)
      match the_int ~target info y, the_int ~target info z with
      | Some j, _ when Targetint.(abs j < limit) ->
          Let (x, Prim (Extern "%direct_int_mul", [ y; z ]))
      | _, Some j when Targetint.(abs j < limit) ->
          Let (x, Prim (Extern "%direct_int_mul", [ y; z ]))
      | _ -> i)
  | Let (x, Prim (Extern "%int_div", [ y; z ])), _ -> (
      match the_int ~target info z with
      | Some j when not (Targetint.is_zero j) ->
          Let (x, Prim (Extern "%direct_int_div", [ y; z ]))
      | _ -> i)
  | Let (x, Prim (Extern "%int_mod", [ y; z ])), _ -> (
      match the_int ~target info z with
      | Some j when not (Targetint.is_zero j) ->
          Let (x, Prim (Extern "%direct_int_mod", [ y; z ]))
      | _ -> i)
  | _, _ -> i

let equal2 a b = Code.Var.equal a b

let equal3 a b c = Code.Var.equal a b && Code.Var.equal b c

let equal4 a b c d = Code.Var.equal a b && Code.Var.equal b c && Code.Var.equal c d

let specialize_instrs ~target info l =
  let rec aux info checks l acc =
    match l with
    | [] -> List.rev acc
    | [ ((Let (alen, Prim (Extern "caml_ml_string_length", [ Pv a ])), _) as len1)
      ; ((Let (blen, Prim (Extern "caml_ml_string_length", [ Pv b ])), _) as len2)
      ; ((Let (len, Prim (Extern "%int_add", [ Pv alen'; Pv blen' ])), _) as len3)
      ; (Let (bytes, Prim (Extern "caml_create_bytes", [ Pv len' ])), _)
      ; ( Let
            ( u1
            , Prim
                ( Extern "caml_blit_string"
                , [ Pv a'; Pc (Int zero1); Pv bytes'; Pc (Int zero2); Pv alen'' ] ) )
        , _ )
      ; ( Let
            ( u2
            , Prim
                ( Extern "caml_blit_string"
                , [ Pv b'; Pc (Int zero3); Pv bytes''; Pv alen'''; Pv blen'' ] ) )
        , _ )
      ; (Let (res, Prim (Extern "caml_string_of_bytes", [ Pv bytes''' ])), _)
      ]
      when Targetint.is_zero zero1
           && Targetint.is_zero zero2
           && Targetint.is_zero zero3
           && equal2 a a'
           && equal2 b b'
           && equal2 len len'
           && equal4 alen alen' alen'' alen'''
           && equal3 blen blen' blen''
           && equal4 bytes bytes' bytes'' bytes''' ->
        [ len1
        ; len2
        ; len3
        ; Let (u1, Constant (Int Targetint.zero)), No
        ; Let (u2, Constant (Int Targetint.zero)), No
        ; Let (res, Prim (Extern "caml_string_concat", [ Pv a; Pv b ])), No
        ; Let (bytes, Prim (Extern "caml_bytes_of_string", [ Pv res ])), No
        ]
    | (i, loc) :: r -> (
        (* We make bound checking explicit. Then, we can remove duplicated
           bound checks. Also, it appears to be more efficient to inline
           the array access. The bound checking function returns the array,
           which allows to produce more compact code. *)
        match i with
        | Let
            ( x
            , Prim
                ( Extern
                    (( "caml_array_get"
                     | "caml_array_get_float"
                     | "caml_floatarray_get"
                     | "caml_array_get_addr" ) as prim)
                , [ y; z ] ) ) ->
            let idx =
              match the_int ~target info z with
              | Some idx -> `Cst idx
              | None -> `Var z
            in
            let instr y =
              let prim =
                match prim with
                | "caml_array_get" -> Extern "caml_array_unsafe_get"
                | "caml_array_get_float" | "caml_floatarray_get" ->
                    Extern "caml_floatarray_unsafe_get"
                | "caml_array_get_addr" -> Array_get
                | _ -> assert false
              in
              Let (x, Prim (prim, [ y; z ])), loc
            in
            if List.mem (y, idx) ~set:checks
            then
              let acc = instr y :: acc in
              aux info checks r acc
            else
              let check =
                match prim with
                | "caml_array_get" -> "caml_check_bound_gen"
                | "caml_array_get_float" | "caml_floatarray_get" ->
                    "caml_check_bound_float"
                | "caml_array_get_addr" -> "caml_check_bound"
                | _ -> assert false
              in
              let y' = Code.Var.fresh () in
              let acc =
                instr (Pv y') :: (Let (y', Prim (Extern check, [ y; z ])), noloc) :: acc
              in
              aux info ((y, idx) :: checks) r acc
        | Let
            ( x
            , Prim
                ( Extern
                    (( "caml_array_set"
                     | "caml_array_set_float"
                     | "caml_floatarray_set"
                     | "caml_array_set_addr" ) as prim)
                , [ y; z; t ] ) ) ->
            let idx =
              match the_int ~target info z with
              | Some idx -> `Cst idx
              | None -> `Var z
            in
            let instr y =
              let prim =
                match prim with
                | "caml_array_set" -> "caml_array_unsafe_set"
                | "caml_array_set_float" | "caml_floatarray_set" ->
                    "caml_floatarray_unsafe_set"
                | "caml_array_set_addr" -> "caml_array_unsafe_set_addr"
                | _ -> assert false
              in
              Let (x, Prim (Extern prim, [ y; z; t ])), loc
            in
            if List.mem (y, idx) ~set:checks
            then
              let acc = instr y :: acc in
              aux info checks r acc
            else
              let check =
                match prim with
                | "caml_array_set" -> "caml_check_bound_gen"
                | "caml_array_set_float" | "caml_floatarray_set" ->
                    "caml_check_bound_float"
                | "caml_array_set_addr" -> "caml_check_bound"
                | _ -> assert false
              in
              let y' = Code.Var.fresh () in
              let acc =
                instr (Pv y') :: (Let (y', Prim (Extern check, [ y; z ])), noloc) :: acc
              in
              aux info ((y, idx) :: checks) r acc
        | _ ->
            let i = specialize_instr ~target info i in
            aux info checks r ((i, loc) :: acc))
  in
  aux info [] l []

let specialize_all_instrs ~target info p =
  let blocks =
    Addr.Map.map
      (fun block -> { block with Code.body = specialize_instrs ~target info block.body })
      p.blocks
  in
  { p with blocks }

(****)

let f info p = specialize_all_instrs ~target:(Config.target ()) info p

let f_once p =
  let rec loop acc l =
    match l with
    | [] -> List.rev acc
    | (i, loc) :: r -> (
        match i with
        | Let
            ( x
            , (Prim
                 ( Extern
                     ( "caml_array_set"
                     | "caml_array_unsafe_set"
                     | "caml_array_set_float"
                     | "caml_floatarray_set"
                     | "caml_array_set_addr"
                     | "caml_array_unsafe_set_float"
                     | "caml_floatarray_unsafe_set" )
                 , [ _; _; _ ] ) as p) ) ->
            let x' = Code.Var.fork x in
            let acc =
              (Let (x', p), loc) :: (Let (x, Constant (Int Targetint.zero)), loc) :: acc
            in
            loop acc r
        | _ -> loop ((i, loc) :: acc) r)
  in
  let blocks =
    Addr.Map.map (fun block -> { block with Code.body = loop [] block.body }) p.blocks
  in
  { p with blocks }

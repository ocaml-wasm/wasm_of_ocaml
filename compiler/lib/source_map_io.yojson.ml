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

open Source_map

let json ?replace_mappings t =
  let rewrite_path path =
    if Filename.is_relative path
    then path
    else
      match Build_path_prefix_map.get_build_path_prefix_map () with
      | Some map -> Build_path_prefix_map.rewrite map path
      | None -> path
  in
  `Assoc
    [ "version", `Float (float_of_int t.version)
    ; "file", `String (rewrite_path t.file)
    ; ( "sourceRoot"
      , `String
          (match t.sourceroot with
          | None -> ""
          | Some s -> rewrite_path s) )
    ; "names", `List (List.map (fun s -> `String s) t.names)
    ; "sources", `List (List.map (fun s -> `String (rewrite_path s)) t.sources)
    ; ( "mappings"
      , `String (Option.value ~default:(string_of_mapping t.mappings) replace_mappings) )
    ; ( "sourcesContent"
      , `List
          (match t.sources_content with
          | None -> []
          | Some l ->
              List.map
                (function
                  | None -> `Null
                  | Some s -> `String s)
                l) )
    ]

let invalid () = invalid_arg "Source_map_io.of_json"

let string name rest =
  try
    match List.assoc name rest with
    | `String s -> Some s
    | `Null -> None
    | _ -> invalid ()
  with Not_found -> None

let list_string name rest =
  try
    match List.assoc name rest with
    | `List l ->
        Some
          (List.map
             (function
               | `String s -> s
               | _ -> invalid ())
             l)
    | _ -> invalid ()
  with Not_found -> None

let list_string_opt name rest =
  try
    match List.assoc name rest with
    | `List l ->
        Some
          (List.map
             (function
               | `String s -> Some s
               | `Null -> None
               | _ -> invalid ())
             l)
    | _ -> invalid ()
  with Not_found -> None

let of_json ~parse_mappings json =
  let parse ~version rest =
    let def v d =
      match v with
      | None -> d
      | Some v -> v
    in
    let file = string "file" rest in
    let sourceroot = string "sourceRoot" rest in
    let names = list_string "names" rest in
    let sources = list_string "sources" rest in
    let sources_content = list_string_opt "sourcesContent" rest in
    let mappings = string "mappings" rest in
    ( { version
      ; file = def file ""
      ; sourceroot
      ; names = def names []
      ; sources_content
      ; sources = def sources []
      ; mappings = (if parse_mappings then mapping_of_string (def mappings "") else [])
      }
    , if parse_mappings then None else mappings )
  in
  match json with
  | `Assoc (("version", `Float version) :: rest) when int_of_float version = 3 ->
      parse ~version:3 rest
  | `Assoc (("version", `Int 3) :: rest) -> parse ~version:3 rest
  | _ -> invalid ()

let of_string s = of_json ~parse_mappings:true (Yojson.Basic.from_string s) |> fst

let to_string m = Yojson.Basic.to_string (json m)

let to_file ?mappings m ~file =
  let replace_mappings = mappings in
  Yojson.Basic.to_file file (json ?replace_mappings m)

let of_file_no_mappings filename =
  of_json ~parse_mappings:false (Yojson.Basic.from_file filename)

let enabled = true

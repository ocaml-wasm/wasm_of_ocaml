(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

type t =
  { provides : StringSet.t
  ; requires : StringSet.t
  ; primitives : string list
  ; crcs : (string * Digest.t) list
  ; force_link : bool
  ; effects_without_cps : bool
  }

let empty =
  { provides = StringSet.empty
  ; requires = StringSet.empty
  ; primitives = []
  ; crcs = []
  ; force_link = false
  ; effects_without_cps = false
  }

let of_cmo (cmo : Cmo_format.compilation_unit) =
  let open Ocaml_compiler in
  let provides = StringSet.singleton (Cmo_format.name cmo) in
  let requires = StringSet.of_list (Cmo_format.requires cmo) in
  let requires = StringSet.diff requires provides in
  let effects_without_cps =
    (not (Config.Flag.effects ()))
    && List.exists (Cmo_format.primitives cmo) ~f:(function
           | "%resume" | "%reperform" | "%perform" -> true
           | _ -> false)
  in
  let force_link = Cmo_format.force_link cmo in
  let crcs =
    List.filter_map (Cmo_format.imports cmo) ~f:(fun (s, o) ->
        match o with
        | None -> None
        | Some o -> Some (s, o))
  in
  { provides; requires; primitives = []; force_link; effects_without_cps; crcs }

let union t1 t2 =
  let provides = StringSet.union t1.provides t2.provides in
  let requires = StringSet.union t1.requires t2.requires in
  let requires = StringSet.diff requires provides in
  let primitives = t1.primitives @ t2.primitives in
  let crcs = t1.crcs @ t2.crcs in
  { provides
  ; requires
  ; primitives
  ; force_link = t1.force_link || t2.force_link
  ; effects_without_cps = t1.effects_without_cps || t2.effects_without_cps
  ; crcs
  }

let prefix = "//# unitInfo:"

let to_string t =
  [ [ prefix; "Provides:"; String.concat ~sep:", " (StringSet.elements t.provides) ]
  ; (if StringSet.equal empty.requires t.requires
     then []
     else [ prefix; "Requires:"; String.concat ~sep:", " (StringSet.elements t.requires) ])
  ; (if List.equal ~eq:String.equal empty.primitives t.primitives
     then []
     else [ prefix; "Primitives:"; String.concat ~sep:", " t.primitives ])
  ; (if Bool.equal empty.force_link t.force_link
     then []
     else [ prefix; "Force_link:"; string_of_bool t.force_link ])
  ; (if Bool.equal empty.effects_without_cps t.effects_without_cps
     then []
     else [ prefix; "Effects_without_cps:"; string_of_bool t.effects_without_cps ])
  ]
  |> List.filter_map ~f:(function
         | [] -> None
         | l -> Some (String.concat ~sep:" " l))
  |> String.concat ~sep:"\n"
  |> fun x -> x ^ "\n"

let to_json tbl t : Yojson.Basic.t =
  let add nm skip v rem = if skip then rem else (nm, v) :: rem in
  let set nm f rem =
    add
      nm
      (List.equal ~eq:String.equal (f empty) (f t))
      (`List (List.map ~f:(fun x -> `String x) (f t)))
      rem
  in
  let bool nm f rem = add nm (Bool.equal (f empty) (f t)) (`Bool (f t)) rem in
  (*
  let map nm f conv rem =
    let l = f t |> StringMap.bindings |> List.map ~f:(fun (k, v) -> k, conv v) in
    add nm (List.is_empty l) (`Assoc l) rem
  in
  let opt f x =
    match x with
    | None -> `Null
    | Some x -> f x
  in
  let digest d = `String (Digest.to_hex d) in
  *)
  let digests =
    List.map
      ~f:(fun (k, v) ->
        `Int
          (try Hashtbl.find tbl (k, v)
           with Not_found ->
             let i = Hashtbl.length tbl in
             Hashtbl.add tbl (k, v) i;
             i))
      t.crcs
  in
  `Assoc
    ([]
    |> set "provides" (fun t -> StringSet.elements t.provides)
    |> set "requires" (fun t -> StringSet.elements t.requires)
    |> set "primitives" (fun t -> t.primitives)
    |> bool "force_link" (fun t -> t.force_link)
    |> bool "effects_without_cps" (fun t -> t.effects_without_cps)
    |> add "crcs" (List.is_empty digests) (`List digests))

let from_json tbl t =
  let open Yojson.Basic.Util in
  let opt_list l = l |> to_option to_list |> Option.map ~f:(List.map ~f:to_string) in
  let list default l = Option.value ~default (opt_list l) in
  let set default l =
    Option.value ~default (Option.map ~f:StringSet.of_list (opt_list l))
  in
  let bool default v = Option.value ~default (to_option to_bool v) in
  { provides = t |> member "provides" |> set empty.provides
  ; requires = t |> member "requires" |> set empty.requires
  ; primitives = t |> member "primitives" |> list empty.primitives
  ; force_link = t |> member "force_link" |> bool empty.force_link
  ; effects_without_cps =
      t |> member "effects_without_cps" |> bool empty.effects_without_cps
  ; crcs =
      t
      |> member "crcs"
      |> to_option to_list
      |> Option.value ~default:[]
      |> List.map ~f:(fun i -> tbl.(to_int i))
      (*[]*)
  }

let parse_stringlist s =
  String.split_on_char ~sep:',' s
  |> List.filter_map ~f:(fun s ->
         match String.trim s with
         | "" -> None
         | s -> Some s)

let parse_stringset s = parse_stringlist s |> StringSet.of_list

let parse acc s =
  match String.drop_prefix ~prefix s with
  | None -> None
  | Some suffix -> (
      let suffix = String.trim suffix in
      match String.lsplit2 ~on:':' suffix with
      | None -> None
      | Some ("Provides", provides) ->
          Some
            { acc with
              provides = StringSet.union acc.provides (parse_stringset provides)
            }
      | Some ("Requires", requires) ->
          Some
            { acc with
              requires = StringSet.union acc.requires (parse_stringset requires)
            }
      | Some ("Primitives", primitives) ->
          Some { acc with primitives = acc.primitives @ parse_stringlist primitives }
      | Some ("Force_link", flink) ->
          Some
            { acc with force_link = bool_of_string (String.trim flink) || acc.force_link }
      | Some ("Effects_without_cps", b) ->
          Some { acc with effects_without_cps = bool_of_string (String.trim b) }
      | Some (_, _) -> None)

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

module Wasm_binary : sig
  type import =
    { module_ : string
    ; name : string
    }

  val read_imports : file:string -> import list
end

module Custom_section : sig
  type fragments = (string * Javascript.expression) list

  val write :
       file:string
    -> build_info:Build_info.t
    -> ?js_runtime:string * Javascript.expression
    -> unit_data:(Unit_info.t * (string list * fragments)) list
    -> unit
    -> unit
end

val associated_wasm_file : js_output_file:string -> string

val build_js_runtime :
     js_launcher:string
  -> prelude:string
  -> primitives:Javascript.expression
  -> generated_js:
       (string option * (string list * (string * Javascript.expression) list)) list
  -> tmp_wasm_file:string
  -> ?separate_compilation:bool
  -> ?missing_primitives:string list
  -> string
  -> string
  -> unit

val link :
  js_launcher:string -> output_file:string -> linkall:bool -> files:string list -> unit

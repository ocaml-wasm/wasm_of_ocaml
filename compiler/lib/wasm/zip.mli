(*
open Stdlib*)

type t

val open_out : string -> t

val add_file : t -> name:string -> file:string -> unit

val close_out : t -> unit

(*
type entry =
  { pos : int
  ; size : int
  ; crc : int32
  }

val read_directory : in_channel -> entry StringMap.t

val read_file : entry -> string

val extract_file : entry -> out_channel -> unit
*)

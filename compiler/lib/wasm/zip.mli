open Stdlib

type output

val open_out : string -> output

val add_entry : output -> name:string -> contents:string -> unit

val add_file : output -> name:string -> file:string -> unit

val close_out : output -> unit

type input

val open_in : string -> input

val read_entry : input -> name:string -> string

val close_in : input -> unit
(*
val read_file : entry -> string

val extract_file : entry -> out_channel -> unit
*)

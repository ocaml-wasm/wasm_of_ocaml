type t
val open_out : string -> t
val add_file : t -> name:string -> file:string -> unit
val close_out : t -> unit

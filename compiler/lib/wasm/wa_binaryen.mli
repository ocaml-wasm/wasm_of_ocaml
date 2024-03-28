val link :
  runtime_files:string list -> input_files:string list -> output_file:string -> unit

val dead_code_elimination :
  dependencies:string -> input_file:string -> output_file:string -> Stdlib.StringSet.t

val optimize :
  profile:Driver.profile option -> input_file:string -> output_file:string -> unit

val gen_file : string -> (string -> 'a) -> 'a

val write_file : name:string -> contents:string -> unit

val with_intermediate_file : ?keep:bool -> string -> (string -> 'a) -> 'a

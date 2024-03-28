val link :
     enable_source_maps:bool
  -> runtime_files:string list
  -> input_files:string list
  -> output_file:string
  -> unit

val dead_code_elimination :
     dependencies:string
  -> enable_source_maps:bool
  -> input_file:string
  -> output_file:string
  -> Stdlib.StringSet.t

val optimize :
     profile:Driver.profile option
  -> ?input_sourcemap_file:string
  -> ?output_sourcemap_file:string
  -> ?sourcemap_url:string
  -> input_file:string
  -> output_file:string
  -> unit
  -> unit

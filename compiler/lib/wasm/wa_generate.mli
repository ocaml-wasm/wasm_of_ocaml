val init : unit -> unit

val f :
  globals:Code.Var.Set.t -> out_channel -> Code.program -> live_vars:int array -> unit

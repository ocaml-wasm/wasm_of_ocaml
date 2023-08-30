val init : unit -> unit

val f :
     out_channel
  -> Code.program
  -> live_vars:int array
  -> cps_calls:Effects.cps_calls
  -> in_cps:Effects.in_cps
  -> unit

type stack = Code.Var.t option list

type spilling_info =
  { reloads : (Code.Var.t * int) list
  ; depth_change : int
  ; spills : (Code.Var.t * int) list
  ; stack : stack
  }

type block_info =
  { initial_depth : int
  ; spilling : spilling_info
  }

type info =
  { max_depth : int
  ; subcalls : bool
  ; initial_spilling : spilling_info
  ; block : block_info Code.Addr.Map.t
  ; instr : spilling_info Code.Var.Map.t
  }

val f :
     Code.program
  -> context:Wa_code_generation.context
  -> closures:Wa_closure_conversion.closure Code.Var.Map.t
  -> pc:int
  -> params:Code.Var.t list
  -> unit

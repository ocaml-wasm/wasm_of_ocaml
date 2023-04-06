(*
type stack = Code.Var.t option list

type spilling_info =
  { reloads : (Code.Var.t * int) list
  ; depth_change : int
  ; spills : (Code.Var.t * int) list
  ; stack : stack
  }

type block_info =
  { initial_depth : int
  ; loaded_variables : Code.Var.Set.t
  ; spilling : spilling_info
  }

type info =
  { max_depth : int
  ; subcalls : bool
  ; initial_spilling : spilling_info
  ; block : block_info Code.Addr.Map.t
  ; instr : spilling_info Code.Var.Map.t
  }
*)

type info

val generate_spilling_information :
     Code.program
  -> context:Wa_code_generation.context
  -> closures:Wa_closure_conversion.closure Code.Var.Map.t
  -> pc:Code.Addr.t
  -> params:Code.Var.t list
  -> info

type ctx

val start_function : info -> ctx

val start_block : info -> Code.Addr.t -> ctx

val perform_reloads : ctx -> Code.Print.xinstr -> ctx Wa_code_generation.t

val perform_spilling :
     ctx
  -> [ `Function | `Instr of Code.Var.t | `Block of Code.Addr.t ]
  -> ctx Wa_code_generation.t

val kill_variables : ctx -> ctx

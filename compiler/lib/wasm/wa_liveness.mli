type block_info =
  { initially_live : Code.Var.Set.t (* Live at start of block *)
  ; live_before_branch : Code.Var.Set.t
  }

type info =
  { instr : Code.Var.Set.t Code.Var.Map.t (* Live variables at spilling point *)
  ; block : block_info Code.Addr.Map.t
  }

val f :
     blocks:Code.block Code.Addr.Map.t
  -> context:Wa_code_generation.context
  -> closures:Wa_closure_conversion.closure Code.Var.Map.t
  -> domain:Code.Addr.Set.t
  -> vars:Code.Var.Set.t
  -> pc:int
  -> info

type instr_info =
  { live_vars : Code.Var.Set.t (* Live variables at spilling point *)
  ; no_longer_live : Code.Var.Set.t
        (* Variable used after spilling point but no longer live after
           the instruction *)
  }

type block_info =
  { initially_live : Code.Var.Set.t (* Live at start of block *)
  ; branch : instr_info
  }

type info =
  { instr : instr_info Code.Var.Map.t
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

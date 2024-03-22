val f :
     param_count:int
  -> local_types:(Wa_ast.var * Wa_ast.value_type option) array
  -> Wa_ast.instruction list
  -> (Wa_ast.var * Wa_ast.value_type option) array * Wa_ast.instruction list

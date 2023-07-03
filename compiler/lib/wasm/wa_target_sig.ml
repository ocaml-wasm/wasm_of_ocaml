module type S = sig
  type expression = Wa_code_generation.expression

  module Memory : sig
    val allocate :
      tag:int -> [ `Expr of Wa_ast.expression | `Var of Wa_ast.var ] list -> expression

    val load_function_pointer :
         arity:int
      -> expression
      -> ([ `Index | `Ref of Wa_ast.var ] * Wa_ast.expression) Wa_code_generation.t

    val load_function_arity : expression -> expression

    val tag : expression -> expression

    val field : expression -> int -> expression

    val set_field : expression -> int -> expression -> unit Wa_code_generation.t

    val array_get : expression -> expression -> expression

    val array_set : expression -> expression -> expression -> unit Wa_code_generation.t

    val bytes_get : expression -> expression -> expression

    val bytes_set : expression -> expression -> expression -> unit Wa_code_generation.t

    val block_length : expression -> expression
  end

  module Value : sig
    val value : Wa_ast.value_type

    val unit : expression

    val val_int : expression -> expression

    val int_val : expression -> expression

    val check_is_not_zero : expression -> expression
    (** Returns an int32 value *)

    val check_is_int : expression -> expression
    (** Returns an int32 value *)

    val not : expression -> expression

    val lt : expression -> expression -> expression

    val le : expression -> expression -> expression

    val eq : expression -> expression -> expression

    val neq : expression -> expression -> expression

    val ult : expression -> expression -> expression

    val is_int : expression -> expression

    val int_add : expression -> expression -> expression

    val int_sub : expression -> expression -> expression

    val int_mul : expression -> expression -> expression

    val int_neg : expression -> expression

    val int_or : expression -> expression -> expression

    val int_and : expression -> expression -> expression

    val int_xor : expression -> expression -> expression

    val int_lsl : expression -> expression -> expression

    val int_lsr : expression -> expression -> expression

    val int_asr : expression -> expression -> expression
  end

  module Constant : sig
    val translate : Code.constant -> expression
  end

  module Closure : sig
    val translate :
         context:Wa_code_generation.context
      -> closures:Wa_closure_conversion.closure Code.Var.Map.t
      -> Code.Var.t
      -> expression

    val bind_environment :
         context:Wa_code_generation.context
      -> closures:Wa_closure_conversion.closure Code.Var.Map.t
      -> Code.Var.t
      -> unit Wa_code_generation.t

    val curry_allocate :
         arity:int
      -> int
      -> f:Wa_ast.symbol
      -> closure:Code.Var.t
      -> arg:Code.Var.t
      -> Wa_ast.expression Wa_code_generation.t

    val curry_load :
      arity:int -> int -> Code.Var.t -> (expression * expression) Wa_code_generation.t
  end

  val entry_point :
    register_primitive:(string -> Wa_ast.func_type -> unit) -> unit Wa_code_generation.t

  module Stack : sig
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

    val perform_reloads : ctx -> Code.Print.xinstr -> unit Wa_code_generation.t

    val perform_spilling :
         ctx
      -> [ `Function | `Instr of Code.Var.t | `Block of Code.Addr.t ]
      -> unit Wa_code_generation.t

    val kill_variables : ctx -> unit
  end
end

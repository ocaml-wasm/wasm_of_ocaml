type 'a t

type expression = Wa_ast.expression t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t

val instr : Wa_ast.instruction -> unit t

val seq : unit t -> expression -> expression

module Arith : sig
  val const : int32 -> expression

  val to_int31 : expression -> expression

  val of_int31 : expression -> expression

  val ( + ) : expression -> expression -> expression

  val ( - ) : expression -> expression -> expression

  val ( * ) : expression -> expression -> expression

  val ( lsl ) : expression -> expression -> expression

  val ( lsr ) : expression -> expression -> expression

  val ( asr ) : expression -> expression -> expression

  val ( land ) : expression -> expression -> expression

  val ( lor ) : expression -> expression -> expression

  val ( lxor ) : expression -> expression -> expression

  val ( < ) : expression -> expression -> expression

  val ( <= ) : expression -> expression -> expression

  val ( = ) : expression -> expression -> expression

  val ( <> ) : expression -> expression -> expression

  val ult : expression -> expression -> expression

  val eqz : expression -> expression
end

val load : Wa_ast.var -> expression

val tee : Wa_ast.var -> expression -> expression

val store : ?always:bool -> Wa_ast.var -> expression -> unit t

val assign : Wa_ast.var -> expression -> unit t

val drop : expression -> unit t

val loop : Wa_ast.func_type -> unit t -> unit t

val block : Wa_ast.func_type -> unit t -> unit t

val if_ : Wa_ast.func_type -> expression -> unit t -> unit t -> unit t

val get_constant_data : Code.Var.t -> Wa_ast.data list t

val get_constant_data_table : Wa_ast.data list Code.Var.Map.t ref t

val add_var : Wa_ast.var -> int t

val define_var : Wa_ast.var -> expression -> unit t

val function_body :
     constants:(Wa_ast.var, Wa_ast.expression) Hashtbl.t
  -> constant_data:Wa_ast.data list Code.Var.Map.t ref
  -> body:unit t
  -> int * Wa_ast.instruction list

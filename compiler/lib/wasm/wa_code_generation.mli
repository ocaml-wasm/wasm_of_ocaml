type constant_global

type context =
  { constants : (Code.Var.t, Wa_ast.expression) Hashtbl.t
  ; mutable data_segments : (bool * Wa_ast.data list) Code.Var.Map.t
  ; mutable constant_globals : constant_global Code.Var.Map.t
  ; mutable other_fields : Wa_ast.module_field list
  ; types : (string, Code.Var.t) Hashtbl.t
  ; mutable closure_envs : Code.Var.t Code.Var.Map.t
        (** GC: mapping of recursive functions to their shared environment *)
  }

val make_context : unit -> context

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

val cast : ?nullable:bool -> Wa_ast.heap_type -> expression -> expression

val load : Wa_ast.var -> expression

val tee : Wa_ast.var -> expression -> expression

val store : ?always:bool -> Wa_ast.var -> expression -> unit t

val assign : Wa_ast.var -> expression -> unit t

val drop : expression -> unit t

val loop : Wa_ast.func_type -> unit t -> unit t

val block : Wa_ast.func_type -> unit t -> unit t

val if_ : Wa_ast.func_type -> expression -> unit t -> unit t -> unit t

val add_var : Wa_ast.var -> int t

val define_var : Wa_ast.var -> expression -> unit t

val is_small_constant : Wa_ast.expression -> bool t

val register_type :
  string -> ?supertype:Wa_ast.var -> ?final:bool -> Wa_ast.str_type -> Wa_ast.var t

val register_global :
  Wa_ast.symbol -> ?constant:bool -> Wa_ast.global_type -> Wa_ast.expression -> unit t

val get_global : Wa_ast.symbol -> Wa_ast.expression option t

val register_data_segment : Code.Var.t -> active:bool -> Wa_ast.data list -> unit t

val get_data_segment : Code.Var.t -> (bool * Wa_ast.data list) t

val get_context : context t

val set_closure_env : Code.Var.t -> Code.Var.t -> unit t

val get_closure_env : Code.Var.t -> Code.Var.t t

val is_closure : Code.Var.t -> bool t

val function_body : context:context -> body:unit t -> int * Wa_ast.instruction list

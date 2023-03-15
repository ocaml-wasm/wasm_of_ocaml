open! Stdlib
open Code
module W = Wa_ast

(*
LLVM type checker does not work well. It does not handle 'br', and
there is a bug with `return` in clang 15.
Use 'clang-16 --target=wasm32 -Wa,--no-type-check' to disable it.
https://github.com/llvm/llvm-project/issues/56935
https://github.com/llvm/llvm-project/issues/58438
*)

(* binaryen does not support block input parameters
   https://github.com/WebAssembly/binaryen/issues/5047 *)

type constant_global =
  { init : W.expression option
  ; constant : bool
  }

type context =
  { constants : (Var.t, W.expression) Hashtbl.t
  ; mutable data_segments : (bool * W.data list) Var.Map.t
  ; mutable constant_globals : constant_global Var.Map.t
  ; mutable other_fields : W.module_field list
  ; types : (string, Var.t) Hashtbl.t
  ; mutable closure_envs : Var.t Var.Map.t
        (** GC: mapping of recursive functions to their shared environment *)
  ; mutable use_exceptions : bool
  ; mutable apply_funs : Var.t IntMap.t
  ; mutable curry_funs : Var.t IntMap.t
  }

let make_context () =
  { constants = Hashtbl.create 128
  ; data_segments = Var.Map.empty
  ; constant_globals = Var.Map.empty
  ; other_fields = []
  ; types = Hashtbl.create 128
  ; closure_envs = Var.Map.empty
  ; use_exceptions = false
  ; apply_funs = IntMap.empty
  ; curry_funs = IntMap.empty
  }

type var =
  | Local of int
  | Expr of W.expression t

and state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : W.instruction list
  ; context : context
  }

and 'a t = state -> 'a * state

type expression = Wa_ast.expression t

let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
 fun st ->
  let v, st = e st in
  f v st

let return x st = x, st

let expression_list f l =
  let rec loop acc l =
    match l with
    | [] -> return (List.rev acc)
    | x :: r ->
        let* x = f x in
        loop (x :: acc) r
  in
  loop [] l

let register_data_segment x ~active v st =
  st.context.data_segments <- Var.Map.add x (active, v) st.context.data_segments;
  (), st

let get_data_segment x st = Var.Map.find x st.context.data_segments, st

let get_context st = st.context, st

let register_constant x e st =
  Hashtbl.add st.context.constants x e;
  (), st

type type_def =
  { supertype : Wa_ast.var option
  ; final : bool
  ; typ : Wa_ast.str_type
  }

let register_type nm gen_typ st =
  let context = st.context in
  let { supertype; final; typ }, st = gen_typ () st in
  ( (try Hashtbl.find context.types nm
     with Not_found ->
       let name = Var.fresh_n nm in
       context.other_fields <-
         Type [ { name; typ; supertype; final } ] :: context.other_fields;
       Hashtbl.add context.types nm name;
       name)
  , st )

let register_global name ?(constant = false) typ init st =
  st.context.other_fields <- W.Global { name; typ; init } :: st.context.other_fields;
  (match name with
  | S _ -> ()
  | V nm ->
      st.context.constant_globals <-
        Var.Map.add
          nm
          { init = (if not typ.mut then Some init else None)
          ; constant = (not typ.mut) || constant
          }
          st.context.constant_globals);
  (), st

let global_is_constant name =
  let* ctx = get_context in
  return
    (match Var.Map.find_opt name ctx.constant_globals with
    | Some { constant = true; _ } -> true
    | _ -> false)

let get_global (name : Wa_ast.symbol) =
  match name with
  | S _ -> return None
  | V name ->
      let* ctx = get_context in
      return
        (match Var.Map.find_opt name ctx.constant_globals with
        | Some { init; _ } -> init
        | _ -> None)

let set_closure_env f env st =
  st.context.closure_envs <- Var.Map.add f env st.context.closure_envs;
  (), st

let get_closure_env f st = Var.Map.find f st.context.closure_envs, st

let is_closure f st = Var.Map.mem f st.context.closure_envs, st

let var x st =
  try Var.Map.find x st.vars, st
  with Not_found -> (
    try Expr (return (Hashtbl.find st.context.constants x)), st
    with Not_found ->
      Format.eprintf "ZZZ %a@." Var.print x;
      Local 0, st)

let add_var x ({ var_count; vars; _ } as st) =
  match Var.Map.find_opt x vars with
  | Some (Local i) -> i, st
  | Some (Expr _) -> assert false
  | None ->
      let i = var_count in
      let vars = Var.Map.add x (Local i) vars in
      i, { st with var_count = var_count + 1; vars }

let define_var x e st = (), { st with vars = Var.Map.add x (Expr e) st.vars }

let instr i : unit t = fun st -> (), { st with instrs = i :: st.instrs }

let instrs l : unit t = fun st -> (), { st with instrs = List.rev_append l st.instrs }

let blk l st =
  let instrs = st.instrs in
  let (), st = l { st with instrs = [] } in
  List.rev st.instrs, { st with instrs }

let cast ?(nullable = false) typ e =
  let* e = e in
  match typ, e with
  | W.I31, W.I31New _ -> return e
  | _ -> return (W.RefCast ({ W.nullable; typ }, e))

module Arith = struct
  let binary op e e' =
    let* e = e in
    let* e' = e' in
    return (W.BinOp (I32 op, e, e'))

  let unary op e =
    let* e = e in
    return (W.UnOp (I32 op, e))

  let ( + ) e e' =
    let* e = e in
    let* e' = e' in
    return
      (match e, e' with
      | W.BinOp (I32 Add, e1, W.Const (I32 n)), W.Const (I32 n') ->
          let n'' = Int32.add n n' in
          if Int32.equal n'' 0l
          then e1
          else W.BinOp (I32 Add, e1, W.Const (I32 (Int32.add n n')))
      | W.Const (I32 n), W.Const (I32 n') -> W.Const (I32 (Int32.add n n'))
      | W.Const (I32 0l), _ -> e'
      | _, W.Const (I32 0l) -> e
      | W.ConstSym (sym, offset), W.Const (I32 n) ->
          W.ConstSym (sym, offset + Int32.to_int n)
      | W.Const _, _ -> W.BinOp (I32 Add, e', e)
      | _ -> W.BinOp (I32 Add, e, e'))

  let ( - ) e e' =
    let* e = e in
    let* e' = e' in
    return
      (match e, e' with
      | W.BinOp (I32 Add, e1, W.Const (I32 n)), W.Const (I32 n') ->
          let n'' = Int32.sub n n' in
          if Int32.equal n'' 0l then e1 else W.BinOp (I32 Add, e1, W.Const (I32 n''))
      | W.Const (I32 n), W.Const (I32 n') -> W.Const (I32 (Int32.sub n n'))
      | _, W.Const (I32 n) ->
          if Int32.equal n 0l then e else W.BinOp (I32 Add, e, W.Const (I32 (Int32.neg n)))
      | _ -> W.BinOp (I32 Sub, e, e'))

  let ( * ) = binary Mul

  let ( lsl ) e e' =
    let* e = e in
    let* e' = e' in
    return
      (match e, e' with
      | W.Const (I32 n), W.Const (I32 n') when Poly.(n' < 31l) ->
          W.Const (I32 (Int32.shift_left n (Int32.to_int n')))
      | _ -> W.BinOp (I32 Shl, e, e'))

  let ( lsr ) = binary (Shr U)

  let ( asr ) = binary (Shr S)

  let ( land ) = binary And

  let ( lor ) = binary Or

  let ( lxor ) = binary Xor

  let ( < ) = binary (Lt S)

  let ( <= ) = binary (Le S)

  let ( = ) = binary Eq

  let ( <> ) = binary Ne

  let ult = binary (Lt U)

  let eqz = unary Eqz

  let const n = return (W.Const (I32 n))

  let to_int31 n =
    let* n = n in
    match n with
    | W.I31Get (S, n') -> return n'
    | _ -> return (W.I31New n)

  let of_int31 n =
    let* n = n in
    match n with
    | W.I31New (Const (I32 _) as c) -> return c (*ZZZ Overflow *)
    | _ -> return (W.I31Get (S, n))
end

let is_small_constant e =
  match e with
  | W.ConstSym _ | W.Const _ | W.I31New (W.Const _) | W.RefFunc _ -> return true
  | W.GlobalGet (V name) -> global_is_constant name
  | _ -> return false

let load x =
  let* x = var x in
  match x with
  | Local x -> return (W.LocalGet x)
  | Expr e -> e

let tee x e =
  let* e = e in
  let* b = is_small_constant e in
  if b
  then
    let* () = register_constant x e in
    return e
  else
    let* i = add_var x in
    return (W.LocalTee (i, e))

let rec store ?(always = false) x e =
  let* e = e in
  match e with
  | W.Seq (l, e') ->
      let* () = instrs l in
      store ~always x (return e')
  | _ ->
      let* b = is_small_constant e in
      if b && not always
      then register_constant x e
      else
        let* i = add_var x in
        instr (LocalSet (i, e))

let assign x e =
  let* x = var x in
  let* e = e in
  match x with
  | Local x -> instr (W.LocalSet (x, e))
  | Expr _ -> assert false

let seq l e =
  let* instrs = blk l in
  let* e = e in
  return (W.Seq (instrs, e))

let drop e =
  let* e = e in
  match e with
  | W.Seq (l, Const _) -> instrs l
  | _ -> instr (Drop e)

let loop ty l =
  let* instrs = blk l in
  instr (Loop (ty, instrs))

let block ty l =
  let* instrs = blk l in
  instr (Block (ty, instrs))

let if_ ty e l1 l2 =
  let* e = e in
  let* instrs1 = blk l1 in
  let* instrs2 = blk l2 in
  match e with
  | W.UnOp (I32 Eqz, e') -> instr (If (ty, e', instrs2, instrs1))
  | _ -> instr (If (ty, e, instrs1, instrs2))

let try_ ty body exception_name handler =
  let* body = blk body in
  let* handler = blk handler in
  instr (Try (ty, body, [ exception_name, handler ], None))

let use_exceptions st =
  st.context.use_exceptions <- true;
  (), st

let need_apply_fun ~arity st =
  let ctx = st.context in
  ( (try IntMap.find arity ctx.apply_funs
     with Not_found ->
       let x = Var.fresh_n (Printf.sprintf "apply_%d" arity) in
       ctx.apply_funs <- IntMap.add arity x ctx.apply_funs;
       x)
  , st )

let need_curry_fun ~arity st =
  let ctx = st.context in
  ( (try IntMap.find arity ctx.curry_funs
     with Not_found ->
       let x = Var.fresh_n (Printf.sprintf "curry_%d" arity) in
       ctx.curry_funs <- IntMap.add arity x ctx.curry_funs;
       x)
  , st )

let function_body ~context ~param_count ~body =
  let st = { var_count = 0; vars = Var.Map.empty; instrs = []; context } in
  let (), st = body st in
  let local_count, body = st.var_count, List.rev st.instrs in
  let body = Wa_tail_call.f body in
  if false then local_count, body else Wa_minimize_locals.f ~param_count ~local_count body

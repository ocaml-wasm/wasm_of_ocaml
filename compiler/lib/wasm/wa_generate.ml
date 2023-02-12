module Types = Wasm.Types
module Operators = Wasm.Operators
module Ast = Wasm.Ast
module I32 = Wasm.I32
module Source = Wasm.Source
open Code

type var =
  | Global of Ast.var
  | Local of Ast.var
  | Env of Ast.var * int

type state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : Ast.instr list
  }

type ctx =
  { live : int array
  ; global : bool
  }

type 'a t = state -> 'a * state

let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
 fun st ->
  let v, st = e st in
  f v st

let return x st = x, st

let var x st = Var.Map.find x st.vars, st

let instr i : unit t =
 fun st -> (), { st with instrs = Source.(i @@ no_region) :: st.instrs }

module Arith = struct
  let binary op e e' =
    let* () = e in
    let* () = e' in
    instr op

  let ( + ) = binary Operators.i32_add

  let ( lsl ) = binary Operators.i32_shl

  let const n = instr Operators.(i32_const Source.(Int32.of_int n @@ no_region))
end

module Memory = struct
  let load ?(offset = 0) e =
    let* () = e in
    instr (Operators.i32_load 4 (Int32.of_int offset))

  let store ?(offset = 0) e e' =
    let* () = e in
    let* () = e' in
    instr (Operators.i32_store 4 (Int32.of_int offset))

  let allocate ~tag:_ ~args:_ = [] (*ZZZ*)

  let tag e = load ~offset:(-4) e

  let array_get e e' = load ~offset:(-2) Arith.(e + (e' lsl const 2))

  let array_set e e' e'' = store ~offset:(-2) Arith.(e + (e' lsl const 2)) e''

  let field e idx = load ~offset:(4 * idx) e

  let set_field e idx e' = store ~offset:(4 * idx) e e'
end

let load x : unit t =
  let* x = var x in
  match x with
  | Local x -> instr (Operators.local_get x)
  | Global x -> instr (Operators.global_get x)
  | Env (x, offset) -> Memory.load ~offset (instr (Operators.local_get x))

let assign x e =
  let* () = e in
  let* x = var x in
  match x with
  | Local x -> instr (Operators.local_set x)
  | Global x -> instr (Operators.global_set x)
  | Env _ -> assert false

let store ctx x e =
  ( let* ) e (fun () ({ var_count; vars; _ } as st) ->
      let i = Source.(Int32.of_int var_count @@ no_region) in
      let vars = Var.Map.add x (if ctx.global then Global i else Local i) vars in
      instr
        (if ctx.global then Operators.global_set i else Operators.local_set i)
        { st with var_count = var_count + 1; vars })

let rec translate_expr e =
  match e with
  | Apply { f = _; args = _; exact } ->
      if exact
      then
        let* () =
          (* push arguments *)
          return ()
        in
        let* () = return () (* load function *) in
        instr Operators.(call_indirect Source.(0l @@ no_region) Source.(0l @@ no_region))
      else return () (*ZZZ*)
  | Block (_tag, _a, _array_or_not) -> return () (*ZZZ*)
  | Field (x, n) -> Memory.field (load x) n
  | Closure (_args, ((_pc, _) as _cont)) -> return () (*ZZZ*)
  | Constant _c -> return () (*ZZZ*)
  | Prim (_p, _l) -> return () (*ZZZ *)

and translate_instr ctx i =
  match i with
  | Assign (x, y) -> assign x (load y)
  | Let (x, e) ->
      if ctx.live.(Var.idx x) = 0
      then
        let* () = translate_expr e in
        instr Operators.drop
      else store ctx x (translate_expr e)
  | Set_field (x, n, y) -> Memory.set_field (load x) n (load y)
  | Offset_ref (x, n) ->
      Memory.set_field (load x) 0 Arith.(Memory.field (load x) 0 + const (2 * n))
  | Array_set (x, y, z) -> Memory.array_set (load x) (load y) (load z)

(*
let compile_closure ctx (pc, args) =
  let st = build_graph ctx pc in
  let current_blocks = !(st.visited_blocks) in
  st.visited_blocks := Addr.Set.empty;
  if debug () then Format.eprintf "@[<hv 2>closure {@;";
  let backs = Addr.Set.empty in
  let loop_stack = [] in
  incr_seen st pc;
  let _never, res =
    compile_branch st [] (pc, args) loop_stack backs Addr.Set.empty Interm.empty
  in
  if Addr.Set.cardinal !(st.visited_blocks) <> Addr.Set.cardinal current_blocks
  then (
    let missing = Addr.Set.diff current_blocks !(st.visited_blocks) in
    Format.eprintf "Some blocks not compiled %s!@." (string_of_set missing);
    assert false);
  if debug () then Format.eprintf "}@]@;";
  res

let compile_program ctx pc =
  let res = compile_closure ctx (pc, []) in
  res

let f
    (p : Code.program)
    ~exported_runtime
    ~live_vars
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
    debug =
  let ctx = { live = live_vars } in
  let p = compile_program ctx p.start in
  p
*)

module Types = Wasm.Types
module Operators = Wasm.Operators
module Ast = Wasm.Ast
module I32 = Wasm.I32
module Source = Wasm.Source
open Code

type ctx =
  { live : int array
  ; global : bool
  ; blocks : block Addr.Map.t
  }

type var =
  | Global of Ast.var
  | Local of Ast.var
  | Env of Ast.var * int

type state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : Ast.instr list
  }

type 'a t = state -> 'a * state

let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
 fun st ->
  let v, st = e st in
  f v st

let return x st = x, st

let var x st = Var.Map.find x st.vars, st

let add_var ~global x ({ var_count; vars; _ } as st) =
  let i = Source.(Int32.of_int var_count @@ no_region) in
  let vars = Var.Map.add x (if global then Global i else Local i) vars in
  i, { st with var_count = var_count + 1; vars }

let instr i : unit t =
 fun st -> (), { st with instrs = Source.(i @@ no_region) :: st.instrs }

let blk l st =
  let instrs = st.instrs in
  let (), st = l { st with instrs = [] } in
  List.rev st.instrs, { st with instrs }

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

  let allocate ~tag:_ _a _array_or_not = return () (*ZZZ Float array?*)

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

let drop e =
  let* () = e in
  instr Operators.drop

let loop l =
  let* instrs = blk l in
  instr (Operators.loop (ValBlockType None) instrs)

let block l =
  let* instrs = blk l in
  instr (Operators.block (ValBlockType None) instrs)

let if_ l1 l2 =
  let* instrs1 = blk l1 in
  let* instrs2 = blk l2 in
  instr (Operators.if_ (ValBlockType None) instrs1 instrs2)

let store ctx x e =
  let* () = e in
  let* i = add_var ~global:ctx.global x in
  instr (if ctx.global then Operators.global_set i else Operators.local_set i)

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
  | Block (tag, a, array_or_not) -> Memory.allocate ~tag a array_or_not
  | Field (x, n) -> Memory.field (load x) n
  | Closure (_args, ((_pc, _) as _cont)) -> return () (*ZZZ*)
  | Constant _c -> return () (*ZZZ*)
  | Prim (_p, _l) -> return () (*ZZZ *)

and translate_instr ctx i =
  match i with
  | Assign (x, y) -> assign x (load y)
  | Let (x, e) ->
      if ctx.live.(Var.idx x) = 0
      then drop (translate_expr e)
      else store ctx x (translate_expr e)
  | Set_field (x, n, y) -> Memory.set_field (load x) n (load y)
  | Offset_ref (x, n) ->
      Memory.set_field (load x) 0 Arith.(Memory.field (load x) 0 + const (2 * n))
  | Array_set (x, y, z) -> Memory.array_set (load x) (load y) (load z)

and translate_instrs ctx l =
  match l with
  | [] -> return ()
  | i :: rem ->
      let* () = translate_instr ctx i in
      translate_instrs ctx rem

let compile_closure ctx _name_opt _params (pc, _args) =
  let g = Wa_structure.build_graph ctx.blocks pc in
  let idom = Wa_structure.dominator_tree g in
  let _dom = Wa_structure.reverse_tree idom in

  let rec index pc i context =
    match context with
    | (`Loop pc' | `Block pc') :: _ when pc = pc' -> i
    | _ :: rem -> index pc (i + 1) rem
    | [] -> assert false
  in
  let open Source in
  let rec do_tree pc context =
    let code =
      node_within
        pc
        (List.filter
           (fun pc' -> Wa_structure.is_merge_node g pc')
           (List.rev (Addr.Set.elements (Wa_structure.get_edges g.succs pc))))
    in
    if Wa_structure.is_loop_header g pc
    then loop (code (`Loop pc :: context))
    else code context
  and node_within pc l context =
    match l with
    | pc' :: rem ->
        let* () = block (node_within pc rem (`Block pc' :: context)) in
        do_tree pc' context
    | [] -> (
        let block = Addr.Map.find pc ctx.blocks in
        let* () = translate_instrs ctx block.body in
        match block.branch with
        | Branch (pc', _) -> (*ZZZ*) do_branch pc pc' context
        | Return x ->
            let* () = load x in
            instr Operators.return
        | Cond (x, (pc1, _), (pc2, _)) ->
            let* () = load x in
            if_ (do_branch pc pc1 (`If :: context)) (do_branch pc pc2 (`If :: context))
        | Stop -> instr Operators.return
        | Switch (_, _, _) -> assert false
        | Raise _ | Pushtrap _ | Poptrap _ -> assert false (*ZZZ*))
  and do_branch src dst context =
    if Wa_structure.is_backward g src dst || Wa_structure.is_merge_node g dst
    then instr (Operators.br (Int32.of_int (index dst 0 context) @@ no_region))
    else do_tree dst context
  in
  ignore (do_tree pc [] { var_count = 0; vars = Var.Map.empty; instrs = [] })

let compile_program ctx pc =
  let res = compile_closure ctx (pc, []) in
  res

let f
    (p : Code.program)
    ~live_vars
    (*
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
*)
      _debug =
  let ctx = { live = live_vars; blocks = p.blocks; global = true } in
  Code.fold_closures
    p
    (fun name_opt params cont () -> compile_closure ctx name_opt params cont)
    ()

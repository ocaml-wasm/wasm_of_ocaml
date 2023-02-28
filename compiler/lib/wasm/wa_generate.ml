(*
module Types = Wasm.Types
module Operators = Wasm.Operators
module Ast = Wasm.Ast
module I32 = Wasm.I32
module Source = Wasm.Source
*)
open! Stdlib
open Code
module W = Wa_ast

type primitive =
  { index : int
  ; arity : int
  }

type ctx =
  { live : int array
  ; blocks : block Addr.Map.t
  ; closures : Wa_closure_conversion.closure Var.Map.t
  ; mutable primitives : W.func_type StringMap.t
  }

type var =
  | Local of int
  | Env of int * int
  | Rec of int * int

type state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : W.instruction list
  }

type 'a t = state -> 'a * state

let func_type n =
  { W.params = List.init ~len:n ~f:(fun _ : W.value_type -> I32); result = I32 }

let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
 fun st ->
  let v, st = e st in
  f v st

let return x st = x, st

let var x st =
  try Var.Map.find x st.vars, st
  with Not_found ->
    Format.eprintf "%a@." Var.print x;
    assert false

let add_var x ({ var_count; vars; _ } as st) =
  let i = var_count in
  let vars = Var.Map.add x (Local i) vars in
  i, { st with var_count = var_count + 1; vars }

let instr i : unit t = fun st -> (), { st with instrs = i :: st.instrs }

let blk l st =
  let instrs = st.instrs in
  let (), st = l { st with instrs = [] } in
  List.rev st.instrs, { st with instrs }

module Arith = struct
  let binary op e e' =
    let* e = e in
    let* e' = e' in
    return (W.BinOp (I32 op, e, e'))

  let ( + ) = binary Add

  let ( - ) = binary Sub

  let ( lsl ) = binary Shl

  let ( lsr ) = binary (Shr U)

  let ( asr ) = binary (Shr S)

  let ( land ) = binary And

  let ( < ) = binary (Lt S)

  let ( <= ) = binary (Le S)

  let ( = ) = binary Eq

  let ( <> ) = binary Ne

  let const n = return (W.Const (I32 n))

  let val_int i = (i lsl const 1l) + const 1l
end

module Memory = struct
  let load ?(offset = 0) e =
    let* e = e in
    return (W.Load (I32 (Int32.of_int offset), e))

  let store ?(offset = 0) e e' =
    let* e = e in
    let* e' = e' in
    instr (Store (I32 (Int32.of_int offset), e, e'))

  let allocate ~tag:_ _a _array_or_not = return (W.Const (I32 0l)) (*ZZZ Float array?*)

  let tag e = load ~offset:(-4) e (*ZZZ mask*)

  let array_get e e' = load ~offset:(-2) Arith.(e + (e' lsl const 2l))

  let array_set e e' e'' = store ~offset:(-2) Arith.(e + (e' lsl const 2l)) e''

  let field e idx = load ~offset:(4 * idx) e

  let set_field e idx e' = store ~offset:(4 * idx) e e'
end

let load x =
  let* x = var x in
  match x with
  | Local x -> return (W.LocalGet x)
  | Env (x, offset) -> Memory.field (return (W.LocalGet x)) offset
  | Rec (x, offset) ->
      let closure = return (W.LocalGet x) in
      if offset = 0 then closure else Arith.(closure + const (Int32.of_int (offset * 8)))

let assign x e =
  let* x = var x in
  let* e = e in
  match x with
  | Local x -> instr (W.LocalSet (x, e))
  | Env _ | Rec _ -> assert false

let drop e =
  let* e = e in
  instr (Drop e)

let loop l =
  let* instrs = blk l in
  instr (Loop instrs)

let block l =
  let* instrs = blk l in
  instr (Block instrs)

let if_ e l1 l2 =
  let* e = e in
  let* instrs1 = blk l1 in
  let* instrs2 = blk l2 in
  instr (If (e, instrs1, instrs2))

let store x e =
  let* i = add_var x in
  let* e = e in
  instr (LocalSet (i, e))

let transl_constant c =
  match c with
  | Int i -> Arith.const Int32.(add (add i i) 1l)
  | _ -> Arith.const 5555l
(*ZZZ *)

let transl_prim_arg x =
  match x with
  | Pv x -> load x
  | Pc c -> transl_constant c

let rec translate_expr ctx e =
  match e with
  | Apply { f; args; exact = _ } ->
      (*ZZZ*)
      let rec loop acc l =
        match l with
        | [] ->
            let* closure = load f in
            let* funct = Memory.field (load f) 0 in
            return
              (W.Call_indirect
                 (func_type (List.length args + 1), funct, List.rev (closure :: acc)))
        | x :: r ->
            let* x = load x in
            loop (x :: acc) r
      in
      loop [] args
  | Block (tag, a, array_or_not) -> Memory.allocate ~tag a array_or_not
  | Field (x, n) -> Memory.field (load x) n
  | Closure (_args, ((_pc, _) as _cont)) -> return (W.Const (I32 0l)) (*ZZZ*)
  | Constant c -> transl_constant c
  | Prim (IsInt, [ x ]) -> Arith.(transl_prim_arg x land const 1l)
  | Prim (Extern "%int_add", [ x; y ]) ->
      Arith.(transl_prim_arg x + transl_prim_arg y - const 1l)
  | Prim (Extern "%int_lsl", [ x; y ]) ->
      Arith.(
        ((transl_prim_arg x - const 1l) lsl transl_prim_arg y asr const 1l) + const 1l)
  | Prim (Extern nm, l) ->
      (*ZZZ Different calling convention when large number of parameters *)
      if not (StringMap.mem nm ctx.primitives)
      then ctx.primitives <- StringMap.add nm (func_type (List.length l)) ctx.primitives;
      let rec loop acc l =
        match l with
        | [] -> return (W.Call (nm, List.rev acc))
        | x :: r ->
            let* x = transl_prim_arg x in
            loop (x :: acc) r
      in
      loop [] l
  | Prim (p, l) -> (
      match p, l with
      | Not, [ x ] -> Arith.(const 4l - transl_prim_arg x)
      | Lt, [ x; y ] -> Arith.(val_int (transl_prim_arg x < transl_prim_arg y))
      | Le, [ x; y ] -> Arith.(val_int (transl_prim_arg x <= transl_prim_arg y))
      | Eq, [ x; y ] -> Arith.(val_int (transl_prim_arg x = transl_prim_arg y))
      | Neq, [ x; y ] -> Arith.(val_int (transl_prim_arg x <> transl_prim_arg y))
      | Ult, [ x; y ] ->
          Arith.(val_int (binary (Lt U) (transl_prim_arg x) (transl_prim_arg y)))
      | _ -> Arith.const 7777l (*ZZZ *))

and translate_instr ctx i =
  match i with
  | Assign (x, y) -> assign x (load y)
  | Let (x, e) ->
      if ctx.live.(Var.idx x) = 0
      then drop (translate_expr ctx e)
      else store x (translate_expr ctx e)
  | Set_field (x, n, y) -> Memory.set_field (load x) n (load y)
  | Offset_ref (x, n) ->
      Memory.set_field
        (load x)
        0
        Arith.(Memory.field (load x) 0 + const (Int32.of_int (2 * n)))
  | Array_set (x, y, z) -> Memory.array_set (load x) (load y) (load z)

and translate_instrs ctx l =
  match l with
  | [] -> return ()
  | i :: rem ->
      let* () = translate_instr ctx i in
      translate_instrs ctx rem

let parallel_renaming params args =
  let rec visit visited prev s m x l =
    if not (Var.Set.mem x visited)
    then
      let visited = Var.Set.add x visited in
      let y = Var.Map.find x m in
      if Code.Var.compare x y = 0
      then visited, None, l
      else if Var.Set.mem y prev
      then
        let t = Code.Var.fresh () in
        visited, Some (y, t), (x, t) :: l
      else if Var.Set.mem y s
      then
        let visited, aliases, l = visit visited (Var.Set.add x prev) s m y l in
        match aliases with
        | Some (a, b) when Code.Var.compare a x = 0 ->
            visited, None, (b, a) :: (x, y) :: l
        | _ -> visited, aliases, (x, y) :: l
      else visited, None, (x, y) :: l
    else visited, None, l
  in
  let visit_all params args =
    let m = Subst.build_mapping params args in
    let s = List.fold_left params ~init:Var.Set.empty ~f:(fun s x -> Var.Set.add x s) in
    let _, l =
      Var.Set.fold
        (fun x (visited, l) ->
          let visited, _, l = visit visited Var.Set.empty s m x l in
          visited, l)
        s
        (Var.Set.empty, [])
    in
    l
  in
  let l = List.rev (visit_all params args) in
  List.fold_left
    l
    ~f:(fun continuation (y, x) ->
      let* () = continuation in
      store y (load x))
    ~init:(return ())

let translate_closure ctx name_opt params ((pc, _) as cont) acc =
  let g = Wa_structure.build_graph ctx.blocks pc in
  let idom = Wa_structure.dominator_tree g in
  let dom = Wa_structure.reverse_tree idom in
  let rec index pc i context =
    match context with
    | (`Loop pc' | `Block pc') :: _ when pc = pc' -> i
    | _ :: rem -> index pc (i + 1) rem
    | [] -> assert false
  in
  let rec translate_tree pc context =
    let is_switch =
      let block = Addr.Map.find pc ctx.blocks in
      match block.branch with
      | Switch _ -> true
      | _ -> false
    in
    let code =
      translate_node_within
        pc
        (List.filter
           ~f:(fun pc' -> is_switch || Wa_structure.is_merge_node g pc')
           (List.rev (Addr.Set.elements (Wa_structure.get_edges dom pc))))
    in
    if Wa_structure.is_loop_header g pc
    then loop (code (`Loop pc :: context))
    else code context
  and translate_node_within pc l context =
    match l with
    | pc' :: rem ->
        let* () = block (translate_node_within pc rem (`Block pc' :: context)) in
        translate_tree pc' context
    | [] -> (
        let block = Addr.Map.find pc ctx.blocks in
        let* () = translate_instrs ctx block.body in
        match block.branch with
        | Branch cont -> translate_branch pc cont context
        | Return x ->
            let* e = load x in
            instr (Return (Some e))
        | Cond (x, cont1, cont2) ->
            if_
              (load x)
              (translate_branch pc cont1 (`If :: context))
              (translate_branch pc cont2 (`If :: context))
        | Stop -> instr (Return None)
        | Switch (x, a1, a2) -> (
            let br_table e a context =
              let len = Array.length a in
              let l = Array.to_list (Array.sub a ~pos:0 ~len:(len - 1)) in
              let dest (pc, args) =
                assert (List.is_empty args);
                index pc 0 context
              in
              let* e = e in
              instr (Br_table (e, List.map ~f:dest l, dest a.(len - 1)))
            in
            match a1, a2 with
            | [||], _ -> br_table (Memory.tag (load x)) a2 context
            | _, [||] -> br_table (load x) a1 context
            | _ ->
                if_
                  Arith.(load x land const 1l)
                  (br_table (load x) a1 context)
                  (br_table (Memory.tag (load x)) a2 context))
        | Raise (x, _) ->
            let* e = load x in
            instr (Return (Some e))
            (*ZZZ*)
        | Pushtrap (cont, x, cont', _) ->
            if_
              (Arith.const 0l)
              (let* () = store x (Arith.const 0l) in
               translate_branch pc cont' (`If :: context))
              (translate_branch pc cont (`If :: context))
            (*ZZZ*)
        | Poptrap cont -> translate_branch pc cont context (*ZZZ*))
  and translate_branch src (dst, args) context =
    let* () =
      if List.is_empty args
      then return ()
      else
        let block = Addr.Map.find dst ctx.blocks in
        parallel_renaming block.params args
    in
    if (src >= 0 && Wa_structure.is_backward g src dst)
       || Wa_structure.is_merge_node g dst
    then instr (Br (index dst 0 context))
    else translate_tree dst context
  in
  let initial_env =
    match name_opt with
    | Some f ->
        let { Wa_closure_conversion.functions; free_variables } =
          Var.Map.find f ctx.closures
        in
        let index =
          let rec index i l =
            match l with
            | [] -> assert false
            | g :: r -> if Var.equal f g then i else index (i + 1) r
          in
          index 0 functions
        in
        let env = 0 in
        let _, vars =
          List.fold_left
            ~f:(fun (i, vars) x -> i + 1, Var.Map.add x (Rec (env, i)) vars)
            ~init:(-index, Var.Map.empty)
            functions
        in
        let _, vars =
          let offset = (2 * (List.length functions - index)) - 1 in
          List.fold_left
            ~f:(fun (i, vars) x -> i + 1, Var.Map.add x (Env (env, i)) vars)
            ~init:(offset, vars)
            free_variables
        in
        { var_count = 1; vars; instrs = [] }
    | None -> { var_count = 1; vars = Var.Map.empty; instrs = [] }
  in
  let _, env =
    List.fold_left
      ~f:(fun l x ->
        let* _ = add_var x in
        let* _ = l in
        return ())
      ~init:(return ())
      params
      initial_env
  in
  (*
  Format.eprintf "=== %d ===@." pc;
*)
  let _, st = translate_branch (-1) cont [] env in
  W.Function
    { name =
        (match name_opt with
        | None -> Var.fresh ()
        | Some x -> x)
    ; typ = func_type (List.length params + 1)
    ; locals = List.init ~len:st.var_count ~f:(fun _ : W.value_type -> I32)
    ; body = List.rev st.instrs
    }
  :: acc

let f
    (p : Code.program)
    ~live_vars
     (*
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
      _debug *) =
  let closures = Wa_closure_conversion.f p in
  let ctx =
    { live = live_vars; blocks = p.blocks; closures; primitives = StringMap.empty }
  in
  let functions =
    Code.fold_closures
      p
      (fun name_opt params cont -> translate_closure ctx name_opt params cont)
      []
  in
  let primitives =
    List.map
      ~f:(fun (name, ty) -> W.Import { name; desc = Fun ty })
      (StringMap.bindings ctx.primitives)
  in
  Wa_output.f (functions @ primitives)

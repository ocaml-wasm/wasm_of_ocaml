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
  ; mutable other_fields : W.module_field list
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
    Format.eprintf "ZZZ %a@." Var.print x;
    Local 0, st

let add_var x ({ var_count; vars; _ } as st) =
  match Var.Map.find_opt x vars with
  | Some (Local i) -> i, st
  | Some (Rec _ | Env _) -> assert false
  | None ->
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

  let ( lsl ) = binary Shl

  let ( lsr ) = binary (Shr U)

  let ( asr ) = binary (Shr S)

  let ( land ) = binary And

  let ( lor ) = binary Or

  let ( lxor ) = binary Xor

  let ( < ) = binary (Lt S)

  let ( <= ) = binary (Le S)

  let ( = ) = binary Eq

  let ( <> ) = binary Ne

  let const n = return (W.Const (I32 n))

  let val_int i = (i lsl const 1l) + const 1l

  let int_val i = i asr const 1l
end

let seq l e =
  let* instrs = blk l in
  let* e = e in
  return (W.Seq (instrs, e))

let load x =
  let* x = var x in
  match x with
  | Local x -> return (W.LocalGet x)
  | Env (x, offset) -> return (W.Load (I32 (Int32.of_int (4 * offset)), W.LocalGet x))
  | Rec (x, offset) ->
      let closure = return (W.LocalGet x) in
      let offset = 4 * offset in
      Arith.(closure + const (Int32.of_int offset))

let tee x e =
  let* i = add_var x in
  let* e = e in
  return (W.LocalTee (i, e))

module Memory = struct
  let mem_load ?(offset = 0) e =
    assert (offset >= 0);
    let* e = e in
    return (W.Load (I32 (Int32.of_int offset), e))

  let mem_store ?(offset = 0) e e' =
    assert (offset >= 0);
    let* e = e in
    let* e' = e' in
    instr (Store (I32 (Int32.of_int offset), e, e'))

  (*
p = young_ptr - size;
if (p < young_limit) {caml_call_gc(); p = young_ptr - size}
...
return p + 4
*)
  let header ?(const = false) ~tag ~len () =
    Int32.of_int ((len lsl 10) + tag + if const then 3 * 256 else 0)

  let allocate ~tag l =
    let len = List.length l in
    let p = Var.fresh_n "p" in
    let size = (len + 1) * 4 in
    seq
      (let* v =
         tee p Arith.(return (W.GlobalGet "young_ptr") - const (Int32.of_int size))
       in
       let* () = instr (W.GlobalSet ("young_ptr", v)) in
       let* () = mem_store (load p) (Arith.const (header ~tag ~len ())) in
       snd
         (List.fold_right
            ~init:(len, return ())
            ~f:(fun v (i, cont) ->
              ( i - 1
              , let* () =
                  mem_store
                    ~offset:(4 * i)
                    (load p)
                    (match v with
                    | `Var y -> load y
                    | `Expr e -> return e)
                in
                cont ))
            l))
      Arith.(load p + const 4l)
  (*ZZZ Float array?*)

  let tag e = Arith.(mem_load (e - const 4l) land const 0xffl)

  let array_get e e' = mem_load Arith.(e + ((e' - const 1l) lsl const 1l))

  let array_set e e' e'' = mem_store Arith.(e + ((e' - const 1l) lsl const 1l)) e''

  let field e idx = mem_load ~offset:(4 * idx) e

  let set_field e idx e' = mem_store ~offset:(4 * idx) e e'
end

let assign x e =
  let* x = var x in
  let* e = e in
  match x with
  | Local x -> instr (W.LocalSet (x, e))
  | Env _ | Rec _ -> assert false

let drop e =
  let* e = e in
  instr (Drop e)

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
  instr (If (ty, e, instrs1, instrs2))

let store x e =
  let* i = add_var x in
  let* e = e in
  instr (LocalSet (i, e))

let rec transl_constant_rec ctx c =
  match c with
  | Int i -> W.DataI32 Int32.(add (add i i) 1l)
  | Tuple (tag, a, _) ->
      let h = Memory.header ~const:true ~tag ~len:(Array.length a) () in
      let name = Var.fresh_n "block" in
      let block =
        W.Data
          { name
          ; read_only = true
          ; contents =
              DataI32 h
              :: List.map ~f:(fun c -> transl_constant_rec ctx c) (Array.to_list a)
          }
      in
      ctx.other_fields <- block :: ctx.other_fields;
      W.DataSym (name, 4)
  | NativeString (Byte s | Utf (Utf8 s)) | String s ->
      let l = String.length s in
      let len = (l + 4) / 4 in
      let h = Memory.header ~const:true ~tag:252 ~len () in
      let name = Var.fresh_n "str" in
      let extra = (4 * len) - l - 1 in
      ctx.other_fields <-
        Data
          { name
          ; read_only = true
          ; contents =
              DataI32 h
              :: DataBytes s
              :: (if extra = 0 then [ DataI8 0 ] else [ DataSpace extra; DataI8 extra ])
          }
        :: ctx.other_fields;
      W.DataSym (name, 4)
  | Float _ ->
      prerr_endline "FLOAT";
      W.DataI32 11115555l
  | Float_array _ ->
      prerr_endline "FLOAT ARRAY";
      W.DataI32 11115555l
  | Int64 _ ->
      prerr_endline "FLOAT ARRAY";
      W.DataI32 11115555l

let transl_constant ctx c =
  return
    (match transl_constant_rec ctx c with
    | W.DataSym (name, offset) -> W.ConstSym (name, offset)
    | W.DataI32 i -> W.Const (I32 i)
    | _ -> assert false)

let transl_prim_arg ctx x =
  match x with
  | Pv x -> load x
  | Pc c -> transl_constant ctx c

let function_offset_in_closure info f =
  let rec index i l =
    match l with
    | [] -> assert false
    | (g, arity) :: r ->
        if Var.equal f g then i else index (i + if arity > 1 then 4 else 3) r
  in
  index 0 info.Wa_closure_conversion.functions

let closure_start_env info =
  List.fold_left
    ~f:(fun i (_, arity) -> i + if arity > 1 then 4 else 3)
    ~init:(-1)
    info.Wa_closure_conversion.functions

let rec translate_expr ctx x e =
  match e with
  | Apply { f; args; exact = _ } ->
      (*ZZZ*)
      let rec loop acc l =
        match l with
        | [] ->
            let len = List.length args in
            let funct = Var.fresh () in
            let* closure = tee funct (load f) in
            let* funct = Memory.field (load funct) (if len = 1 then 0 else 2) in
            return
              (W.Call_indirect (func_type (len + 1), funct, List.rev (closure :: acc)))
        | x :: r ->
            let* x = load x in
            loop (x :: acc) r
      in
      loop [] args
  | Block (tag, a, _) ->
      Memory.allocate ~tag (List.map ~f:(fun x -> `Var x) (Array.to_list a))
  | Field (x, n) -> Memory.field (load x) n
  | Closure (_args, ((_pc, _) as _cont)) ->
      let info = Var.Map.find x ctx.closures in
      let f, _ = List.hd info.functions in
      if Var.equal x f
      then
        let start_env = closure_start_env info in
        let _, start =
          List.fold_left
            ~f:(fun (i, start) (f, arity) ->
              let start =
                if i = 0
                then start
                else W.Const (I32 (Memory.header ~tag:249 ~len:i ())) :: start
              in
              let clos_info =
                (*ZZZ arity might overflow *)
                Int32.of_int ((arity lsl 24) + ((start_env - i) lsl 1) + 1)
              in
              let start = W.Const (I32 clos_info) :: W.ConstSym (f, 0) :: start in
              if arity > 1 then i + 4, W.ConstSym (f, 0) :: start else i + 3, start)
            ~init:(0, [])
            info.functions
        in
        if List.is_empty info.free_variables
        then (
          let l =
            List.rev_map
              ~f:(fun e ->
                match e with
                | W.Const (I32 i) -> W.DataI32 i
                | ConstSym (sym, offset) -> DataSym (sym, offset)
                | _ -> assert false)
              start
          in
          let h = Memory.header ~const:true ~tag:247 ~len:(List.length l) () in
          let name = Var.fork x in
          let closure = W.Data { name; read_only = true; contents = DataI32 h :: l } in
          ctx.other_fields <- closure :: ctx.other_fields;
          return (W.ConstSym (name, 4)))
        else
          Memory.allocate
            ~tag:247
            (List.rev_map ~f:(fun e -> `Expr e) start
            @ List.map ~f:(fun x -> `Var x) info.free_variables)
      else
        let offset = Int32.of_int (4 * function_offset_in_closure info x) in
        Arith.(load f + const offset)
  | Constant c -> transl_constant ctx c
  | Prim (Extern "caml_array_unsafe_get", [ x; y ]) ->
      Memory.array_get (transl_prim_arg ctx x) (transl_prim_arg ctx y)
  | Prim (IsInt, [ x ]) -> Arith.(transl_prim_arg ctx x land const 1l)
  | Prim (Extern "%int_add", [ x; y ]) ->
      Arith.(transl_prim_arg ctx x + transl_prim_arg ctx y - const 1l)
  | Prim (Extern "%int_sub", [ x; y ]) ->
      Arith.(transl_prim_arg ctx x - transl_prim_arg ctx y + const 1l)
  | Prim (Extern "%int_mul", [ x; y ]) ->
      Arith.(val_int (int_val (transl_prim_arg ctx x) * int_val (transl_prim_arg ctx y)))
  | Prim (Extern "%int_neg", [ x ]) -> Arith.(const 2l - transl_prim_arg ctx x)
  | Prim (Extern "%int_or", [ x; y ]) ->
      Arith.(transl_prim_arg ctx x lor transl_prim_arg ctx y)
  | Prim (Extern "%int_and", [ x; y ]) ->
      Arith.(transl_prim_arg ctx x land transl_prim_arg ctx y)
  | Prim (Extern "%int_xor", [ x; y ]) ->
      Arith.(transl_prim_arg ctx x lxor transl_prim_arg ctx y lor const 1l)
  | Prim (Extern "%int_lsl", [ x; y ]) ->
      Arith.(
        ((transl_prim_arg ctx x - const 1l) lsl int_val (transl_prim_arg ctx y))
        + const 1l)
  | Prim (Extern "%int_lsr", [ x; y ]) ->
      Arith.((transl_prim_arg ctx x lsr int_val (transl_prim_arg ctx y)) lor const 1l)
  | Prim (Extern "%int_asr", [ x; y ]) ->
      Arith.((transl_prim_arg ctx x asr int_val (transl_prim_arg ctx y)) lor const 1l)
  | Prim (Extern nm, l) ->
      (*ZZZ Different calling convention when large number of parameters *)
      if not (StringMap.mem nm ctx.primitives)
      then ctx.primitives <- StringMap.add nm (func_type (List.length l)) ctx.primitives;
      let rec loop acc l =
        match l with
        | [] -> return (W.Call (nm, List.rev acc))
        | x :: r ->
            let* x = transl_prim_arg ctx x in
            loop (x :: acc) r
      in
      loop [] l
  | Prim (p, l) -> (
      match p, l with
      | Not, [ x ] -> Arith.(const 4l - transl_prim_arg ctx x)
      | Lt, [ x; y ] -> Arith.(val_int (transl_prim_arg ctx x < transl_prim_arg ctx y))
      | Le, [ x; y ] -> Arith.(val_int (transl_prim_arg ctx x <= transl_prim_arg ctx y))
      | Eq, [ x; y ] -> Arith.(val_int (transl_prim_arg ctx x = transl_prim_arg ctx y))
      | Neq, [ x; y ] -> Arith.(val_int (transl_prim_arg ctx x <> transl_prim_arg ctx y))
      | Ult, [ x; y ] ->
          Arith.(val_int (binary (Lt U) (transl_prim_arg ctx x) (transl_prim_arg ctx y)))
      | _ -> Arith.const 11119999l (*ZZZ *))

and translate_instr ctx i =
  match i with
  | Assign (x, y) -> assign x (load y)
  | Let (x, e) ->
      if ctx.live.(Var.idx x) = 0
      then drop (translate_expr ctx x e)
      else store x (translate_expr ctx x e)
  | Set_field (x, n, y) -> Memory.set_field (load x) n (load y)
  | Offset_ref (x, n) ->
      let n' = 2 * n in
      Memory.set_field
        (load x)
        0
        Arith.(Memory.field (load x) 0 + const (Int32.of_int n'))
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
  let rec translate_tree typ pc context =
    let is_switch =
      let block = Addr.Map.find pc ctx.blocks in
      match block.branch with
      | Switch _ -> true
      | _ -> false
    in
    let code =
      translate_node_within
        typ
        pc
        (List.filter
           ~f:(fun pc' -> is_switch || Wa_structure.is_merge_node g pc')
           (List.rev (Addr.Set.elements (Wa_structure.get_edges dom pc))))
    in
    if Wa_structure.is_loop_header g pc
    then loop typ (code (`Loop pc :: context))
    else code context
  and translate_node_within typ pc l context =
    match l with
    | pc' :: rem ->
        let* () =
          block None (translate_node_within None pc rem (`Block pc' :: context))
        in
        translate_tree typ pc' context
    | [] -> (
        let block = Addr.Map.find pc ctx.blocks in
        let* () = translate_instrs ctx block.body in
        match block.branch with
        | Branch cont -> translate_branch typ pc cont context
        | Return x -> (
            let* e = load x in
            match context with
            | `Then :: _ ->
                (* Return is miscompiled here...
                   See: https://github.com/llvm/llvm-project/issues/56935 *)
                instr (Br (List.length context, Some e))
            | _ -> instr (Return (Some e)))
        | Cond (x, cont1, cont2) ->
            if_
              typ
              Arith.(load x <> const 1l)
              (translate_branch typ pc cont1 (`Then :: context))
              (translate_branch typ pc cont2 (`Else :: context))
        | Stop ->
            let* e = Arith.const 0l in
            instr (Return (Some e))
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
                  typ
                  Arith.(load x land const 1l)
                  (br_table (load x) a1 context)
                  (br_table (Memory.tag (load x)) a2 context))
        | Raise (x, _) ->
            let* e = load x in
            instr (Br (List.length context, Some e))
            (*ZZZ*)
        | Pushtrap (cont, x, cont', _) ->
            if_
              typ
              (Arith.const 0l)
              (let* () = store x (Arith.const 0l) in
               translate_branch typ pc cont' (`Then :: context))
              (translate_branch typ pc cont (`Else :: context))
            (*ZZZ*)
        | Poptrap cont -> translate_branch typ pc cont context (*ZZZ*))
  and translate_branch typ src (dst, args) context =
    let* () =
      if List.is_empty args
      then return ()
      else
        let block = Addr.Map.find dst ctx.blocks in
        parallel_renaming block.params args
    in
    if (src >= 0 && Wa_structure.is_backward g src dst)
       || Wa_structure.is_merge_node g dst
    then instr (Br (index dst 0 context, None))
    else translate_tree typ dst context
  in
  let initial_env =
    match name_opt with
    | Some f ->
        let ({ Wa_closure_conversion.functions; free_variables } as info) =
          Var.Map.find f ctx.closures
        in
        let funct_index = function_offset_in_closure info f in
        let env = 0 in
        let _, vars =
          List.fold_left
            ~f:(fun (i, vars) (x, arity) ->
              (i + if arity > 1 then 4 else 3), Var.Map.add x (Rec (env, i)) vars)
            ~init:(-funct_index, Var.Map.empty)
            functions
        in
        let start_env = closure_start_env info in
        let _, vars =
          let offset = start_env - funct_index in
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
        let* _ = l in
        let* _ = add_var x in
        return ())
      ~init:(return ())
      params
      initial_env
  in
  (*
  Format.eprintf "=== %d ===@." pc;
*)
  let _, st = translate_branch (Some (I32 : W.value_type)) (-1) cont [] env in

  let param_count = List.length params + 1 in
  let local_count, body =
    if true
    then st.var_count, List.rev st.instrs
    else Wa_minimize_locals.f ~param_count ~local_count:st.var_count (List.rev st.instrs)
  in
  W.Function
    { name =
        (match name_opt with
        | None -> Var.fresh ()
        | Some x -> x)
    ; typ = func_type param_count
    ; locals = List.init ~len:(local_count - param_count) ~f:(fun _ : W.value_type -> I32)
    ; body
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
    { live = live_vars
    ; blocks = p.blocks
    ; closures
    ; primitives = StringMap.empty
    ; other_fields = []
    }
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
  Wa_output.f
    (W.Import { name = "young_ptr"; desc = Global I32 }
    :: (primitives @ functions @ ctx.other_fields))

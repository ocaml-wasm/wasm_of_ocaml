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

type ctx =
  { live : int array
  ; blocks : block Addr.Map.t
  ; constants : (Code.Var.t, W.expression) Hashtbl.t
  ; closures : Wa_closure_conversion.closure Var.Map.t
  ; mutable primitives : W.func_type StringMap.t
  ; constant_data : W.data list Var.Map.t ref
  }

type var =
  | Local of int
  | Env of Code.Var.t * int
  | Rec of Code.Var.t * int
  | Const of W.expression

type state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : W.instruction list
  ; constants : (Code.Var.t, W.expression) Hashtbl.t
  ; constant_data : W.data list Var.Map.t ref
  }

let empty_env constants constant_data =
  { var_count = 0; vars = Var.Map.empty; instrs = []; constants; constant_data }

type 'a t = state -> 'a * state

let func_type n =
  { W.params = List.init ~len:n ~f:(fun _ : W.value_type -> I32); result = [ I32 ] }

let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
 fun st ->
  let v, st = e st in
  f v st

let return x st = x, st

let register_constant x e st =
  Hashtbl.add st.constants x e;
  (), st

let var x st =
  try Var.Map.find x st.vars, st
  with Not_found -> (
    try Const (Hashtbl.find st.constants x), st
    with Not_found ->
      Format.eprintf "ZZZ %a@." Var.print x;
      Local 0, st)

let add_var x ({ var_count; vars; _ } as st) =
  match Var.Map.find_opt x vars with
  | Some (Local i) -> i, st
  | Some (Rec _ | Env _ | Const _) -> assert false
  | None ->
      let i = var_count in
      let vars = Var.Map.add x (Local i) vars in
      i, { st with var_count = var_count + 1; vars }

let instr i : unit t = fun st -> (), { st with instrs = i :: st.instrs }

let instrs l : unit t = fun st -> (), { st with instrs = List.rev_append l st.instrs }

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

  let const n = return (W.Const (I32 n))

  let val_int i = (i lsl const 1l) + const 1l

  let int_val i = i asr const 1l
end

let seq l e =
  let* instrs = blk l in
  let* e = e in
  return (W.Seq (instrs, e))

let rec load x =
  let* x = var x in
  match x with
  | Local x -> return (W.LocalGet x)
  | Env (x, offset) ->
      let* x = load x in
      return (W.Load (I32 (Int32.of_int (4 * offset)), x))
  | Rec (x, offset) ->
      let offset = 4 * offset in
      Arith.(load x + const (Int32.of_int offset))
  | Const e -> return e

let tee x e =
  let* e = e in
  match e with
  | W.ConstSym _ ->
      let* () = register_constant x e in
      return e
  | _ ->
      let* i = add_var x in
      return (W.LocalTee (i, e))

let store ?(always = false) x e =
  let* e = e in
  match e with
  | (W.ConstSym _ | W.Const _) when not always -> register_constant x e
  | _ ->
      let* i = add_var x in
      instr (LocalSet (i, e))

module Memory = struct
  let mem_load ?(offset = 0) e =
    assert (offset >= 0);
    let* e = e in
    match e with
    | W.ConstSym (V x, offset') ->
        fun st ->
          let rec get_data offset l =
            match l with
            | [] -> assert false
            | W.DataI32 i :: _ when offset = 0 -> W.Const (I32 i)
            | W.DataSym (sym, ofs) :: _ when offset = 0 -> W.ConstSym (sym, ofs)
            | (W.DataI32 _ | DataSym _) :: r -> get_data (offset - 4) r
            | (DataI8 _ | DataBytes _ | DataSpace _ | DataI64 _) :: _ -> assert false
          in
          let data = get_data (offset + offset') (Var.Map.find x !(st.constant_data)) in
          data, st
    | _ -> return (W.Load (I32 (Int32.of_int offset), e))

  let mem_store ?(offset = 0) e e' =
    assert (offset >= 0);
    let* e = e in
    let* e' = e' in
    instr (Store (I32 (Int32.of_int offset), e, e'))

  (*ZZZ
    p = young_ptr - size;
    if (p < young_limit) {caml_call_gc(); p = young_ptr - size}
    ...
    return p + 4
  *)
  let header ?(const = false) ~tag ~len () =
    Int32.(add (shift_left (of_int len) 10) (of_int (tag + if const then 3 * 256 else 0)))

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

  (*
  let length e = Arith.(mem_load (e - const 4l) lsr const 10l)
*)

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
  | Env _ | Rec _ | Const _ -> assert false

let drop e =
  let* e = e in
  match e with
  | W.Seq (l, Const (I32 1l)) -> instrs l
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
  instr (If (ty, e, instrs1, instrs2))

let rec transl_constant_rec constant_data c =
  match c with
  | Int i -> W.DataI32 Int32.(add (add i i) 1l)
  | Tuple (tag, a, _) ->
      let h = Memory.header ~const:true ~tag ~len:(Array.length a) () in
      let name = Var.fresh_n "block" in
      let block =
        W.DataI32 h
        :: List.map ~f:(fun c -> transl_constant_rec constant_data c) (Array.to_list a)
      in
      constant_data := Var.Map.add name block !constant_data;
      W.DataSym (V name, 4)
  | NativeString (Byte s | Utf (Utf8 s)) | String s ->
      let l = String.length s in
      let len = (l + 4) / 4 in
      let h = Memory.header ~const:true ~tag:Obj.string_tag ~len () in
      let name = Var.fresh_n "str" in
      let extra = (4 * len) - l - 1 in
      let string =
        W.DataI32 h
        :: DataBytes s
        :: (if extra = 0 then [ DataI8 0 ] else [ DataSpace extra; DataI8 extra ])
      in
      constant_data := Var.Map.add name string !constant_data;
      W.DataSym (V name, 4)
  | Float f ->
      let h = Memory.header ~const:true ~tag:Obj.double_tag ~len:2 () in
      let name = Var.fresh_n "float" in
      let block = [ W.DataI32 h; DataI64 (Int64.bits_of_float f) ] in
      constant_data := Var.Map.add name block !constant_data;
      W.DataSym (V name, 4)
  | Float_array l ->
      (*ZZZ Boxed array? *)
      let l = Array.to_list l in
      let h =
        Memory.header ~const:true ~tag:Obj.double_array_tag ~len:(List.length l) ()
      in
      let name = Var.fresh_n "float_array" in
      let block =
        W.DataI32 h
        :: List.map ~f:(fun f -> transl_constant_rec constant_data (Float f)) l
      in
      constant_data := Var.Map.add name block !constant_data;
      W.DataSym (V name, 4)
  | Int64 i ->
      let h = Memory.header ~const:true ~tag:Obj.custom_tag ~len:3 () in
      let name = Var.fresh_n "int64" in
      let block = [ W.DataI32 h; DataSym (S "caml_int64_ops", 0); DataI64 i ] in
      constant_data := Var.Map.add name block !constant_data;
      W.DataSym (V name, 4)

let transl_constant c st =
  ( (match transl_constant_rec st.constant_data c with
    | W.DataSym (V name, offset) -> W.ConstSym (V name, offset)
    | W.DataI32 i -> W.Const (I32 i)
    | _ -> assert false)
  , st )

let transl_prim_arg x =
  match x with
  | Pv x -> load x
  | Pc c -> transl_constant c

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

let closure_stats =
  let s = ref 0 in
  let n = ref 0 in
  fun (ctx : ctx) info ->
    let free_variables =
      List.filter
        ~f:(fun x -> not (Hashtbl.mem ctx.constants x))
        info.Wa_closure_conversion.free_variables
    in
    if true && not (List.is_empty free_variables)
    then
      (incr n;
       s := !s + List.length free_variables;
       Format.eprintf "OOO %d %f %s@." (List.length free_variables) (float !s /. float !n))
        (Var.to_string (fst (List.hd info.functions)))

let register_primitive ctx nm typ =
  (*ZZZ check type*)
  if not (StringMap.mem nm ctx.primitives)
  then ctx.primitives <- StringMap.add nm typ ctx.primitives

let rec translate_expr ctx x e =
  match e with
  | Apply { f; args; exact = _ } ->
      (*ZZZ*)
      let rec loop acc l =
        match l with
        | [] -> (
            let len = List.length args in
            let funct = Var.fresh () in
            let* closure = tee funct (load f) in
            let* funct = Memory.field (load funct) (if len = 1 then 0 else 2) in
            match funct with
            | W.ConstSym (g, 0) -> return (W.Call (g, List.rev (closure :: acc)))
            | _ ->
                return
                  (W.Call_indirect (func_type (len + 1), funct, List.rev (closure :: acc)))
            )
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
      then (
        let start_env = closure_start_env info in
        let _, start =
          List.fold_left
            ~f:(fun (i, start) (f, arity) ->
              let start =
                if i = 0
                then start
                else W.Const (I32 (Memory.header ~tag:Obj.infix_tag ~len:i ())) :: start
              in
              let clos_info =
                Int32.(
                  add
                    (shift_left (of_int arity) 24)
                    (of_int (((start_env - i) lsl 1) + 1)))
              in
              let start = W.Const (I32 clos_info) :: W.ConstSym (V f, 0) :: start in
              if arity > 1 then i + 4, W.ConstSym (V f, 0) :: start else i + 3, start)
            ~init:(0, [])
            info.functions
        in
        closure_stats ctx info;
        let free_variables =
          List.filter ~f:(fun x -> not (Hashtbl.mem ctx.constants x)) info.free_variables
        in
        if List.is_empty free_variables
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
          let h =
            Memory.header ~const:true ~tag:Obj.closure_tag ~len:(List.length l) ()
          in
          let name = Var.fresh_n "closure" in
          ctx.constant_data := Var.Map.add name (W.DataI32 h :: l) !(ctx.constant_data);
          return (W.ConstSym (V name, 4)))
        else
          Memory.allocate
            ~tag:Obj.closure_tag
            (List.rev_map ~f:(fun e -> `Expr e) start
            @ List.map ~f:(fun x -> `Var x) free_variables))
      else
        let offset = Int32.of_int (4 * function_offset_in_closure info x) in
        Arith.(load f + const offset)
  | Constant c -> transl_constant c
  | Prim (Extern "caml_array_unsafe_get", [ x; y ]) ->
      Memory.array_get (transl_prim_arg x) (transl_prim_arg y)
  | Prim (Extern "caml_array_unsafe_set", [ x; y; z ]) ->
      seq
        (Memory.array_set (transl_prim_arg x) (transl_prim_arg y) (transl_prim_arg z))
        (Arith.const 1l)
  | Prim (IsInt, [ x ]) -> Arith.(transl_prim_arg x land const 1l)
  | Prim (Extern "%int_add", [ x; y ]) ->
      Arith.(transl_prim_arg x + transl_prim_arg y - const 1l)
  | Prim (Extern "%int_sub", [ x; y ]) ->
      Arith.(transl_prim_arg x - transl_prim_arg y + const 1l)
  | Prim (Extern "%int_mul", [ x; y ]) ->
      Arith.(val_int (int_val (transl_prim_arg x) * int_val (transl_prim_arg y)))
  | Prim (Extern "%int_neg", [ x ]) -> Arith.(const 2l - transl_prim_arg x)
  | Prim (Extern "%int_or", [ x; y ]) -> Arith.(transl_prim_arg x lor transl_prim_arg y)
  | Prim (Extern "%int_and", [ x; y ]) -> Arith.(transl_prim_arg x land transl_prim_arg y)
  | Prim (Extern "%int_xor", [ x; y ]) ->
      Arith.(transl_prim_arg x lxor transl_prim_arg y lor const 1l)
  | Prim (Extern "%int_lsl", [ x; y ]) ->
      Arith.(((transl_prim_arg x - const 1l) lsl int_val (transl_prim_arg y)) + const 1l)
  | Prim (Extern "%int_lsr", [ x; y ]) ->
      Arith.((transl_prim_arg x lsr int_val (transl_prim_arg y)) lor const 1l)
  | Prim (Extern "%int_asr", [ x; y ]) ->
      Arith.((transl_prim_arg x asr int_val (transl_prim_arg y)) lor const 1l)
  | Prim (p, l) -> (
      match p, l with
      | Extern nm, l ->
          (*ZZZ Different calling convention when large number of parameters *)
          register_primitive ctx nm (func_type (List.length l));
          let rec loop acc l =
            match l with
            | [] -> return (W.Call (S nm, List.rev acc))
            | x :: r ->
                let* x = transl_prim_arg x in
                loop (x :: acc) r
          in
          loop [] l
      | Not, [ x ] -> Arith.(const 4l - transl_prim_arg x)
      | Lt, [ x; y ] -> Arith.(val_int (transl_prim_arg x < transl_prim_arg y))
      | Le, [ x; y ] -> Arith.(val_int (transl_prim_arg x <= transl_prim_arg y))
      | Eq, [ x; y ] -> Arith.(val_int (transl_prim_arg x = transl_prim_arg y))
      | Neq, [ x; y ] -> Arith.(val_int (transl_prim_arg x <> transl_prim_arg y))
      | Ult, [ x; y ] ->
          Arith.(val_int (binary (Lt U) (transl_prim_arg x) (transl_prim_arg y)))
      | Array_get, [ x; y ] -> Memory.array_get (transl_prim_arg x) (transl_prim_arg y)
      | IsInt, [ x ] -> Arith.(val_int (transl_prim_arg x land const 1l))
      | Vectlength, [ x ] ->
          Arith.(
            (Memory.mem_load (transl_prim_arg x - const 4l) lsr const 9l) lor const 1l)
      | (Not | Lt | Le | Eq | Neq | Ult | Array_get | IsInt | Vectlength), _ ->
          assert false)

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
      store ~always:true y (load x))
    ~init:(return ())

let translate_closure ctx name_opt toplevel_name params ((pc, _) as cont) acc =
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
        | Return x ->
            let* e = load x in
            instr (Return (Some e))
        | Cond (x, cont1, cont2) ->
            if_
              typ
              Arith.(load x <> const 1l)
              (translate_branch typ pc cont1 (`If :: context))
              (translate_branch typ pc cont2 (`If :: context))
        | Stop ->
            let* e = Arith.const 1l in
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
              (let* () = store ~always:true x (Arith.const 1l) in
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
  let env = empty_env ctx.constants ctx.constant_data in
  let _, env =
    List.fold_left
      ~f:(fun l x ->
        let* _ = l in
        let* _ = add_var x in
        return ())
      ~init:(return ())
      params
      env
  in
  let env =
    match name_opt with
    | Some f ->
        let info = Var.Map.find f ctx.closures in
        let funct_index = function_offset_in_closure info f in
        let _, env =
          if Hashtbl.mem ctx.constants f
          then (), { env with var_count = env.var_count + 1 }
          else
            (let* _ = add_var f in
             return ())
              env
        in
        let _, vars =
          List.fold_left
            ~f:(fun (i, vars) (x, arity) ->
              ( (i + if arity > 1 then 4 else 3)
              , if i = 0 then vars else Var.Map.add x (Rec (f, i)) vars ))
            ~init:(-funct_index, env.vars)
            info.functions
        in
        let start_env = closure_start_env info in
        let _, vars =
          let offset = start_env - funct_index in
          let free_variables =
            List.filter
              ~f:(fun x -> not (Hashtbl.mem ctx.constants x))
              info.free_variables
          in
          List.fold_left
            ~f:(fun (i, vars) x -> i + 1, Var.Map.add x (Env (f, i)) vars)
            ~init:(offset, vars)
            free_variables
        in
        { env with vars }
    | None -> env
  in
  (*
  Format.eprintf "=== %d ===@." pc;
*)
  let _, st = translate_branch (Some (I32 : W.value_type)) (-1) cont [] env in

  let param_count = List.length params + 1 in
  let local_count, body =
    if false
    then st.var_count, List.rev st.instrs
    else Wa_minimize_locals.f ~param_count ~local_count:st.var_count (List.rev st.instrs)
  in
  W.Function
    { name =
        (match name_opt with
        | None -> toplevel_name
        | Some x -> x)
    ; exported_name = None
    ; typ = func_type param_count
    ; locals = List.init ~len:(local_count - param_count) ~f:(fun _ : W.value_type -> I32)
    ; body
    }
  :: acc

let entry_point ctx toplevel_fun entry_name =
  (*ZZZ run ctors? *)
  let typ = { W.params = []; result = [] } in
  let body =
    register_primitive ctx "__wasm_call_ctors" typ;
    let* () = instr (W.CallInstr (S "__wasm_call_ctors", [])) in
    let* sz = Arith.const 3l in
    let* high = Arith.((return (W.MemoryGrow (0, sz)) + const 3l) lsl const 16l) in
    let* () = instr (W.GlobalSet ("young_ptr", high)) in
    let low = W.ConstSym (S "__heap_base", 0) in
    let* () = instr (W.GlobalSet ("young_limit", low)) in
    let* arg = Arith.const 1l in
    drop (return (W.Call (V toplevel_fun, [ arg ])))
  in
  W.Function
    { name = Var.fresh_n "entry_point"
    ; exported_name = Some entry_name
    ; typ
    ; locals = []
    ; body =
        List.rev (snd (body (empty_env (Hashtbl.create 8) (ref Var.Map.empty)))).instrs
    }

let f
    (p : Code.program)
    ~live_vars
     (*
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
      _debug *) =
  let p, closures = Wa_closure_conversion.f p in
  (*
  Code.Print.program (fun _ _ -> "") p;
*)
  let ctx =
    { live = live_vars
    ; blocks = p.blocks
    ; constants = Hashtbl.create 128
    ; closures
    ; primitives = StringMap.empty
    ; constant_data = ref Var.Map.empty
    }
  in
  let toplevel_name = Var.fresh_n "toplevel" in
  let start_function = entry_point ctx toplevel_name "kernel_run" in
  let functions =
    Code.fold_closures_outermost_first
      p
      (fun name_opt params cont ->
        translate_closure ctx name_opt toplevel_name params cont)
      []
  in
  let primitives =
    List.map
      ~f:(fun (name, ty) -> W.Import { name; desc = Fun ty })
      (StringMap.bindings ctx.primitives)
  in
  let constant_data =
    List.map
      ~f:(fun (name, contents) -> W.Data { name; read_only = true; contents })
      (Var.Map.bindings !(ctx.constant_data))
  in
  Wa_asm_output.f
    (W.Global { name = "young_ptr"; typ = I32 }
    :: Global { name = "young_limit"; typ = I32 }
       (*    :: Tag { name = "ocaml_exception"; typ = I32 }*)
    :: (primitives @ functions @ (start_function :: constant_data)))

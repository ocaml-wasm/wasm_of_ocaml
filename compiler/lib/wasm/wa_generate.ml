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

let target = `GC (*`Core*)

(* binaryen does not support block input parameters
   https://github.com/WebAssembly/binaryen/issues/5047 *)
let enable_multivalue = false

open Wa_code_generation

module Generate (Target : Wa_target_sig.S) = struct
  open Target

  let transl_prim_arg x =
    match x with
    | Pv x -> load x
    | Pc c -> Constant.translate c

  type ctx =
    { live : int array
    ; blocks : block Addr.Map.t
    ; closures : Wa_closure_conversion.closure Var.Map.t
    ; mutable primitives : W.func_type StringMap.t
    ; global_context : Wa_code_generation.context
    }

  let register_primitive ctx nm typ =
    (*ZZZ check type*)
    if not (StringMap.mem nm ctx.primitives)
    then ctx.primitives <- StringMap.add nm typ ctx.primitives

  let func_type n =
    { W.params = List.init ~len:n ~f:(fun _ -> Value.value); result = [ Value.value ] }

  let rec translate_expr ctx x e =
    match e with
    | Apply { f; args; exact } when exact || List.length args = 1 ->
        (*ZZZ*)
        let rec loop acc l =
          match l with
          | [] -> (
              let arity = List.length args in
              let funct = Var.fresh () in
              let* closure = tee funct (load f) in
              let* kind, funct = Memory.load_function_pointer ~arity (load funct) in
              let* b = is_closure f in
              if b
              then return (W.Call (V f, List.rev (closure :: acc)))
              else
                match kind, funct with
                | `Index, W.ConstSym (g, 0) | `Ref _, W.RefFunc g ->
                    (* Functions with constant closures ignore their
                       environment *)
                    return (W.Call (g, List.rev (W.I31New (Const (I32 0l)) :: acc)))
                | `Index, _ ->
                    return
                      (W.Call_indirect
                         (Decl (func_type (arity + 1)), funct, List.rev (closure :: acc)))
                | `Ref ty, _ -> return (W.Call_ref (ty, funct, List.rev (closure :: acc)))
              )
          | x :: r ->
              let* x = load x in
              loop (x :: acc) r
        in
        loop [] args
    | Apply { f; args; _ } ->
        let* apply = need_apply_fun ~arity:(List.length args) in
        let* args = expression_list load args in
        let* closure = load f in
        return (W.Call (V apply, args @ [ closure ]))
    | Block (tag, a, _) ->
        Memory.allocate ~tag (List.map ~f:(fun x -> `Var x) (Array.to_list a))
    | Field (x, n) -> Memory.field (load x) n
    | Closure _ -> Closure.translate ~context:ctx.global_context ~closures:ctx.closures x
    | Constant c -> Constant.translate c
    | Prim (p, l) -> (
        let l = List.map ~f:transl_prim_arg l in
        match p, l with
        | Extern "caml_array_unsafe_get", [ x; y ] -> Memory.array_get x y
        | Extern "caml_array_unsafe_set", [ x; y; z ] ->
            seq (Memory.array_set x y z) Value.unit
        | Extern "caml_string_unsafe_get", [ x; y ] -> Memory.bytes_get x y
        | Extern "caml_string_unsafe_set", [ x; y; z ] ->
            seq (Memory.bytes_set x y z) Value.unit
        | Extern "%int_add", [ x; y ] -> Value.int_add x y
        | Extern "%int_sub", [ x; y ] -> Value.int_sub x y
        | Extern "%int_mul", [ x; y ] -> Value.int_mul x y
        | Extern "%int_neg", [ x ] -> Value.int_neg x
        | Extern "%int_or", [ x; y ] -> Value.int_or x y
        | Extern "%int_and", [ x; y ] -> Value.int_and x y
        | Extern "%int_xor", [ x; y ] -> Value.int_xor x y
        | Extern "%int_lsl", [ x; y ] -> Value.int_lsl x y
        | Extern "%int_lsr", [ x; y ] -> Value.int_lsr x y
        | Extern "%int_asr", [ x; y ] -> Value.int_asr x y
        | Extern nm, l ->
            (*ZZZ Different calling convention when large number of parameters *)
            register_primitive ctx nm (func_type (List.length l));
            let rec loop acc l =
              match l with
              | [] -> return (W.Call (S nm, List.rev acc))
              | x :: r ->
                  let* x = x in
                  loop (x :: acc) r
            in
            loop [] l
        | Not, [ x ] -> Value.not x
        | Lt, [ x; y ] -> Value.lt x y
        | Le, [ x; y ] -> Value.le x y
        | Eq, [ x; y ] -> Value.eq x y
        | Neq, [ x; y ] -> Value.neq x y
        | Ult, [ x; y ] -> Value.ult x y
        | Array_get, [ x; y ] -> Memory.array_get x y
        | IsInt, [ x ] -> Value.is_int x
        | Vectlength, [ x ] -> Memory.block_length x
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

  let exception_name = "ocaml_exception"

  let extend_context fall_through context =
    match fall_through with
    | `Block _ as b -> b :: context
    | `Return -> `Skip :: context

  let translate_function ctx name_opt toplevel_name params ((pc, _) as cont) acc =
    let g = Wa_structure.build_graph ctx.blocks pc in
    let idom = Wa_structure.dominator_tree g in
    let dom = Wa_structure.reverse_tree idom in
    let rec index pc i context =
      match context with
      | `Block pc' :: _ when pc = pc' -> i
      | (`Block _ | `Skip) :: rem -> index pc (i + 1) rem
      | [] -> assert false
    in
    let rec translate_tree result_typ fall_through pc context =
      let block = Addr.Map.find pc ctx.blocks in
      let is_switch =
        match block.branch with
        | Switch _ -> true
        | _ -> false
      in
      let param_ty =
        if enable_multivalue then List.map ~f:(fun _ -> Value.value) block.params else []
      in
      let code ~context =
        translate_node_within
          ~param_ty
          ~result_typ
          ~fall_through
          ~pc
          ~l:
            (List.filter
               ~f:(fun pc' -> is_switch || Wa_structure.is_merge_node g pc')
               (List.rev (Addr.Set.elements (Wa_structure.get_edges dom pc))))
          ~context
      in
      if Wa_structure.is_loop_header g pc
      then
        loop
          { params = param_ty; result = result_typ }
          (code ~context:(`Block pc :: context))
      else code ~context
    and translate_node_within ~param_ty ~result_typ ~fall_through ~pc ~l ~context =
      match l with
      | pc' :: rem ->
          let* () =
            let result_typ =
              if enable_multivalue
              then
                let block' = Addr.Map.find pc' ctx.blocks in
                List.map ~f:(fun _ -> Value.value) block'.params
              else []
            in
            let code ~context =
              translate_node_within
                ~param_ty
                ~result_typ
                ~fall_through:(`Block pc')
                ~pc
                ~l:rem
                ~context
            in
            (* Do not insert a block if the inner code contains a
               structured control flow instruction ([if] or [try] *)
            if (not (List.is_empty rem))
               ||
               let block = Addr.Map.find pc ctx.blocks in
               match block.branch with
               | Cond _ | Pushtrap _ -> false (*ZZZ also some Switch*)
               | _ -> true
            then
              block
                { params = param_ty; result = result_typ }
                (code ~context:(`Block pc' :: context))
            else code ~context
          in
          translate_tree result_typ fall_through pc' context
      | [] -> (
          let block = Addr.Map.find pc ctx.blocks in
          let* () =
            if enable_multivalue
            then
              List.fold_left
                block.params
                ~f:(fun continuation x ->
                  (*ZZZ Check order *)
                  let* () = store x (return (W.Pop Value.value)) in
                  continuation)
                ~init:(return ())
            else return ()
          in
          let* () = translate_instrs ctx block.body in
          match block.branch with
          | Branch cont -> translate_branch result_typ fall_through pc cont context
          | Return x -> (
              let* e = load x in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ -> instr (Return (Some e)))
          | Cond (x, cont1, cont2) ->
              let context' = extend_context fall_through context in
              if_
                { params = []; result = result_typ }
                (Value.check_is_not_zero (load x))
                (translate_branch result_typ fall_through pc cont1 context')
                (translate_branch result_typ fall_through pc cont2 context')
          | Stop -> (
              let* e = Value.unit in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ -> instr (Return (Some e)))
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
                  (*ZZZ Use Br_on_cast *)
                  let context' = extend_context fall_through context in
                  if_
                    { params = []; result = result_typ }
                    (Value.check_is_int (load x))
                    (br_table (load x) a1 context')
                    (br_table (Memory.tag (load x)) a2 context'))
          | Raise (x, _) ->
              let* () = use_exceptions in
              let* e = load x in
              instr (Throw (exception_name, e))
          | Pushtrap (cont, x, cont', _) ->
              let context' = extend_context fall_through context in
              let* () = use_exceptions in
              try_
                { params = []; result = result_typ }
                (translate_branch result_typ fall_through pc cont context')
                exception_name
                (let* () = store ~always:true x (return (W.Pop Value.value)) in
                 translate_branch result_typ fall_through pc cont' context')
          | Poptrap cont -> translate_branch result_typ fall_through pc cont context)
    and translate_branch result_typ fall_through src (dst, args) context =
      let* () =
        if enable_multivalue
        then
          List.fold_left
            args
            ~f:(fun continuation x ->
              let* () = continuation in
              let* x = load x in
              instr (Push x))
            ~init:(return ())
        else if List.is_empty args
        then return ()
        else
          let block = Addr.Map.find dst ctx.blocks in
          parallel_renaming block.params args
      in
      if (src >= 0 && Wa_structure.is_backward g src dst)
         || Wa_structure.is_merge_node g dst
      then
        match fall_through with
        | `Block dst' when dst = dst' -> return ()
        | _ -> instr (Br (index dst 0 context, None))
      else translate_tree result_typ fall_through dst context
    in
    let bind_parameters =
      List.fold_left
        ~f:(fun l x ->
          let* _ = l in
          let* _ = add_var x in
          return ())
        ~init:(return ())
        params
    in
    let build_initial_env =
      let* () = bind_parameters in
      match name_opt with
      | Some f ->
          Closure.bind_environment ~context:ctx.global_context ~closures:ctx.closures f
      | None -> return ()
    in
    (*
  Format.eprintf "=== %d ===@." pc;
*)
    let param_count =
      match name_opt with
      | None -> 0
      | Some _ -> List.length params + 1
    in
    let local_count, body =
      function_body
        ~context:ctx.global_context
        ~param_count
        ~body:
          (let* () = build_initial_env in
           translate_branch [ Value.value ] `Return (-1) cont [])
    in
    W.Function
      { name =
          (match name_opt with
          | None -> toplevel_name
          | Some x -> x)
      ; exported_name = None
      ; typ = Decl (func_type param_count)
      ; locals = List.init ~len:(local_count - param_count) ~f:(fun _ -> Value.value)
      ; body
      }
    :: acc

  let entry_point ctx toplevel_fun entry_name =
    let body =
      let* () = entry_point ~register_primitive:(register_primitive ctx) in
      drop (return (W.Call (V toplevel_fun, [])))
    in
    let _, body = function_body ~context:ctx.global_context ~param_count:0 ~body in
    W.Function
      { name = Var.fresh_n "entry_point"
      ; exported_name = Some entry_name
      ; typ = Decl { W.params = []; result = [] }
      ; locals = []
      ; body
      }

  module Curry = Wa_curry.Make (Target)

  let f
      (p : Code.program)
      ~live_vars
       (*
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
      _debug *)
      =
    let p, closures = Wa_closure_conversion.f p in
    (*
  Code.Print.program (fun _ _ -> "") p;
*)
    let ctx =
      { live = live_vars
      ; blocks = p.blocks
      ; closures
      ; primitives = StringMap.empty
      ; global_context = make_context ()
      }
    in
    let toplevel_name = Var.fresh_n "toplevel" in
    let start_function = entry_point ctx toplevel_name "kernel_run" in
    let functions =
      Code.fold_closures_outermost_first
        p
        (fun name_opt params cont ->
          translate_function ctx name_opt toplevel_name params cont)
        []
    in
    let primitives =
      List.map
        ~f:(fun (name, ty) -> W.Import { name; desc = Fun (Decl ty) })
        (StringMap.bindings ctx.primitives)
    in
    let constant_data =
      List.map
        ~f:(fun (name, (active, contents)) ->
          W.Data { name; read_only = true; active; contents })
        (Var.Map.bindings ctx.global_context.data_segments)
    in
    Curry.f ~context:ctx.global_context;
    let fields =
      List.rev_append
        ctx.global_context.other_fields
        (primitives @ functions @ (start_function :: constant_data))
    in
    if ctx.global_context.use_exceptions
    then W.Tag { name = S exception_name; typ = Value.value } :: fields
    else fields
end

let f (p : Code.program) ~live_vars =
  match target with
  | `Core ->
      let module G = Generate (Wa_core_target) in
      let fields = G.f ~live_vars p in
      Wa_asm_output.f fields
  | `GC ->
      let module G = Generate (Wa_gc_target) in
      let fields = G.f ~live_vars p in
      Wa_wat_output.f fields

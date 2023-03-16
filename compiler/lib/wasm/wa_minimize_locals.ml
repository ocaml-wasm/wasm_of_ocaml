(* Reuse locals so as not to use too many of them *)

open Stdlib

type ctx =
  { mutable position : int
  ; last_use : int array
  ; mapping : int array
  ; assignemnt_count : int array
  ; mutable largest_used : int
  ; mutable free_variables : IntSet.t
  }

let handle_assignment ctx i =
  ctx.assignemnt_count.(i) <- ctx.assignemnt_count.(i) + 1;
  (* Extend the variable's scope, but only if it is used. If the
     variable is not used anywhere, we can remove the assignemnt. This
     will be detected by the fact that there is no scope associated to
     it ([ctx.last_use.(i) = -1]). If it is used, we need to include
     the assignment in its scope. *)
  if ctx.last_use.(i) >= 0 then ctx.last_use.(i) <- ctx.position

let rec scan_expression ctx e =
  match e with
  | Wa_ast.Const _ | ConstSym _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull -> ()
  | UnOp (_, e')
  | Load (_, e')
  | Load8 (_, _, e')
  | MemoryGrow (_, e')
  | I31New e'
  | I31Get (_, e')
  | ArrayLength e'
  | StructGet (_, _, _, e')
  | RefCast (_, e')
  | RefTest (_, e')
  | ExternInternalize e'
  | ExternExternalize e' -> scan_expression ctx e'
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | ArrayGet (_, _, e', e'')
  | RefEq (e', e'') ->
      scan_expression ctx e';
      scan_expression ctx e''
  | LocalGet i -> ctx.last_use.(i) <- ctx.position
  | LocalTee (i, e') ->
      scan_expression ctx e';
      ctx.position <- ctx.position + 1;
      handle_assignment ctx i
  | Call_indirect (_, e', l) | Call_ref (_, e', l) ->
      scan_expressions ctx l;
      scan_expression ctx e'
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> scan_expressions ctx l
  | Seq (l, e') ->
      scan_instructions ctx l;
      scan_expression ctx e'

and scan_expressions ctx l = List.iter ~f:(fun e -> scan_expression ctx e) l

and scan_instruction ctx i =
  match i with
  | Wa_ast.Drop e
  | GlobalSet (_, e)
  | Br (_, Some e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e)
  | Push e
  | Br_on_cast (_, _, e)
  | Br_on_cast_fail (_, _, e) -> scan_expression ctx e
  | Store (_, e, e') | Store8 (_, _, e, e') | StructSet (_, _, _, e, e') ->
      scan_expression ctx e;
      scan_expression ctx e'
  | LocalSet (i, e) ->
      scan_expression ctx e;
      ctx.position <- ctx.position + 1;
      handle_assignment ctx i
  | Loop (_, l) | Block (_, l) -> scan_instructions ctx l
  | If (_, e, l, l') ->
      scan_expression ctx e;
      scan_instructions ctx l;
      scan_instructions ctx l'
  | Try (_, body, catches, catch_all) ->
      scan_instructions ctx body;
      List.iter ~f:(fun (_, l) -> scan_instructions ctx l) catches;
      Option.iter ~f:(fun l -> scan_instructions ctx l) catch_all
  | CallInstr (_, l) | Return_call (_, l) -> scan_expressions ctx l
  | Br (_, None) | Return None | Rethrow _ | Nop -> ()
  | ArraySet (_, _, e, e', e'') ->
      scan_expression ctx e;
      scan_expression ctx e';
      scan_expression ctx e''
  | Return_call_indirect (_, e', l) | Return_call_ref (_, e', l) ->
      scan_expressions ctx l;
      scan_expression ctx e'

and scan_instructions ctx l = List.iter ~f:(fun i -> scan_instruction ctx i) l

let assignment ctx v e =
  ctx.position <- ctx.position + 1;
  let v' =
    let v' = ctx.mapping.(v) in
    if v' <> -1
    then v'
    else
      match e with
      | Wa_ast.LocalGet v0 when IntSet.mem ctx.mapping.(v0) ctx.free_variables ->
          (* Reuse the same register to eliminate the assignment *)
          let v' = ctx.mapping.(v0) in
          ctx.free_variables <- IntSet.remove v' ctx.free_variables;
          ctx.mapping.(v) <- v';
          v'
      | Wa_ast.LocalGet v0
        when ctx.last_use.(v0) >= ctx.last_use.(v) && ctx.assignemnt_count.(v) = 1 ->
          (* Reuse the same register to eliminate the assignment *)
          ctx.last_use.(v) <- ctx.last_use.(v0);
          let v' = ctx.mapping.(v0) in
          ctx.mapping.(v) <- v';
          v'
      | _ -> (
          match IntSet.min_elt_opt ctx.free_variables with
          | Some v' ->
              ctx.free_variables <- IntSet.remove v' ctx.free_variables;
              ctx.mapping.(v) <- v';
              v'
          | None ->
              let v' = ctx.largest_used + 1 in
              ctx.largest_used <- v';
              ctx.mapping.(v) <- v';
              v')
  in
  if ctx.last_use.(v) = ctx.position
  then ctx.free_variables <- IntSet.add v' ctx.free_variables;
  v'

let rec rewrite_expression ctx e =
  match e with
  | Wa_ast.Const _ | ConstSym _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull -> e
  | UnOp (op, e') -> UnOp (op, rewrite_expression ctx e')
  | BinOp (op, e', e'') ->
      let e' = rewrite_expression ctx e' in
      let e'' = rewrite_expression ctx e'' in
      BinOp (op, e', e'')
  | Load (op, e') -> Load (op, rewrite_expression ctx e')
  | Load8 (s, op, e') -> Load8 (s, op, rewrite_expression ctx e')
  | LocalGet v ->
      let v' = ctx.mapping.(v) in
      assert (v' <> -1);
      if ctx.position = ctx.last_use.(v)
      then ctx.free_variables <- IntSet.add v' ctx.free_variables;
      LocalGet v'
  | LocalTee (v, e0) -> (
      let e' = rewrite_expression ctx e0 in
      if ctx.last_use.(v) = -1
      then (
        ctx.position <- ctx.position + 1;
        e')
      else
        let v' = assignment ctx v e0 in
        match e' with
        | LocalGet v'' when v' = v'' -> e'
        | _ -> LocalTee (v', e'))
  | Call_indirect (typ, e', l) ->
      let l = rewrite_expressions ctx l in
      let e' = rewrite_expression ctx e' in
      Call_indirect (typ, e', l)
  | Call (f, l) -> Call (f, rewrite_expressions ctx l)
  | MemoryGrow (m, e') -> MemoryGrow (m, rewrite_expression ctx e')
  | Seq (l, e') ->
      let l = rewrite_instructions ctx l in
      let e' = rewrite_expression ctx e' in
      Seq (l, e')
  | Call_ref (typ, e', l) ->
      let l = rewrite_expressions ctx l in
      let e' = rewrite_expression ctx e' in
      Call_ref (typ, e', l)
  | I31New e' -> I31New (rewrite_expression ctx e')
  | I31Get (s, e') -> I31Get (s, rewrite_expression ctx e')
  | ArrayNew (symb, e', e'') ->
      ArrayNew (symb, rewrite_expression ctx e', rewrite_expression ctx e'')
  | ArrayNewFixed (symb, l) -> ArrayNewFixed (symb, rewrite_expressions ctx l)
  | ArrayNewData (symb, symb', e', e'') ->
      ArrayNewData (symb, symb', rewrite_expression ctx e', rewrite_expression ctx e'')
  | ArrayGet (s, symb, e', e'') ->
      ArrayGet (s, symb, rewrite_expression ctx e', rewrite_expression ctx e'')
  | ArrayLength e' -> ArrayLength (rewrite_expression ctx e')
  | StructNew (symb, l) -> StructNew (symb, rewrite_expressions ctx l)
  | StructGet (s, symb, i, e') -> StructGet (s, symb, i, rewrite_expression ctx e')
  | RefCast (ty, e') -> RefCast (ty, rewrite_expression ctx e')
  | RefTest (ty, e') -> RefTest (ty, rewrite_expression ctx e')
  | RefEq (e', e'') -> RefEq (rewrite_expression ctx e', rewrite_expression ctx e'')
  | ExternInternalize e' -> ExternInternalize (rewrite_expression ctx e')
  | ExternExternalize e' -> ExternExternalize (rewrite_expression ctx e')

and rewrite_expressions ctx l = List.map ~f:(fun e -> rewrite_expression ctx e) l

and rewrite_instruction ctx i =
  match i with
  | Wa_ast.Drop e -> Wa_ast.Drop (rewrite_expression ctx e)
  | Store (op, e, e') ->
      let e = rewrite_expression ctx e in
      let e' = rewrite_expression ctx e' in
      Store (op, e, e')
  | Store8 (s, op, e, e') ->
      let e = rewrite_expression ctx e in
      let e' = rewrite_expression ctx e' in
      Store8 (s, op, e, e')
  | LocalSet (v, e0) -> (
      let e = rewrite_expression ctx e0 in
      if ctx.last_use.(v) = -1
      then (
        ctx.position <- ctx.position + 1;
        Drop e)
      else
        let v' = assignment ctx v e0 in
        match e with
        | LocalGet v'' when v' = v'' -> Nop
        | _ -> LocalSet (v', e))
  | GlobalSet (nm, e) -> GlobalSet (nm, rewrite_expression ctx e)
  | Loop (typ, l) -> Loop (typ, rewrite_instructions ctx l)
  | Block (typ, l) -> Block (typ, rewrite_instructions ctx l)
  | If (typ, e, l, l') ->
      let e = rewrite_expression ctx e in
      let l = rewrite_instructions ctx l in
      let l' = rewrite_instructions ctx l' in
      If (typ, e, l, l')
  | Try (typ, body, catches, catch_all) ->
      let body = rewrite_instructions ctx body in
      let catches =
        List.map ~f:(fun (tag, l) -> tag, rewrite_instructions ctx l) catches
      in
      let catch_all = Option.map ~f:(fun l -> rewrite_instructions ctx l) catch_all in
      Try (typ, body, catches, catch_all)
  | Br (label, Some e) -> Br (label, Some (rewrite_expression ctx e))
  | Br_table (e, l, label) -> Br_table (rewrite_expression ctx e, l, label)
  | Return (Some e) -> Return (Some (rewrite_expression ctx e))
  | Throw (i, e) -> Throw (i, rewrite_expression ctx e)
  | CallInstr (f, l) -> CallInstr (f, List.map ~f:(fun e -> rewrite_expression ctx e) l)
  | Push e -> Push (rewrite_expression ctx e)
  | ArraySet (s, symb, e, e', e'') ->
      ArraySet
        ( s
        , symb
        , rewrite_expression ctx e
        , rewrite_expression ctx e'
        , rewrite_expression ctx e'' )
  | StructSet (s, symb, i, e, e') ->
      StructSet (s, symb, i, rewrite_expression ctx e, rewrite_expression ctx e')
  | Br_on_cast (i, ty, e) -> Br_on_cast (i, ty, rewrite_expression ctx e)
  | Br_on_cast_fail (i, ty, e) -> Br_on_cast_fail (i, ty, rewrite_expression ctx e)
  | Br (_, None) | Return None | Rethrow _ | Nop -> i
  | Return_call_indirect (typ, e', l) ->
      let l = rewrite_expressions ctx l in
      let e' = rewrite_expression ctx e' in
      Return_call_indirect (typ, e', l)
  | Return_call (f, l) -> Return_call (f, rewrite_expressions ctx l)
  | Return_call_ref (typ, e', l) ->
      let l = rewrite_expressions ctx l in
      let e' = rewrite_expression ctx e' in
      Return_call_ref (typ, e', l)

and rewrite_instructions ctx l = List.map ~f:(fun i -> rewrite_instruction ctx i) l

let f ~param_count ~local_count instrs =
  let ctx =
    { position = 0
    ; last_use = Array.make (max param_count local_count) (-1)
    ; mapping = Array.make (max param_count local_count) (-1)
    ; assignemnt_count = Array.make (max param_count local_count) 0
    ; largest_used = param_count - 1
    ; free_variables = IntSet.empty
    }
  in
  scan_instructions ctx instrs;
  for i = 0 to param_count - 1 do
    ctx.mapping.(i) <- i;
    if ctx.last_use.(i) < 0
    then (* unused parameters *)
      ctx.free_variables <- IntSet.add i ctx.free_variables
  done;
  ctx.position <- 0;
  let instrs = rewrite_instructions ctx instrs in
  (*
  Format.eprintf
    "ZZZZ %d ==> %d@."
    (local_count - param_count)
    (ctx.largest_used + 1 - param_count);
*)
  ctx.largest_used + 1, instrs

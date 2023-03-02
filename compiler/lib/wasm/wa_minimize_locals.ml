(* Reuse locals so as not to use too many of them *)

open Stdlib

type ctx =
  { mutable position : int
  ; last_use : int array
  ; mapping : int array
  ; mutable largest_used : int
  ; mutable free_variables : IntSet.t
  }

let rec scan_expression ctx e =
  match e with
  | Wa_ast.Const _ | ConstSym _ | GlobalGet _ -> ()
  | UnOp (_, e') | Load (_, e') -> scan_expression ctx e'
  | BinOp (_, e', e'') ->
      scan_expression ctx e';
      scan_expression ctx e''
  | LocalGet i -> ctx.last_use.(i) <- ctx.position
  | LocalTee (i, e') ->
      scan_expression ctx e';
      ctx.position <- ctx.position + 1;
      ctx.last_use.(i) <- ctx.position
  | Call_indirect (_, e', l) ->
      List.iter ~f:(fun e' -> scan_expression ctx e') l;
      scan_expression ctx e'
  | Call (_, l) -> List.iter ~f:(fun e' -> scan_expression ctx e') l
  | Seq (l, e') ->
      List.iter ~f:(fun i -> scan_instruction ctx i) l;
      scan_expression ctx e'

and scan_instruction ctx i =
  match i with
  | Drop e | GlobalSet (_, e) | Br (_, Some e) | Br_table (e, _, _) | Return (Some e) ->
      scan_expression ctx e
  | Store (_, e, e') ->
      scan_expression ctx e;
      scan_expression ctx e'
  | LocalSet (i, e) ->
      scan_expression ctx e;
      ctx.position <- ctx.position + 1;
      ctx.last_use.(i) <- ctx.position
  | Loop (_, l) | Block (_, l) -> scan_instructions ctx l
  | If (_, e, l, l') ->
      scan_expression ctx e;
      scan_instructions ctx l;
      scan_instructions ctx l'
  | Br (_, None) | Return None | Nop -> ()

and scan_instructions ctx l = List.iter ~f:(fun i' -> scan_instruction ctx i') l

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
  | Wa_ast.Const _ | ConstSym _ | GlobalGet _ -> e
  | UnOp (op, e') -> UnOp (op, rewrite_expression ctx e')
  | BinOp (op, e', e'') ->
      let e' = rewrite_expression ctx e' in
      let e'' = rewrite_expression ctx e'' in
      BinOp (op, e', e'')
  | Load (op, e') -> Load (op, rewrite_expression ctx e')
  | LocalGet v ->
      let v' = ctx.mapping.(v) in
      if ctx.position = ctx.last_use.(v)
      then ctx.free_variables <- IntSet.add v' ctx.free_variables;
      LocalGet v'
  | LocalTee (v, e') -> (
      let e' = rewrite_expression ctx e' in
      let v' = assignment ctx v e in
      match e' with
      | LocalGet v'' when v' = v'' -> e'
      | _ -> LocalTee (v', e'))
  | Call_indirect (typ, e', l) ->
      let l = List.map ~f:(fun e' -> rewrite_expression ctx e') l in
      let e' = rewrite_expression ctx e' in
      Call_indirect (typ, e', l)
  | Call (f, l) -> Call (f, List.map ~f:(fun e' -> rewrite_expression ctx e') l)
  | Seq (l, e') ->
      let l = List.map ~f:(fun i -> rewrite_instruction ctx i) l in
      let e' = rewrite_expression ctx e' in
      Seq (l, e')

and rewrite_instruction ctx i =
  match i with
  | Drop e -> Drop (rewrite_expression ctx e)
  | Store (op, e, e') ->
      let e = rewrite_expression ctx e in
      let e' = rewrite_expression ctx e' in
      Store (op, e, e')
  | LocalSet (v, e) -> (
      let e = rewrite_expression ctx e in
      let v' = assignment ctx v e in
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
  | Br (label, Some e) -> Br (label, Some (rewrite_expression ctx e))
  | Br_table (e, l, label) -> Br_table (rewrite_expression ctx e, l, label)
  | Return (Some e) -> Return (Some (rewrite_expression ctx e))
  | Br (_, None) | Return None | Nop -> i

and rewrite_instructions ctx l = List.map ~f:(fun i' -> rewrite_instruction ctx i') l

let f ~param_count ~local_count instrs =
  let ctx =
    { position = 0
    ; last_use = Array.make local_count (-1)
    ; mapping = Array.make local_count (-1)
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
  Format.eprintf "ZZZZ %d ==> %d@." local_count (ctx.largest_used - 1);
  ctx.largest_used + 1, instrs

open Stdlib

type effect_kind =
  | Global of Wa_ast.var
  | Array
  | Memory
  | Call
  | Pop

type effect = effect_kind * bool

let is_pop e =
  match e with
  | Pop, _ -> true
  | _ -> false

let effect_add e l =
  match l, e with
  | _, (Call, _) -> if List.exists ~f:is_pop l then [ e; Pop, false ] else [ e ]
  | [ ((Call, _) as e') ], (Pop, _) -> [ e'; e ]
  | (Call, _) :: _, _ -> l
  | _ -> e :: l

let effect_union l l' =
  match l, l' with
  | (Call, _) :: _, _ -> l
  | _, (Call, _) :: _ -> l'
  | _ -> l @ l'

let conflicts e e' =
  match e, e' with
  | (Global x, set), (Global x', set') when (set || set') && Poly.equal x x' -> true
  | (Array, set), (Array, set') -> set || set'
  | (Memory, set), (Memory, set') -> set || set'
  | (Call, _), _ | _, (Call, _) -> true
  | _ -> false

module Context : sig
  type 'a t

  val return : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val perform : 'a t -> 'a

  val lookup : int -> Wa_ast.expression t

  val effect : effect -> 'a -> 'a t

  val flush_all : unit t

  val instruction : Wa_ast.instruction t -> Wa_ast.instruction list t

  val push : int -> Wa_ast.expression t -> Wa_ast.instruction list t

  val block : Wa_ast.instruction list t -> Wa_ast.instruction list t
end = struct
  type assignement =
    { expr : Wa_ast.expression
    ; effects : effect list
    ; order : int
    }

  type context =
    { pending_assignments : assignement IntMap.t
    ; flushed_assignments : IntSet.t
    ; effects : effect list
    ; position : int
    }

  type 'a t = context -> 'a * context

  let return e ctx = e, ctx

  let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
   fun ctx ->
    let v, ctx = e ctx in
    f v ctx

  let perform f =
    fst
      (f
         { pending_assignments = IntMap.empty
         ; flushed_assignments = IntSet.empty
         ; effects = []
         ; position = 0
         })

  let lookup x ctx =
    match IntMap.find_opt x ctx.pending_assignments with
    | Some { expr; _ } ->
        ( Wa_ast.LocalTee (x, expr)
        , { ctx with pending_assignments = IntMap.remove x ctx.pending_assignments } )
    | None -> Wa_ast.LocalGet x, ctx

  let flushed_assignments ctx =
    let l =
      List.sort
        ~cmp:(fun (_, i) (_, i') -> compare i' i)
        (IntSet.fold
           (fun x l -> (x, (IntMap.find x ctx.pending_assignments).order) :: l)
           ctx.flushed_assignments
           [])
    in
    snd (List.fold_left ~f:(fun (ctx, l) _x -> (*ZZZ*) ctx, l) ~init:(ctx, []) l)

  let effect e x ctx =
    let flushed_assignments =
      IntMap.fold
        (fun x ({ effects; _ } : assignement) flushed ->
          if List.exists ~f:(fun e' -> conflicts e e') effects
          then IntSet.add x flushed
          else flushed)
        ctx.pending_assignments
        ctx.flushed_assignments
    in
    x, { ctx with flushed_assignments; effects = effect_add e ctx.effects }

  let flush_all ctx =
    ( ()
    , { ctx with
        flushed_assignments =
          IntMap.fold
            (fun x _ flushed -> IntSet.add x flushed)
            ctx.pending_assignments
            IntSet.empty
      } )

  let instruction i ctx =
    assert (IntSet.is_empty ctx.flushed_assignments);
    let i, ctx' = i { ctx with effects = [] } in
    ( flushed_assignments ctx' @ [ i ]
    , { ctx' with
        flushed_assignments = IntSet.empty
      ; effects = effect_union ctx.effects ctx'.effects
      } )

  let push x e ctx =
    assert (IntSet.is_empty ctx.flushed_assignments);
    let e, ctx' = e { ctx with effects = [] } in
    assert (not (IntMap.mem x ctx.pending_assignments));
    if List.exists ~f:is_pop ctx'.effects
    then
      (* Assignments containing a Pop are flushed at once, since it is
         otherwise hard to control the contents of the stack when the
         Pop is performed *)
      ( flushed_assignments ctx' @ [ Wa_ast.LocalSet (x, e) ]
      , { ctx' with
          flushed_assignments = IntSet.empty
        ; effects =
            effect_union
              ctx.effects
              (List.filter ~f:(fun x -> not (is_pop x)) ctx'.effects)
        } )
    else
      ( flushed_assignments ctx'
      , { pending_assignments =
            IntMap.add
              x
              { expr = e; effects = ctx'.effects; order = ctx.position }
              ctx'.pending_assignments
        ; flushed_assignments = IntSet.empty
        ; effects = effect_union ctx.effects ctx'.effects
        ; position = ctx.position + 1
        } )

  let block l =
    let* () = flush_all in
    fun ctx ->
      assert (IntMap.is_empty ctx.pending_assignments);
      let l', ctx' =
        (let* l = l in
         let* () = flush_all in
         return l)
          { ctx with flushed_assignments = IntSet.empty }
      in
      assert (IntMap.is_empty ctx'.pending_assignments);
      ( l' @ flushed_assignments ctx'
      , { flushed_assignments = ctx.flushed_assignments
        ; pending_assignments = ctx.pending_assignments
        ; effects = effect_union ctx.effects ctx'.effects
        ; position = ctx'.position
        } )
end

open Context

(*
let rec expression_effects e acc =
  match e with
  | Wa_ast.Const _ | ConstSym _ | RefFunc _ | RefNull | LocalGet _ -> acc
  | GlobalGet v -> effect_add (Global v, false) acc
  | Pop _ -> effect_add (Pop, false) acc
  | UnOp (_, e')
  | LocalTee (_, e')
  | I31New e'
  | I31Get (_, e')
  | ArrayLen e'
  | StructGet (_, _, _, e')
  | ExternInternalize e'
  | ExternExternalize e' -> expression_effects e' acc
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | RefEq (e', e'') -> acc |> expression_effects e' |> expression_effects e''
  | Load (_, e') | Load8 (_, _, e') | RefCast (_, e') | RefTest (_, e') ->
      acc |> expression_effects e' |> effect_add (Memory, false)
  | Call_indirect (_, e', l) | Call_ref (_, e', l) ->
      acc |> expression_list_effects l |> expression_effects e' |> effect_add (Call, true)
  | Call (_, l) -> acc |> expression_list_effects l |> effect_add (Call, true)
  | MemoryGrow (_, e') -> acc |> expression_effects e' |> effect_add (Memory, false)
  | Seq _ -> assert false
  | ArrayNewFixed (_, l) | StructNew (_, l) -> expression_list_effects l acc
  | ArrayGet (_, _, e', e'') ->
      acc |> expression_effects e' |> expression_effects e'' |> effect_add (Array, false)

and expression_list_effects l acc =
  List.fold_left ~f:(fun acc e -> expression_effects e acc) ~init:acc l
*)
let rec rewrite_expression e =
  match e with
  | Wa_ast.Const _ | ConstSym _ | RefFunc _ | RefNull -> return e
  | GlobalGet v -> effect (Global v, false) e
  | Pop _ -> effect (Pop, false) e
  | UnOp (op, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.UnOp (op, e'))
  | BinOp (op, e', e'') ->
      let* e' = rewrite_expression e' in
      let* e'' = rewrite_expression e'' in
      return (Wa_ast.BinOp (op, e', e''))
  | I32WrapI64 e' ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.I32WrapI64 e')
  | I64ExtendI32 (s, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.I64ExtendI32 (s, e'))
  | Load (op, e') ->
      let* e' = rewrite_expression e' in
      effect (Memory, false) (Wa_ast.Load (op, e'))
  | Load8 (s, op, e') ->
      let* e' = rewrite_expression e' in
      effect (Memory, false) (Wa_ast.Load8 (s, op, e'))
  | LocalGet v -> lookup v
  | LocalTee (v, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.LocalTee (v, e'))
  | Call_indirect (typ, e', l) ->
      let* l = rewrite_expressions l in
      let* e' = rewrite_expression e' in
      effect (Call, true) (Wa_ast.Call_indirect (typ, e', l))
  | Call (f, l) ->
      let* l = rewrite_expressions l in
      effect (Call, true) (Wa_ast.Call (f, l))
  | MemoryGrow (m, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.MemoryGrow (m, e'))
  | Seq _ -> assert false
  | Call_ref (typ, e', l) ->
      let* l = rewrite_expressions l in
      let* e' = rewrite_expression e' in
      effect (Call, true) (Wa_ast.Call_ref (typ, e', l))
  | I31New e' ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.I31New e')
  | I31Get (s, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.I31Get (s, e'))
  | ArrayNew (symb, e', e'') ->
      let* e' = rewrite_expression e' in
      let* e'' = rewrite_expression e'' in
      return (Wa_ast.ArrayNew (symb, e', e''))
  | ArrayNewFixed (symb, l) ->
      let* l = rewrite_expressions l in
      return (Wa_ast.ArrayNewFixed (symb, l))
  | ArrayNewData (symb, symb', e', e'') ->
      let* e' = rewrite_expression e' in
      let* e'' = rewrite_expression e'' in
      return (Wa_ast.ArrayNewData (symb, symb', e', e''))
  | ArrayGet (s, symb, e', e'') ->
      let* e' = rewrite_expression e' in
      let* e'' = rewrite_expression e'' in
      effect (Array, false) (Wa_ast.ArrayGet (s, symb, e', e''))
  | ArrayLen e' ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.ArrayLen e')
  | StructNew (symb, l) ->
      let* l = rewrite_expressions l in
      return (Wa_ast.StructNew (symb, l))
  | StructGet (s, symb, i, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.StructGet (s, symb, i, e'))
  | RefCast (ty, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.RefCast (ty, e'))
  | RefTest (ty, e') ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.RefTest (ty, e'))
  | RefEq (e', e'') ->
      let* e' = rewrite_expression e' in
      let* e'' = rewrite_expression e'' in
      return (Wa_ast.RefEq (e', e''))
  | ExternInternalize e' ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.ExternInternalize e')
  | ExternExternalize e' ->
      let* e' = rewrite_expression e' in
      return (Wa_ast.ExternExternalize e')

and rewrite_expressions l =
  let* l =
    List.fold_left
      ~f:(fun acc e ->
        let* acc = acc in
        let* e = rewrite_expression e in
        return (e :: acc))
      ~init:(return [])
      l
  in
  return (List.rev l)

and rewrite_instruction i =
  match i with
  | Wa_ast.Drop e ->
      instruction
        (let* e = rewrite_expression e in
         return (Wa_ast.Drop e))
  | Store (op, e, e') ->
      instruction
        (let* e = rewrite_expression e in
         let* e' = rewrite_expression e' in
         effect (Memory, true) (Wa_ast.Store (op, e, e')))
  | Store8 (op, e, e') ->
      instruction
        (let* e = rewrite_expression e in
         let* e' = rewrite_expression e' in
         effect (Memory, true) (Wa_ast.Store8 (op, e, e')))
  | LocalSet (v, e) -> push v (rewrite_expression e)
  | GlobalSet (nm, e) ->
      instruction
        (let* e = rewrite_expression e in
         effect (Global nm, true) (Wa_ast.GlobalSet (nm, e)))
  | Loop (typ, l) ->
      instruction
        (let* l = rewrite_block l in
         return (Wa_ast.Loop (typ, l)))
  | Block (typ, l) ->
      instruction
        (let* l = rewrite_block l in
         return (Wa_ast.Block (typ, l)))
  | If (typ, e, l, l') ->
      instruction
        (let* e = rewrite_expression e in
         let* l = rewrite_block l in
         let* l' = rewrite_block l' in
         return (Wa_ast.If (typ, e, l, l')))
  | Try (typ, body, catches, catch_all) ->
      instruction
        (let* body = rewrite_block body in
         let* catches =
           List.fold_left
             ~f:(fun acc (tag, l) ->
               let* acc = acc in
               let* l = rewrite_block l in
               return ((tag, l) :: acc))
             ~init:(return [])
             catches
         in
         let* catch_all =
           match catch_all with
           | None -> return None
           | Some l ->
               let* l = rewrite_block l in
               return (Some l)
         in
         return (Wa_ast.Try (typ, body, List.rev catches, catch_all)))
  | Br (label, Some e) ->
      instruction
        (let* e = rewrite_expression e in
         let* () = flush_all in
         return (Wa_ast.Br (label, Some e)))
  | Br_table (e, l, label) ->
      instruction
        (let* e = rewrite_expression e in
         let* () = flush_all in
         return (Wa_ast.Br_table (e, l, label)))
  | Return (Some e) ->
      instruction
        (let* e = rewrite_expression e in
         let* () = flush_all in
         return (Wa_ast.Return (Some e)))
  | Throw (i, e) ->
      instruction
        (let* e = rewrite_expression e in
         let* () = flush_all in
         return (Wa_ast.Throw (i, e)))
  | CallInstr (f, l) ->
      instruction
        (let* l = rewrite_expressions l in
         effect (Call, true) (Wa_ast.CallInstr (f, l)))
  | Push e ->
      instruction
        (let* e = rewrite_expression e in
         return (Wa_ast.Push e))
  | ArraySet (symb, e, e', e'') ->
      instruction
        (let* e = rewrite_expression e in
         let* e' = rewrite_expression e' in
         let* e'' = rewrite_expression e'' in
         return (Wa_ast.ArraySet (symb, e, e', e'')))
  | StructSet (symb, i, e, e') ->
      instruction
        (let* e = rewrite_expression e in
         let* e' = rewrite_expression e' in
         return (Wa_ast.StructSet (symb, i, e, e')))
  | Br_on_cast (i, ty, ty', e) ->
      instruction
        (let* e = rewrite_expression e in
         let* () = flush_all in
         return (Wa_ast.Br_on_cast (i, ty, ty', e)))
  | Br_on_cast_fail (i, ty, ty', e) ->
      instruction
        (let* e = rewrite_expression e in
         let* () = flush_all in
         return (Wa_ast.Br_on_cast_fail (i, ty, ty', e)))
  | Br (_, None) | Return None | Rethrow _ ->
      instruction
        (let* () = flush_all in
         return i)
  | Nop -> instruction (return i)
  | Return_call_indirect (typ, e', l) ->
      instruction
        (let* l = rewrite_expressions l in
         let* e' = rewrite_expression e' in
         let* () = flush_all in
         return (Wa_ast.Return_call_indirect (typ, e', l)))
  | Return_call (f, l) ->
      instruction
        (let* l = rewrite_expressions l in
         let* () = flush_all in
         return (Wa_ast.Return_call (f, l)))
  | Return_call_ref (typ, e', l) ->
      instruction
        (let* l = rewrite_expressions l in
         let* e' = rewrite_expression e' in
         return (Wa_ast.Return_call_ref (typ, e', l)))

and rewrite_block_rec l =
  match l with
  | i :: rem ->
      let* i = rewrite_instruction i in
      let* l = rewrite_block_rec rem in
      return (i @ l)
  | [] -> return []

and rewrite_block l = block (rewrite_block_rec l)

let f instrs = perform (rewrite_block instrs)

(*
Queue of expressions that commute with one another

Parallel renaming: do not assign to a variable still in the queue
*)

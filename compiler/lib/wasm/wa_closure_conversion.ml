(*
A lot like lambda lifting
===> get function free variables
===> merge closure of mutually recursive functions

Free variables of a function:
- immediate free variables
- free variables from subfunctions which are not bound
*)

open! Stdlib
open Code

let collect_free_vars program var_depth depth pc =
  let vars = ref Var.Set.empty in
  let rec traverse pc =
    Code.preorder_traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Code.Addr.Map.find pc program.blocks in
        Freevars.iter_block_free_vars
          (fun x ->
            let idx = Var.idx x in
            if idx < Array.length var_depth
            then (
              let d = var_depth.(idx) in
              assert (d >= 0);
              if d < depth then vars := Var.Set.add x !vars))
          block;
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (_, Closure (_, (pc', _))) -> traverse pc'
            | _ -> ()))
      pc
      program.blocks
      ()
  in
  traverse pc;
  !vars

let mark_bound_variables var_depth block depth =
  Freevars.iter_block_bound_vars (fun x -> var_depth.(Var.idx x) <- depth) block;
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Closure (params, _)) ->
          List.iter params ~f:(fun x -> var_depth.(Var.idx x) <- depth + 1)
      | _ -> ())

let rec traverse var_depth (program, functions) pc depth =
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc (program, functions) ->
      let block = Code.Addr.Map.find pc program.blocks in
      mark_bound_variables var_depth block depth;
      let rec rewrite_body st l =
        match l with
        | Let (f, (Closure (_, (pc', _)) as cl)) :: rem ->
            let program, functions = traverse var_depth st pc' (depth + 1) in
            let free_vars = collect_free_vars program var_depth (depth + 1) pc' in
            let s =
              Var.Set.fold
                (fun x m -> Var.Map.add x (Var.fork x) m)
                free_vars
                Var.Map.empty
            in
            let program = Subst.cont (Subst.from_map s) pc' program in
            let f' = try Var.Map.find f s with Not_found -> Var.fork f in
            let s = Var.Map.bindings (Var.Map.remove f s) in
            let f'' = Var.fork f in
            let pc'' = program.free_pc in
            let bl = { params = []; body = [ Let (f', cl) ]; branch = Return f' } in
            let program =
              { program with
                free_pc = pc'' + 1
              ; blocks = Addr.Map.add pc'' bl program.blocks
              }
            in
            let functions =
              Let (f'', Closure (List.map s ~f:snd, (pc'', []))) :: functions
            in
            let rem', st = rewrite_body (program, functions) rem in
            Let (f, Apply { f = f''; args = List.map ~f:fst s; exact = true }) :: rem', st
        | i :: rem ->
            let rem', st = rewrite_body st rem in
            i :: rem', st
        | [] -> [], st
      in
      let body, (program, functions) = rewrite_body (program, functions) block.body in
      ( { program with blocks = Addr.Map.add pc { block with body } program.blocks }
      , functions ))
    pc
    program.blocks
    (program, functions)

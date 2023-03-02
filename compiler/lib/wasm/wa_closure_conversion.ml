open! Stdlib
open Code

type closure =
  { functions : (Var.t * int) list
  ; free_variables : Var.t list
  }

module SCC = Strongly_connected_components.Make (Var)

let rec collect_closures acc clos_acc instrs =
  let push_closures clos_acc acc =
    if List.is_empty clos_acc then acc else clos_acc :: acc
  in
  match instrs with
  | [] -> push_closures clos_acc acc
  | Let (f, Closure (params, (pc, _))) :: rem ->
      collect_closures acc ((f, List.length params, pc) :: clos_acc) rem
  | _ :: rem -> collect_closures (push_closures clos_acc acc) [] rem

let collect_free_vars program var_depth depth pc closures =
  let vars = ref Var.Set.empty in
  let add_if_free_variable x =
    let idx = Var.idx x in
    let d = var_depth.(idx) in
    assert (d >= 0);
    if d < depth then vars := Var.Set.add x !vars
  in
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc () ->
      let block = Code.Addr.Map.find pc program.blocks in
      Freevars.iter_block_free_vars add_if_free_variable block;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (f, Closure _) -> (
              match Var.Map.find_opt f closures with
              | Some { functions = (g, _) :: _; free_variables; _ } when Var.equal f g ->
                  List.iter ~f:add_if_free_variable free_variables
              | Some _ | None -> ())
          | _ -> ()))
    pc
    program.blocks
    ();
  !vars

let mark_bound_variables var_depth block depth =
  Freevars.iter_block_bound_vars (fun x -> var_depth.(Var.idx x) <- depth) block;
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Closure (params, _)) ->
          List.iter params ~f:(fun x -> var_depth.(Var.idx x) <- depth + 1)
      | _ -> ())

let rec traverse var_depth program pc depth closures =
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc closures ->
      let block = Code.Addr.Map.find pc program.blocks in
      mark_bound_variables var_depth block depth;
      List.fold_left
        ~f:(fun closures l ->
          let closures =
            List.fold_left
              ~f:(fun closures (_, _, pc') ->
                traverse var_depth program pc' (depth + 1) closures)
              ~init:closures
              l
          in
          let free_vars =
            List.fold_left
              ~f:(fun free_vars (f, _, pc') ->
                Var.Map.add
                  f
                  (collect_free_vars program var_depth (depth + 1) pc' closures)
                  free_vars)
              ~init:Var.Map.empty
              l
          in
          let domain =
            List.fold_left ~f:(fun s (f, _, _) -> Var.Set.add f s) ~init:Var.Set.empty l
          in
          let graph = Var.Map.map (fun s -> Var.Set.inter s domain) free_vars in
          let components = SCC.connected_components_sorted_from_roots_to_leaf graph in
          Array.fold_left
            ~f:(fun closures component ->
              let functions =
                match component with
                | SCC.No_loop x -> [ x ]
                | SCC.Has_loop l -> l
              in
              let free_variables =
                Var.Set.elements
                  (List.fold_left
                     ~f:(fun fv x -> Var.Set.remove x fv)
                     ~init:
                       (List.fold_left
                          ~f:(fun fv x -> Var.Set.union fv (Var.Map.find x free_vars))
                          ~init:Var.Set.empty
                          functions)
                     functions)
              in
              let functions =
                let arities =
                  List.fold_left
                    ~f:(fun m (f, a, _) -> Var.Map.add f a m)
                    ~init:Var.Map.empty
                    l
                in
                List.map
                  ~f:(fun f -> f, Var.Map.find f arities)
                  (List.sort ~cmp:Var.compare functions)
              in
              if List.length functions > 1
              then (
                Format.eprintf "AAA";
                List.iter
                  ~f:(fun (f, _) ->
                    Format.eprintf " %a" Code.Var.print f;
                    Format.eprintf "@.")
                  functions);
              List.fold_left
                ~f:(fun closures (f, _) ->
                  Var.Map.add f { functions; free_variables } closures)
                ~init:closures
                functions)
            ~init:closures
            components)
        ~init:closures
        (collect_closures [] [] block.body))
    pc
    program.blocks
    closures

let f p =
  let t = Timer.make () in
  let nv = Var.count () in
  let var_depth = Array.make nv (-1) in
  let res = traverse var_depth p p.start 0 Var.Map.empty in
  if Debug.find "times" () then Format.eprintf "  closure conversion: %a@." Timer.print t;
  res

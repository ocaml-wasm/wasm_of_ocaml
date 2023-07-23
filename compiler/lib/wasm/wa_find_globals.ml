let f globals { Code.start; blocks; _ } =
  Code.traverse
    { fold = Code.fold_children }
    (fun pc s ->
      let b = Code.Addr.Map.find pc blocks in
      List.fold_left
        (fun s (i, _) ->
          match i with
          | Code.Let (x, Block (_, a, _)) when Code.Var.Set.mem x globals ->
              Array.fold_right Code.Var.Set.add a s
          | _ -> s)
        s
        b.body)
    start
    blocks
    Code.Var.Set.empty

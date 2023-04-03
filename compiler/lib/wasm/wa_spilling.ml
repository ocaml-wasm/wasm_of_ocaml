open! Stdlib
open Code

module Domain = struct
  type t =
    | Bot
    | Set of
        { input : Var.Set.t
        ; output : Var.Set.t
        }

  let bot = Bot

  let equal v v' =
    match v, v' with
    | Bot, Bot -> true
    | Bot, Set _ | Set _, Bot -> false
    | Set { input; _ }, Set { input = input'; _ } -> Var.Set.equal input input'
end

let make_table l =
  let h = Hashtbl.create 16 in
  List.iter ~f:(fun s -> Hashtbl.add h s ()) l;
  h

let no_alloc_tbl =
  make_table
    [ "caml_array_unsafe_set"
    ; "caml_string_unsafe_get"
    ; "caml_string_unsafe_set"
    ; "caml_bytes_unsafe_get"
    ; "caml_bytes_unsafe_set"
    ; "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%int_neg"
    ; "%int_or"
    ; "%int_and"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ]

let no_pointer_tbl =
  make_table
    [ "caml_string_unsafe_get"
    ; "caml_string_unsafe_set"
    ; "caml_bytes_unsafe_get"
    ; "caml_bytes_unsafe_set"
    ; "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%int_neg"
    ; "%int_or"
    ; "%int_and"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ]

let no_alloc p =
  match p with
  | Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult -> true
  | Extern nm -> Hashtbl.mem no_alloc_tbl nm (* ZZZ Refine *)

let no_pointer p =
  match p with
  | Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult -> true
  | Extern nm -> Hashtbl.mem no_pointer_tbl nm (* ZZZ Refine *)
  | Array_get -> false

(*ZZZ from wa_generate *)
let get_free_variables ~context info =
  List.filter
    ~f:(fun x -> not (Hashtbl.mem context.Wa_code_generation.constants x))
    info.Wa_closure_conversion.free_variables

let function_free_variables ~context ~closures x =
  let info = Code.Var.Map.find x closures in
  let f, _ = List.hd info.Wa_closure_conversion.functions in
  if Code.Var.equal x f then get_free_variables ~context info else []

let get_set h x = try Hashtbl.find h x with Not_found -> Addr.Set.empty

let get_list h x = try Hashtbl.find h x with Not_found -> []

let cont_deps (deps, rev_deps) pc ?exn (pc', _) =
  Hashtbl.replace deps pc (Addr.Set.add pc' (get_set deps pc));
  Hashtbl.replace rev_deps pc' ((pc, exn) :: get_list rev_deps pc')

let block_deps vars deps block pc =
  match fst block.branch with
  | Return _ | Raise _ | Stop -> ()
  | Branch cont | Poptrap cont -> cont_deps deps pc cont
  | Cond (_, cont1, cont2) ->
      cont_deps deps pc cont1;
      cont_deps deps pc cont2
  | Switch (_, a1, a2) ->
      Array.iter a1 ~f:(fun cont -> cont_deps deps pc cont);
      Array.iter a2 ~f:(fun cont -> cont_deps deps pc cont)
  | Pushtrap (cont, exn, cont_h, _) ->
      cont_deps deps pc cont;
      vars := Var.Set.add exn !vars;
      cont_deps deps pc ~exn cont_h

let function_deps blocks ~context ~closures pc params =
  let vars = ref params in
  let domain = ref Addr.Set.empty in
  let deps = Hashtbl.create 16, Hashtbl.create 16 in
  Code.traverse
    { fold = fold_children }
    (fun pc () ->
      domain := Addr.Set.add pc !domain;
      let block = Addr.Map.find pc blocks in
      vars :=
        List.fold_left
          ~f:(fun vars (i, _) ->
            match i with
            | Let (x, e) -> (
                match e with
                | Constant _ -> vars
                | Prim (p, _) -> if no_pointer p then vars else Var.Set.add x vars
                | Closure _ ->
                    if List.is_empty (function_free_variables ~context ~closures x)
                    then vars
                    else Var.Set.add x vars
                | Apply _ | Block _ | Field _ -> Var.Set.add x vars)
            | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> vars)
          ~init:!vars
          block.body;
      vars := Var.Set.union !vars (Var.Set.of_list block.params);
      block_deps vars deps block pc)
    pc
    blocks
    ();
  !domain, deps, !vars

let inter s s' =
  match s, s' with
  | None, None -> None
  | _, None -> s
  | None, _ -> s'
  | Some s, Some s' -> Some (Var.Set.inter s s')

let propagate_through_expr ~context ~closures s x e =
  match e with
  | Apply _ | Block _ -> Var.Set.empty
  | Prim (p, _) -> if no_alloc p then s else Var.Set.empty
  | Closure _ ->
      if List.is_empty (function_free_variables ~context ~closures x)
      then s
      else Var.Set.empty
  | Constant _ | Field _ -> s

let propagate_through_instr ~context ~closures s (i, _) =
  match i with
  | Let (x, e) -> Var.Set.add x (propagate_through_expr ~context ~closures s x e)
  | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> s

let propagate blocks ~context ~closures rev_deps pc0 params st pc =
  let input =
    pc
    |> get_list rev_deps
    |> List.map ~f:(fun (pc', exn_opt) ->
           match Addr.Map.find pc' st with
           | Domain.Bot -> None
           | Set { output; _ } ->
               Some
                 (match exn_opt with
                 | None -> output
                 | Some x -> Var.Set.add x output))
    |> List.fold_left ~f:inter ~init:None
  in
  let input = if pc = pc0 then inter input (Some params) else input in
  match input with
  | None -> Domain.Bot
  | Some input ->
      let b = Addr.Map.find pc blocks in
      let input = Var.Set.union input (Var.Set.of_list b.params) in
      let output =
        List.fold_left
          ~f:(fun s i -> propagate_through_instr ~context ~closures s i)
          ~init:input
          b.body
      in
      Set { input; output }

module G = Dgraph.Make (Int) (Addr.Set) (Addr.Map)
module Solver = G.Solver (Domain)

let check_spilled vars loaded x spilled =
  if Var.Set.mem x loaded || not (Var.Set.mem x vars)
  then spilled
  else Var.Set.add x spilled

let spilled_variables blocks context closures domain vars st =
  let spilled = Var.Set.empty in
  Addr.Set.fold
    (fun pc spilled ->
      let loaded =
        match Addr.Map.find pc st with
        | Domain.Bot -> assert false
        | Domain.Set { input; _ } -> input
      in
      let block = Addr.Map.find pc blocks in
      let loaded, spilled =
        List.fold_left
          ~f:(fun (loaded, spilled) i ->
            let loaded' = propagate_through_instr ~context ~closures loaded i in
            let spilled =
              match fst i with
              | Let (x, e) -> (
                  match e with
                  | Apply { f; args; _ } ->
                      List.fold_left
                        ~f:(fun spilled x -> check_spilled vars loaded x spilled)
                        (f :: args)
                        ~init:spilled
                  | Block (_, l, _) ->
                      Array.fold_left
                        ~f:(fun spilled x -> check_spilled vars loaded' x spilled)
                        l
                        ~init:spilled
                  | Prim (_, args) ->
                      List.fold_left
                        ~f:(fun spilled x ->
                          match x with
                          | Pv x -> check_spilled vars loaded x spilled
                          | Pc _ -> spilled)
                        args
                        ~init:spilled
                  | Closure _ ->
                      let fv = function_free_variables ~context ~closures x in
                      List.fold_left
                        ~f:(fun spilled x -> check_spilled vars loaded x spilled)
                        fv
                        ~init:spilled
                  | Constant _ -> spilled
                  | Field (x, _) -> check_spilled vars loaded x spilled)
              | Assign (_, x) | Offset_ref (x, _) -> check_spilled vars loaded x spilled
              | Set_field (x, _, y) ->
                  spilled |> check_spilled vars loaded x |> check_spilled vars loaded y
              | Array_set (x, y, z) ->
                  spilled
                  |> check_spilled vars loaded x
                  |> check_spilled vars loaded y
                  |> check_spilled vars loaded z
            in
            loaded', spilled)
          ~init:(loaded, spilled)
          block.body
      in
      let handle_cont (_, args) spilled =
        List.fold_left
          ~f:(fun spilled x -> check_spilled vars loaded x spilled)
          args
          ~init:spilled
      in
      match fst block.branch with
      | Return x | Raise (x, _) -> check_spilled vars loaded x spilled
      | Stop -> spilled
      | Branch cont | Poptrap cont -> handle_cont cont spilled
      | Cond (_, cont1, cont2) -> spilled |> handle_cont cont1 |> handle_cont cont2
      | Switch (_, a1, a2) ->
          let spilled = Array.fold_right a1 ~f:handle_cont ~init:spilled in
          Array.fold_right a2 ~f:handle_cont ~init:spilled
      | Pushtrap (cont, _, cont_h, _) -> spilled |> handle_cont cont |> handle_cont cont_h)
    domain
    spilled

let f { blocks; _ } context closures pc0 params =
  let params = Var.Set.of_list params in
  let domain, (deps, rev_deps), vars =
    function_deps blocks ~context ~closures pc0 params
  in
  let fold_children f pc acc = Addr.Set.fold f (get_set deps pc) acc in
  let g = { G.domain; fold_children } in
  let st =
    Solver.f g (fun st pc ->
        propagate blocks ~context ~closures rev_deps pc0 params st pc)
  in
  let sv = spilled_variables blocks context closures domain vars st in
  Format.eprintf "SPILLED:";
  Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) sv;
  Format.eprintf "@.";
  Addr.Set.iter
    (fun pc ->
      let s = Addr.Map.find pc st in
      (match s with
      | Domain.Bot -> ()
      | Domain.Set { input; output } ->
          Format.eprintf "INPUT:";
          Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) input;
          Format.eprintf "@.";
          Format.eprintf "OUTPUT:";
          Var.Set.iter (fun x -> Format.eprintf " %a" Var.print x) output;
          Format.eprintf "@.");
      let block = Addr.Map.find pc blocks in
      Code.Print.block (fun _ _ -> "") pc block)
    domain;
  ignore st

(*
TODO:
stack structure / spilling / reload
*)

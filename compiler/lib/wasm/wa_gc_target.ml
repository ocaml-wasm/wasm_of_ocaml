open! Stdlib
module W = Wa_ast
open Wa_code_generation

type expression = Wa_ast.expression Wa_code_generation.t

module Value = struct
  let value = W.Ref Eq

  let unit = return (W.I31New (Const (I32 0l)))

  let val_int = Arith.to_int31

  let int_val = Arith.of_int31

  let is_not_zero i =
    let* i = i in
    return (W.UnOp (I32 Eqz, RefEq (i, W.I31New (Const (I32 0l)))))

  let not = Arith.eqz

  let binop op i i' = val_int (op (int_val i) (int_val i'))

  let lt = binop Arith.( < )

  let le = binop Arith.( <= )

  let eq i i' =
    let* i = i in
    let* i' = i' in
    val_int (return (W.RefEq (i, i')))

  let neq i i' =
    let* i = i in
    let* i' = i' in
    val_int (Arith.eqz (return (W.RefEq (i, i'))))

  let ult = binop Arith.(ult)

  let is_int i =
    let* i = i in
    val_int (return (W.RefTest (I31, i)))

  let int_add = binop Arith.( + )

  let int_sub = binop Arith.( - )

  let int_mul = binop Arith.( * )

  let int_neg i = val_int Arith.(const 0l - int_val i)

  let int_or = binop Arith.( lor )

  let int_and = binop Arith.( land )

  let int_xor = binop Arith.( lxor )

  let int_lsl = binop Arith.( lsl )

  let int_lsr = binop Arith.( lsr )

  let int_asr = binop Arith.( asr )
end

module Memory = struct
  let block : W.symbol = S "toto"

  (*ZZZ Move*)
  let expression_list f l =
    let rec loop acc l =
      match l with
      | [] -> return (List.rev acc)
      | x :: r ->
          let* x = f x in
          loop (x :: acc) r
    in
    loop [] l

  let allocate ~tag l =
    let* l =
      expression_list
        (fun v ->
          match v with
          | `Var y -> load y
          | `Expr e -> return e)
        l
    in
    return
      (W.ArrayNewFixed
         (block, List.length l + 1, I31New (Const (I32 (Int32.of_int tag))) :: l))
  (*ZZZ Float array?*)

  let wasm_array_get e e' = return (W.ArrayGet (None, block, RefCast (Type block, e), e'))

  let wasm_array_set e e' e'' =
    instr (W.ArraySet (None, block, RefCast (Type block, e), e', e''))

  let tag e =
    let* e = e in
    wasm_array_get e (Const (I32 0l))

  let block_length e =
    let* e = e in
    Value.int_val (return (W.ArrayLength e))

  let array_get e e' =
    let* e = e in
    let* offset = Arith.(Value.int_val e' + const 1l) in
    wasm_array_get e offset

  let array_set e e' e'' =
    let* e = e in
    let* offset = Arith.(Value.int_val e' + const 1l) in
    let* e'' = e'' in
    wasm_array_set e offset e''

  let field e idx =
    let* e = e in
    wasm_array_get e (W.Const (I32 (Int32.of_int (idx + 1))))

  let set_field e idx e' =
    let* e = e in
    let* e' = e' in
    wasm_array_set e (W.Const (I32 (Int32.of_int (idx + 1)))) e'

  let load_function_pointer ~arity closure =
    let* closure = closure in
    return
      (W.StructGet
         ( None
         , S "closure"
         , (if arity = 1 then 1 else 2)
         , RefCast (Type (S "closure"), closure) ))

  let header ?const:_ ~tag:_ ~len:_ () = 0l (*ZZZ*)
end

module Constant = struct
  open Wa_core_target (*ZZZ*)

  let rec translate_rec constant_data c =
    match c with
    | Code.Int i -> W.DataI32 Int32.(add (add i i) 1l)
    | Tuple (tag, a, _) ->
        let h = Memory.header ~const:true ~tag ~len:(Array.length a) () in
        let name = Code.Var.fresh_n "block" in
        let block =
          W.DataI32 h
          :: List.map ~f:(fun c -> translate_rec constant_data c) (Array.to_list a)
        in
        constant_data := Code.Var.Map.add name block !constant_data;
        W.DataSym (V name, 4)
    | NativeString (Byte s | Utf (Utf8 s)) | String s ->
        let l = String.length s in
        let len = (l + 4) / 4 in
        let h = Memory.header ~const:true ~tag:Obj.string_tag ~len () in
        let name = Code.Var.fresh_n "str" in
        let extra = (4 * len) - l - 1 in
        let string =
          W.DataI32 h
          :: DataBytes s
          :: (if extra = 0 then [ DataI8 0 ] else [ DataSpace extra; DataI8 extra ])
        in
        constant_data := Code.Var.Map.add name string !constant_data;
        W.DataSym (V name, 4)
    | Float f ->
        let h = Memory.header ~const:true ~tag:Obj.double_tag ~len:2 () in
        let name = Code.Var.fresh_n "float" in
        let block = [ W.DataI32 h; DataI64 (Int64.bits_of_float f) ] in
        constant_data := Code.Var.Map.add name block !constant_data;
        W.DataSym (V name, 4)
    | Float_array l ->
        (*ZZZ Boxed array? *)
        let l = Array.to_list l in
        let h =
          Memory.header ~const:true ~tag:Obj.double_array_tag ~len:(List.length l) ()
        in
        let name = Code.Var.fresh_n "float_array" in
        let block =
          W.DataI32 h :: List.map ~f:(fun f -> translate_rec constant_data (Float f)) l
        in
        constant_data := Code.Var.Map.add name block !constant_data;
        W.DataSym (V name, 4)
    | Int64 i ->
        let h = Memory.header ~const:true ~tag:Obj.custom_tag ~len:3 () in
        let name = Code.Var.fresh_n "int64" in
        let block = [ W.DataI32 h; DataSym (S "caml_int64_ops", 0); DataI64 i ] in
        constant_data := Code.Var.Map.add name block !constant_data;
        W.DataSym (V name, 4)

  let translate c =
    let* constant_data = get_constant_data_table in
    return
      (match translate_rec constant_data c with
      | W.DataSym (V name, offset) -> W.ConstSym (V name, offset)
      | W.DataI32 i -> W.I31New (Const (I32 i))
      | _ -> assert false)
end

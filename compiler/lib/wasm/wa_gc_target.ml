open! Stdlib
module W = Wa_ast
open Wa_code_generation

type expression = Wa_ast.expression Wa_code_generation.t

module Value = struct
  let value = W.Ref Eq

  let block_type = register_type "block" (W.Array { mut = true; typ = Value value })

  let string_type = register_type "string" (W.Array { mut = true; typ = Packed I8 })

  let float_type = register_type "float" (W.Struct [ { mut = true; typ = Value F64 } ])

  let int64_type = register_type "float" (W.Struct [ { mut = true; typ = Value I64 } ])

  let closure_type =
    register_type
      "closure"
      (W.Struct [ { mut = false; typ = Value I32 }; { mut = false; typ = Value value } ])

  let unit = return (W.I31New (Const (I32 0l)))

  let val_int = Arith.to_int31

  let int_val = Arith.of_int31

  let check_is_not_zero i =
    let* i = i in
    return (W.UnOp (I32 Eqz, RefEq (i, W.I31New (Const (I32 0l)))))

  let check_is_int i =
    let* i = i in
    return (W.RefTest (I31, i))

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
  (*ZZZ Move?*)
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
    let* ty = Value.block_type in
    return (W.ArrayNewFixed (ty, I31New (Const (I32 (Int32.of_int tag))) :: l))
  (*ZZZ Float array?*)

  let wasm_array_get e e' =
    let* ty = Value.block_type in
    return (W.ArrayGet (None, ty, RefCast (Type ty, e), e'))

  let wasm_array_set e e' e'' =
    let* ty = Value.block_type in
    instr (W.ArraySet (None, ty, RefCast (Type ty, e), e', e''))

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
    let* ty = Value.closure_type in
    return
      (W.StructGet (None, ty, (if arity = 1 then 1 else 2), RefCast (Type ty, closure)))

  let header ?const:_ ~tag:_ ~len:_ () = 0l (*ZZZ*)
end

module Constant = struct
  let rec translate_rec c =
    match c with
    | Code.Int i -> return (W.I31New (Const (I32 i))) (*ZZZ 32 bit integers *)
    | Tuple (tag, a, _) ->
        let* ty = Value.block_type in
        let* l =
          Array.fold_left
            ~f:(fun prev c ->
              let* acc = prev in
              let* c = translate_rec c in
              return (c :: acc))
            ~init:(return [])
            a
        in
        return (W.ArrayNewFixed (ty, I31New (Const (I32 (Int32.of_int tag))) :: l))
    | NativeString (Byte s | Utf (Utf8 s)) | String s ->
        let* ty = Value.string_type in
        (*ZZZ Use this for long strings
          let name = Code.Var.fresh_n "string" in
          let* () = register_data_segment name [ DataBytes s ] in
          return
            (W.ArrayNewData
               (ty, name, Const (I32 0l), Const (I32 (Int32.of_int (String.length s)))))
        *)
        let l =
          String.fold_right
            ~f:(fun c r -> W.Const (I32 (Int32.of_int (Char.code c))) :: r)
            s
            ~init:[]
        in
        return (W.ArrayNewFixed (ty, l))
    | Float f ->
        let* ty = Value.float_type in
        return (W.StructNew (ty, [ Const (F64 f) ]))
    | Float_array l ->
        let l = Array.to_list l in
        let* bl_ty = Value.block_type in
        let* ty = Value.float_type in
        (*ZZZ Boxed array? *)
        return
          (W.ArrayNewFixed
             ( bl_ty
             , I31New (Const (I32 (Int32.of_int Obj.double_array_tag)))
               :: List.map ~f:(fun f -> W.StructNew (ty, [ Const (F64 f) ])) l ))
    | Int64 i ->
        let* ty = Value.int64_type in
        return (W.StructNew (ty, [ Const (I64 i) ]))

  let translate c =
    let* c = translate_rec c in
    match c with
    | W.I31New _ -> return c
    | _ ->
        let name = Code.Var.fresh_n "const" in
        let* () = register_global (V name) { mut = false; typ = Value.value } c in
        return (W.GlobalGet (V name))
end

let entry_point ~register_primitive:_ = return ()

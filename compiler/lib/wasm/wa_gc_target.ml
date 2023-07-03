open! Stdlib
module W = Wa_ast
open Wa_code_generation

type expression = Wa_ast.expression Wa_code_generation.t

let include_closure_arity = false

module Type = struct
  let value = W.Ref { nullable = false; typ = Eq }

  let block_type =
    register_type "block" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Array { mut = true; typ = Value value }
          })

  let string_type =
    register_type "string" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Array { mut = true; typ = Packed I8 }
          })

  let float_type =
    register_type "float" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Struct [ { mut = false; typ = Value F64 } ]
          })

  let compare_type =
    register_type "compare" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value; value ]; result = [ I32 ] }
          })

  let hash_type =
    register_type "hash" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value ]; result = [ I32 ] }
          })

  let custom_operations_type =
    register_type "custom_operations" (fun () ->
        let* string = string_type in
        let* compare = compare_type in
        let* hash = hash_type in
        return
          { supertype = None
          ; final = true
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type string })
                  }
                ; { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type compare })
                  }
                ; { mut = false; typ = Value (Ref { nullable = true; typ = Type hash }) }
                ]
          })

  let custom_type =
    register_type "custom" (fun () ->
        let* custom_operations = custom_operations_type in
        return
          { supertype = None
          ; final = false
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type custom_operations })
                  }
                ]
          })

  let int32_type =
    register_type "int32" (fun () ->
        let* custom_operations = custom_operations_type in
        let* custom = custom_type in
        return
          { supertype = Some custom
          ; final = false
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type custom_operations })
                  }
                ; { mut = false; typ = Value I32 }
                ]
          })

  let int64_type =
    register_type "int64" (fun () ->
        let* custom_operations = custom_operations_type in
        let* custom = custom_type in
        return
          { supertype = Some custom
          ; final = false
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type custom_operations })
                  }
                ; { mut = false; typ = Value I64 }
                ]
          })

  let func_type n =
    { W.params = List.init ~len:(n + 1) ~f:(fun _ -> value); result = [ value ] }

  let function_type n =
    register_type (Printf.sprintf "function_%d" n) (fun () ->
        return { supertype = None; final = true; typ = W.Func (func_type n) })

  let closure_common_fields =
    let* fun_ty = function_type 1 in
    return
      (let function_pointer =
         [ { W.mut = false; typ = W.Value (Ref { nullable = false; typ = Type fun_ty }) }
         ]
       in
       if include_closure_arity
       then { W.mut = false; typ = W.Value I32 } :: function_pointer
       else function_pointer)

  let closure_type_1 =
    register_type "closure" (fun () ->
        let* fields = closure_common_fields in
        return { supertype = None; final = false; typ = W.Struct fields })

  let closure_last_arg_type =
    register_type "closure_last_arg" (fun () ->
        let* cl_typ = closure_type_1 in
        let* fields = closure_common_fields in
        return { supertype = Some cl_typ; final = false; typ = W.Struct fields })

  let closure_type ~usage arity =
    if arity = 1
    then
      match usage with
      | `Alloc -> closure_last_arg_type
      | `Access -> closure_type_1
    else
      register_type (Printf.sprintf "closure_%d" arity) (fun () ->
          let* cl_typ = closure_type_1 in
          let* common = closure_common_fields in
          let* fun_ty' = function_type arity in
          return
            { supertype = Some cl_typ
            ; final = false
            ; typ =
                W.Struct
                  (common
                  @ [ { mut = false
                      ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                      }
                    ])
            })

  let env_type ~arity n =
    register_type (Printf.sprintf "env_%d_%d" arity n) (fun () ->
        let* cl_typ = closure_type ~usage:`Alloc arity in
        let* common = closure_common_fields in
        let* fun_ty' = function_type arity in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                ((if arity = 1
                  then common
                  else
                    common
                    @ [ { mut = false
                        ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                        }
                      ])
                @ List.init
                    ~f:(fun _ ->
                      { W.mut = false
                      ; typ = W.Value (Ref { nullable = false; typ = Eq })
                      })
                    ~len:n)
          })

  let rec_env_type ~function_count ~free_variable_count =
    register_type
      (Printf.sprintf "rec_env_%d_%d" function_count free_variable_count)
      (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ =
              W.Struct
                (List.init
                   ~f:(fun i ->
                     { W.mut = i < function_count
                     ; typ = W.Value (Ref { nullable = false; typ = Eq })
                     })
                   ~len:(function_count + free_variable_count))
          })

  let rec_closure_type ~arity ~function_count ~free_variable_count =
    register_type
      (Printf.sprintf "closure_rec_%d_%d_%d" arity function_count free_variable_count)
      (fun () ->
        let* cl_typ = closure_type ~usage:`Alloc arity in
        let* common = closure_common_fields in
        let* fun_ty' = function_type arity in
        let* env_ty = rec_env_type ~function_count ~free_variable_count in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                ((if arity = 1
                  then common
                  else
                    common
                    @ [ { mut = false
                        ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                        }
                      ])
                @ [ { W.mut = false
                    ; typ = W.Value (Ref { nullable = false; typ = Type env_ty })
                    }
                  ])
          })

  let rec curry_type arity m =
    register_type (Printf.sprintf "curry_%d_%d" arity m) (fun () ->
        let* cl_typ = closure_type ~usage:(if m = 2 then `Alloc else `Access) 1 in
        let* common = closure_common_fields in
        let* cl_ty =
          if m = arity then closure_type ~usage:`Alloc arity else curry_type arity (m + 1)
        in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                (common
                @ [ { mut = false
                    ; typ = Value (Ref { nullable = false; typ = Type cl_ty })
                    }
                  ; { W.mut = false; typ = Value value }
                  ])
          })

  let dummy_closure_type ~arity =
    register_type (Printf.sprintf "dummy_closure_%d" arity) (fun () ->
        let* cl_typ = closure_type ~usage:`Alloc arity in
        let* cl_typ' = closure_type ~usage:`Access arity in
        let* common = closure_common_fields in
        let* fun_ty' = function_type arity in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                ((if arity = 1
                  then common
                  else
                    common
                    @ [ { mut = false
                        ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                        }
                      ])
                @ [ { W.mut = true
                    ; typ = W.Value (Ref { nullable = true; typ = Type cl_typ' })
                    }
                  ])
          })
end

module Value = struct
  let value = Type.value

  let unit = return (W.I31New (Const (I32 0l)))

  let val_int = Arith.to_int31

  let int_val i = Arith.of_int31 (cast I31 i)

  let check_is_not_zero i =
    let* i = i in
    return (W.UnOp (I32 Eqz, RefEq (i, W.I31New (Const (I32 0l)))))

  let check_is_int i =
    let* i = i in
    return (W.RefTest ({ nullable = false; typ = I31 }, i))

  let not i = val_int (Arith.eqz (int_val i))

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
    val_int (return (W.RefTest ({ nullable = false; typ = I31 }, i)))

  let int_add = binop Arith.( + )

  let int_sub = binop Arith.( - )

  let int_mul = binop Arith.( * )

  let int_div = binop Arith.( / )

  let int_mod = binop Arith.( mod )

  let int_neg i = val_int Arith.(const 0l - int_val i)

  let int_or = binop Arith.( lor )

  let int_and = binop Arith.( land )

  let int_xor = binop Arith.( lxor )

  let int_lsl = binop Arith.( lsl )

  let int_lsr i i' = val_int Arith.((int_val i land const 0x7fffffffl) lsr int_val i')

  let int_asr = binop Arith.( asr )
end

module Memory = struct
  let allocate _ _ ~tag l =
    let* l =
      expression_list
        (fun v ->
          match v with
          | `Var y -> load y
          | `Expr e -> return e)
        l
    in
    let* ty = Type.block_type in
    return (W.ArrayNewFixed (ty, I31New (Const (I32 (Int32.of_int tag))) :: l))
  (*ZZZ Float array?*)

  let wasm_cast ty e =
    let* e = e in
    return (W.RefCast ({ nullable = false; typ = Type ty }, e))

  let wasm_struct_get ty e i =
    let* e = e in
    match e with
    | W.RefCast (_, GlobalGet nm) -> (
        let* init = get_global nm in
        match init with
        | Some (W.StructNew (_, l)) ->
            let e = List.nth l i in
            let* b = is_small_constant e in
            if b then return e else return (W.StructGet (None, ty, i, e))
        | _ -> return (W.StructGet (None, ty, i, e)))
    | _ -> return (W.StructGet (None, ty, i, e))

  let wasm_struct_set ty e i e' =
    let* e = e in
    let* e' = e' in
    instr (W.StructSet (ty, i, e, e'))

  let wasm_array_get ?(ty = Type.block_type) e e' =
    let* ty = ty in
    let* e = wasm_cast ty e in
    let* e' = e' in
    return (W.ArrayGet (None, ty, e, e'))

  let wasm_array_set ?(ty = Type.block_type) e e' e'' =
    let* ty = ty in
    let* e = wasm_cast ty e in
    let* e' = e' in
    let* e'' = e'' in
    instr (W.ArraySet (ty, e, e', e''))

  let tag e = Value.int_val (wasm_array_get e (Arith.const 0l))

  let block_length e =
    let* ty = Type.block_type in
    let* e = wasm_cast ty e in
    Arith.(return (W.ArrayLen e) - const 1l)

  let array_get e e' = wasm_array_get e Arith.(Value.int_val e' + const 1l)

  let array_set e e' e'' = wasm_array_set e Arith.(Value.int_val e' + const 1l) e''

  let bytes_length e =
    let* ty = Type.string_type in
    let* e = wasm_cast ty e in
    return (W.ArrayLen e)

  let bytes_get e e' =
    Value.val_int (wasm_array_get ~ty:Type.string_type e (Value.int_val e'))

  let bytes_set e e' e'' =
    wasm_array_set ~ty:Type.string_type e (Value.int_val e') (Value.int_val e'')

  let field e idx = wasm_array_get e (Arith.const (Int32.of_int (idx + 1)))

  let set_field e idx e' = wasm_array_set e (Arith.const (Int32.of_int (idx + 1))) e'

  let env_start arity =
    (if include_closure_arity then 1 else 0) + if arity = 1 then 1 else 2

  let load_function_pointer ~arity ?(skip_cast = false) closure =
    let* ty = Type.closure_type ~usage:`Access arity in
    let* fun_ty = Type.function_type arity in
    let casted_closure = if skip_cast then closure else wasm_cast ty closure in
    let* e = wasm_struct_get ty casted_closure (env_start arity - 1) in
    return (`Ref fun_ty, e)

  let load_real_closure ~arity closure =
    let* ty = Type.dummy_closure_type ~arity in
    let* cl_typ = Type.closure_type ~usage:`Access arity in
    let* e =
      wasm_cast cl_typ (wasm_struct_get ty (wasm_cast ty closure) (env_start arity))
    in
    return (cl_typ, e)

  let check_function_arity f arity if_match if_mismatch =
    let* fun_ty = Type.closure_type ~usage:`Access arity in
    let* closure = load f in
    let* () =
      drop
        (block_expr
           { params = []; result = [ Value.value ] }
           (let* e =
              if_match
                ~typ:(Some (W.Ref { nullable = false; typ = Type fun_ty }))
                (return
                   (W.Br_on_cast_fail
                      ( 0
                      , { nullable = false; typ = Eq }
                      , { nullable = false; typ = Type fun_ty }
                      , closure )))
            in
            instr (W.Return (Some e))))
    in
    if_mismatch

  let box_float _ _ e =
    let* ty = Type.float_type in
    let* e = e in
    return (W.StructNew (ty, [ e ]))

  let unbox_float e =
    let* ty = Type.float_type in
    wasm_struct_get ty (wasm_cast ty e) 0

  let make_int32 ~kind e =
    let* custom_operations = Type.custom_operations_type in
    let* int32_ops =
      register_import
        ~name:
          (match kind with
          | `Int32 -> "int32_ops"
          | `Nativeint -> "nativeint_ops")
        (Global
           { mut = false; typ = Ref { nullable = false; typ = Type custom_operations } })
    in
    let* ty = Type.int32_type in
    let* e = e in
    return (W.StructNew (ty, [ GlobalGet int32_ops; e ]))

  let box_int32 _ _ e = make_int32 ~kind:`Int32 e

  let unbox_int32 e =
    let* ty = Type.int32_type in
    wasm_struct_get ty (wasm_cast ty e) 1

  let make_int64 e =
    let* custom_operations = Type.custom_operations_type in
    let* int64_ops =
      register_import
        ~name:"int64_ops"
        (Global
           { mut = false; typ = Ref { nullable = false; typ = Type custom_operations } })
    in
    let* ty = Type.int64_type in
    let* e = e in
    return (W.StructNew (ty, [ GlobalGet int64_ops; e ]))

  let box_int64 _ _ e = make_int64 e

  let unbox_int64 e =
    let* ty = Type.int64_type in
    wasm_struct_get ty (wasm_cast ty e) 1

  let box_nativeint _ _ e = make_int32 ~kind:`Nativeint e

  let unbox_nativeint e =
    let* ty = Type.int32_type in
    wasm_struct_get ty (wasm_cast ty e) 1
end

module Constant = struct
  let string_length_threshold = 100

  let store_in_global c =
    let name = Code.Var.fresh_n "const" in
    let* () = register_global name { mut = false; typ = Type.value } c in
    return (W.GlobalGet name)

  let rec translate_rec c =
    match c with
    | Code.Int (Regular, i) -> return (true, W.I31New (Const (I32 i)))
    | Tuple (tag, a, _) ->
        let* ty = Type.block_type in
        let* l =
          Array.fold_left
            ~f:(fun prev c ->
              let* acc = prev in
              let* c = translate_rec c in
              return (c :: acc))
            ~init:(return [])
            a
        in
        let l = List.rev l in
        let l' =
          List.map ~f:(fun (const, v) -> if const then v else W.I31New (Const (I32 0l))) l
        in
        let c = W.ArrayNewFixed (ty, I31New (Const (I32 (Int32.of_int tag))) :: l') in
        if List.exists ~f:(fun (const, _) -> not const) l
        then
          let* c = store_in_global c in
          let* () =
            register_init_code
              (snd
                 (List.fold_left
                    ~f:(fun (i, before) (const, v) ->
                      ( i + 1
                      , let* () = before in
                        if const
                        then return ()
                        else
                          Memory.wasm_array_set
                            (return c)
                            (Arith.const (Int32.of_int i))
                            (return v) ))
                    ~init:(1, return ())
                    l))
          in
          return (true, c)
        else return (true, c)
    | NativeString (Byte s | Utf (Utf8 s)) | String s ->
        let* ty = Type.string_type in
        if String.length s > string_length_threshold
        then
          let name = Code.Var.fresh_n "string" in
          let* () = register_data_segment name ~active:false [ DataBytes s ] in
          return
            ( false
            , W.ArrayNewData
                (ty, name, Const (I32 0l), Const (I32 (Int32.of_int (String.length s))))
            )
        else
          let l =
            String.fold_right
              ~f:(fun c r -> W.Const (I32 (Int32.of_int (Char.code c))) :: r)
              s
              ~init:[]
          in
          return (true, W.ArrayNewFixed (ty, l))
    | Float f ->
        let* ty = Type.float_type in
        return (true, W.StructNew (ty, [ Const (F64 f) ]))
    | Float_array l ->
        let l = Array.to_list l in
        let* bl_ty = Type.block_type in
        let* ty = Type.float_type in
        (*ZZZ Boxed array? *)
        return
          ( true
          , W.ArrayNewFixed
              ( bl_ty
              , I31New (Const (I32 (Int32.of_int Obj.double_array_tag)))
                :: List.map ~f:(fun f -> W.StructNew (ty, [ Const (F64 f) ])) l ) )
    | Int64 i ->
        let* e = Memory.make_int64 (return (W.Const (I64 i))) in
        return (true, e)
    | Int (Int32, i) ->
        let* e = Memory.make_int32 ~kind:`Int32 (return (W.Const (I32 i))) in
        return (true, e)
    | Int (Native, i) ->
        let* e = Memory.make_int32 ~kind:`Nativeint (return (W.Const (I32 i))) in
        return (true, e)

  let translate c =
    let* const, c = translate_rec c in
    if const
    then
      let* b = is_small_constant c in
      if b then return c else store_in_global c
    else
      let name = Code.Var.fresh_n "const" in
      let* () =
        register_global
          ~constant:true
          name
          { mut = true; typ = Type.value }
          (W.I31New (Const (I32 0l)))
      in
      let* () = register_init_code (instr (W.GlobalSet (name, c))) in
      return (W.GlobalGet name)
end

module Closure = struct
  let get_free_variables ~context info =
    List.filter
      ~f:(fun x -> not (Hashtbl.mem context.constants x))
      info.Wa_closure_conversion.free_variables

  let rec is_last_fun l f =
    match l with
    | [] -> false
    | [ (g, _) ] -> Code.Var.equal f g
    | _ :: r -> is_last_fun r f

  let translate ~context ~closures ~stack_ctx:_ f =
    let info = Code.Var.Map.find f closures in
    let free_variables = get_free_variables ~context info in
    let arity = List.assoc f info.functions in
    let* curry_fun = if arity > 1 then need_curry_fun ~arity else return f in
    if List.is_empty free_variables
    then
      let* typ = Type.closure_type ~usage:`Alloc arity in
      let name = Code.Var.fresh_n "closure" in
      let* () =
        register_global
          name
          { mut = false; typ = Type.value }
          (W.StructNew
             ( typ
             , let code_pointers =
                 if arity = 1 then [ W.RefFunc f ] else [ RefFunc curry_fun; RefFunc f ]
               in
               if include_closure_arity
               then Const (I32 (Int32.of_int arity)) :: code_pointers
               else code_pointers ))
      in
      return (W.GlobalGet name)
    else
      let free_variable_count = List.length free_variables in
      match info.Wa_closure_conversion.functions with
      | [] -> assert false
      | [ _ ] ->
          let* typ = Type.env_type ~arity free_variable_count in
          let* l = expression_list load free_variables in
          return
            (W.StructNew
               ( typ
               , (let code_pointers =
                    if arity = 1
                    then [ W.RefFunc f ]
                    else [ RefFunc curry_fun; RefFunc f ]
                  in
                  if include_closure_arity
                  then W.Const (I32 (Int32.of_int arity)) :: code_pointers
                  else code_pointers)
                 @ l ))
      | (g, _) :: _ as functions ->
          let function_count = List.length functions in
          let* env_typ = Type.rec_env_type ~function_count ~free_variable_count in
          let env =
            if Code.Var.equal f g
            then
              let env = Code.Var.fresh () in
              let* () = set_closure_env f env in
              let* l = expression_list load free_variables in
              tee
                ~typ:(W.Ref { nullable = false; typ = Type env_typ })
                env
                (return
                   (W.StructNew
                      ( env_typ
                      , List.init ~len:function_count ~f:(fun _ ->
                            W.I31New (W.Const (I32 0l)))
                        @ l )))
            else
              let* env = get_closure_env g in
              let* () = set_closure_env f env in
              load env
          in
          let* typ = Type.rec_closure_type ~arity ~function_count ~free_variable_count in
          let res =
            let* env = env in
            return
              (W.StructNew
                 ( typ
                 , (let code_pointers =
                      if arity = 1
                      then [ W.RefFunc f ]
                      else [ RefFunc curry_fun; RefFunc f ]
                    in
                    if include_closure_arity
                    then W.Const (I32 (Int32.of_int arity)) :: code_pointers
                    else code_pointers)
                   @ [ env ] ))
          in
          if is_last_fun functions f
          then
            seq
              (snd
                 (List.fold_left
                    ~f:(fun (i, prev) (g, _) ->
                      ( i + 1
                      , let* () = prev in
                        Memory.wasm_struct_set
                          env_typ
                          env
                          i
                          (if Code.Var.equal f g then tee f res else load g) ))
                    ~init:(0, return ())
                    functions))
              (load f)
          else res

  let bind_environment ~context ~closures f =
    if Hashtbl.mem context.constants f
    then
      (* The closures are all constants and the environment is empty. *)
      let* _ = add_var (Code.Var.fresh ()) in
      return ()
    else
      let info = Code.Var.Map.find f closures in
      let free_variables = get_free_variables ~context info in
      let free_variable_count = List.length free_variables in
      let arity = List.assoc f info.functions in
      let offset = Memory.env_start arity in
      match info.Wa_closure_conversion.functions with
      | [ _ ] ->
          let* typ = Type.env_type ~arity free_variable_count in
          let* _ = add_var f in
          (*ZZZ Store env with right type in local variable? *)
          snd
            (List.fold_left
               ~f:(fun (i, prev) x ->
                 ( i + 1
                 , let* () = prev in
                   define_var x Memory.(wasm_struct_get typ (wasm_cast typ (load f)) i) ))
               ~init:(offset, return ())
               free_variables)
      | functions ->
          let function_count = List.length functions in
          let* typ = Type.rec_closure_type ~arity ~function_count ~free_variable_count in
          let* _ = add_var f in
          let env = Code.Var.fresh_n "env" in
          let* env_typ = Type.rec_env_type ~function_count ~free_variable_count in
          let* () =
            store
              ~typ:(W.Ref { nullable = false; typ = Type env_typ })
              env
              Memory.(wasm_struct_get typ (wasm_cast typ (load f)) offset)
          in
          snd
            (List.fold_left
               ~f:(fun (i, prev) x ->
                 ( i + 1
                 , let* () = prev in
                   define_var x Memory.(wasm_struct_get env_typ (load env) i) ))
               ~init:(0, return ())
               (List.map ~f:fst functions @ free_variables))

  let curry_allocate ~stack_ctx:_ ~x:_ ~arity m ~f ~closure ~arg =
    let* ty = Type.curry_type arity m in
    let* cl_ty =
      if m = arity
      then Type.closure_type ~usage:`Alloc arity
      else Type.curry_type arity (m + 1)
    in
    let* closure = Memory.wasm_cast cl_ty (load closure) in
    let* arg = load arg in
    let closure_contents = [ W.RefFunc f; closure; arg ] in
    return
      (W.StructNew
         ( ty
         , if include_closure_arity
           then Const (I32 1l) :: closure_contents
           else closure_contents ))

  let curry_load ~arity m closure =
    let m = m + 1 in
    let* ty = Type.curry_type arity m in
    let* cl_ty =
      if m = arity
      then Type.closure_type ~usage:`Alloc arity
      else Type.curry_type arity (m + 1)
    in
    let cast e = if m = 2 then Memory.wasm_cast ty e else e in
    let offset = Memory.env_start 1 in
    return
      ( Memory.wasm_struct_get ty (cast (load closure)) (offset + 1)
      , Memory.wasm_struct_get ty (cast (load closure)) offset
      , Some (W.Ref { nullable = false; typ = Type cl_ty }) )

  let dummy ~arity =
    (* The runtime only handle function with arity up to 4 *)
    let arity = if arity > 4 then 1 else arity in
    let* dummy_fun = need_dummy_fun ~arity in
    let* ty = Type.dummy_closure_type ~arity in
    let* curry_fun = if arity > 1 then need_curry_fun ~arity else return dummy_fun in
    let* cl_typ = Type.closure_type ~usage:`Alloc arity in
    let closure_contents =
      if arity = 1
      then [ W.RefFunc dummy_fun; RefNull (Type cl_typ) ]
      else [ RefFunc curry_fun; RefFunc dummy_fun; RefNull (Type cl_typ) ]
    in
    return
      (W.StructNew
         ( ty
         , if include_closure_arity
           then Const (I32 1l) :: closure_contents
           else closure_contents ))
end

module Stack = struct
  type stack = Code.Var.t option list

  type info = unit

  let generate_spilling_information _ ~context:_ ~closures:_ ~pc:_ ~env:_ ~params:_ = ()

  let add_spilling _ ~location:_ ~stack:_ ~live_vars:_ ~spilled_vars:_ = (), []

  type ctx = unit

  let start_function ~context:_ _ = ()

  let start_block ~context:_ _ _ = ()

  let perform_reloads _ _ = return ()

  let perform_spilling _ _ = return ()

  let kill_variables _ = ()

  let assign _ _ = return ()

  let make_info () = ()

  let adjust_stack _ ~src:_ ~dst:_ = return ()

  let stack_adjustment_needed _ ~src:_ ~dst:_ = false
end

module Math = struct
  let float_func_type n =
    { W.params = List.init ~len:n ~f:(fun _ : W.value_type -> F64); result = [ F64 ] }

  let unary name x =
    let* f = register_import ~import_module:"Math" ~name (Fun (float_func_type 1)) in
    let* x = x in
    return (W.Call (f, [ x ]))

  let cos f = unary "cos" f

  let sin f = unary "sin" f

  let tan f = unary "tan" f

  let acos f = unary "acos" f

  let asin f = unary "asin" f

  let atan f = unary "atan" f

  let cosh f = unary "cosh" f

  let sinh f = unary "sinh" f

  let tanh f = unary "tanh" f

  let acosh f = unary "acosh" f

  let asinh f = unary "asinh" f

  let atanh f = unary "atanh" f

  let cbrt f = unary "cbrt" f

  let exp f = unary "exp" f

  let expm1 f = unary "expm1" f

  let log f = unary "log" f

  let log1p f = unary "log1p" f

  let log2 f = unary "log2" f

  let log10 f = unary "log10" f

  let binary name x y =
    let* f = register_import ~import_module:"Math" ~name (Fun (float_func_type 2)) in
    let* x = x in
    let* y = y in
    return (W.Call (f, [ x; y ]))

  let atan2 f g = binary "atan2" f g

  let hypot f g = binary "hypot" f g

  let power f g = binary "pow" f g

  let fmod f g = binary "fmod" f g

  let round x =
    let* f = register_import ~name:"caml_round" (Fun (float_func_type 1)) in
    let* x = x in
    return (W.Call (f, [ x ]))

  let exp2 x = power (return (W.Const (F64 2.))) x
end

let post_process_function_body = Wa_initialize_locals.f

let entry_point ~context = init_code context

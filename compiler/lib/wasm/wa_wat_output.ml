open! Stdlib
open Wa_ast

type sexp =
  | Atom of string
  | List of sexp list

let rec format_sexp f s =
  match s with
  | Atom s -> Format.fprintf f "%s" s
  | List l ->
      Format.fprintf f "@[<2>(";
      Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "@ ") format_sexp f l;
      Format.fprintf f ")@]"

let index symb =
  Atom
    ("$"
    ^
    match symb with
    | S s -> s
    | V x -> Code.Var.to_string x)

let value_type (t : value_type) =
  Atom
    (match t with
    | I32 -> "i32"
    | I64 -> "i64"
    | F64 -> "f64")

let list name f l = if List.is_empty l then [] else [ List (Atom name :: f l) ]

let value_type_list name tl = list name (fun tl -> List.map ~f:value_type tl) tl

let funct_type { params; result } =
  value_type_list "param" params @ value_type_list "result" result

let block_type ty =
  match ty with
  | None -> []
  | Some t -> [ List [ Atom "result"; value_type t ] ]

let quoted_name name = Atom ("\"" ^ name ^ "\"")

let export name =
  match name with
  | None -> []
  | Some name -> [ List [ Atom "export"; quoted_name name ] ]

let type_prefix op nm =
  (match op with
  | I32 _ -> "i32."
  | I64 _ -> "i64."
  | F64 _ -> "f64.")
  ^ nm

let int_un_op op =
  match op with
  | Clz -> "clz"
  | Ctz -> "ctz"
  | Popcnt -> "popcnt"
  | Eqz -> "eqz"

let signage op (s : Wa_ast.signage) =
  op
  ^
  match s with
  | S -> "_s"
  | U -> "_"

let int_bin_op (op : int_bin_op) =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div s -> signage "div" s
  | Rem s -> signage "rem" s
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Shr s -> signage "shr" s
  | Rotl -> "rotl"
  | Rotr -> "rotr"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt s -> signage "lt" s
  | Gt s -> signage "gt" s
  | Le s -> signage "le" s
  | Ge s -> signage "ge" s

let float_un_op op =
  match op with
  | Neg -> "neg"
  | Abs -> "abs"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Trunc -> "trunc"
  | Nearest -> "nearest"
  | Sqrt -> "sqrt"

let float_bin_op op =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Min -> "min"
  | Max -> "max"
  | CopySign -> "copysign"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "lt"
  | Gt -> "gt"
  | Le -> "le"
  | Ge -> "ge"

let select i32 i64 f64 op =
  match op with
  | I32 x -> i32 x
  | I64 x -> i64 x
  | F64 x -> f64 x

type ctx =
  { addresses : int Code.Var.Map.t
  ; constants : int StringMap.t
  ; mutable functions : int Code.Var.Map.t
  ; mutable function_count : int
  }

let lookup_symbol ctx symb =
  match symb with
  | S nm -> (
      try StringMap.find nm ctx.constants
      with Not_found ->
        prerr_endline nm;
        assert false)
  | V x -> (
      try Code.Var.Map.find x ctx.addresses
      with Not_found -> (
        try Code.Var.Map.find x ctx.functions
        with Not_found ->
          let i = ctx.function_count in
          ctx.functions <- Code.Var.Map.add x i ctx.functions;
          ctx.function_count <- ctx.function_count + 1;
          i))

let expression_or_instructions ctx =
  let rec expression e =
    match e with
    | Const op ->
        [ List
            [ Atom (type_prefix op "const")
            ; Atom (select Int32.to_string Int64.to_string string_of_float (*ZZZ*) op)
            ]
        ]
    | ConstSym (symb, ofs) ->
        let i = lookup_symbol ctx symb in
        [ List [ Atom "i32.const"; Atom (string_of_int (i + ofs)) ] ]
    | UnOp (op, e') ->
        [ List
            (Atom (type_prefix op (select int_un_op int_un_op float_un_op op))
            :: expression e')
        ]
    | BinOp (op, e1, e2) ->
        [ List
            (Atom (type_prefix op (select int_bin_op int_bin_op float_bin_op op))
            :: (expression e1 @ expression e2))
        ]
    | Load (offset, e') ->
        let offs i =
          if Int32.equal i 0l then [] else [ Atom (Printf.sprintf "offset=%ld" i) ]
        in
        [ List
            ((Atom (type_prefix offset "load") :: select offs offs offs offset)
            @ expression e')
        ]
    | LocalGet i -> [ List [ Atom "local.get"; Atom (string_of_int i) ] ]
    | LocalTee (i, e') ->
        [ List (Atom "local.tee" :: Atom (string_of_int i) :: expression e') ]
    | GlobalGet nm -> [ List [ Atom "global.get"; index (S nm) ] ]
    | Call_indirect (typ, e, l) ->
        [ List
            ((Atom "call_indirect" :: funct_type typ)
            @ List.concat (List.map ~f:expression (l @ [ e ])))
        ]
    | Call (f, l) ->
        [ List (Atom "call" :: index f :: List.concat (List.map ~f:expression l)) ]
    | MemoryGrow (_, e) -> [ List (Atom "memory.grow" :: expression e) ]
    | Seq (l, e) -> List.concat (List.map ~f:instruction l) @ expression e
  and instruction i =
    match i with
    | Drop e -> [ List (Atom "drop" :: expression e) ]
    | Store (offset, e1, e2) ->
        let offs i =
          if Int32.equal i 0l then [] else [ Atom (Printf.sprintf "offset=%ld" i) ]
        in
        [ List
            (Atom (type_prefix offset "store")
            :: (select offs offs offs offset @ expression e1 @ expression e2))
        ]
    | LocalSet (i, e) ->
        [ List (Atom "local.set" :: Atom (string_of_int i) :: expression e) ]
    | GlobalSet (nm, e) -> [ List (Atom "global.set" :: index (S nm) :: expression e) ]
    | Loop (ty, l) -> [ List (Atom "loop" :: (block_type ty @ instructions l)) ]
    | Block (ty, l) -> [ List (Atom "block" :: (block_type ty @ instructions l)) ]
    | If (ty, e, l1, l2) ->
        [ List
            (Atom "if"
            :: (block_type ty
               @ expression e
               @ list "then" instructions l1
               @ list "else" instructions l2))
        ]
    | Try (ty, body, catches, catch_all) ->
        [ List
            (Atom "try"
            :: (block_type ty
               @ List (Atom "do" :: instructions body)
                 :: (List.map
                       ~f:(fun (tag, l) ->
                         List (Atom "catch" :: index (S tag) :: instructions l))
                       catches
                    @
                    match catch_all with
                    | None -> []
                    | Some l -> [ List (Atom "catch_all" :: instructions l) ])))
        ]
    | Br_table (e, l, i) ->
        [ List
            (Atom "br_table"
            :: (List.map ~f:(fun i -> Atom (string_of_int i)) (l @ [ i ]) @ expression e)
            )
        ]
    | Br (i, e) ->
        [ List
            (Atom "br"
            :: Atom (string_of_int i)
            ::
            (match e with
            | None -> []
            | Some e -> expression e))
        ]
    | Return e ->
        [ List
            (Atom "return"
            ::
            (match e with
            | None -> []
            | Some e -> expression e))
        ]
    | Throw (i, e) -> [ List (Atom "throw" :: index (S i) :: expression e) ]
    | Rethrow i -> [ List [ Atom "rethrow"; Atom (string_of_int i) ] ]
    | CallInstr (f, l) ->
        [ List (Atom "call" :: index f :: List.concat (List.map ~f:expression l)) ]
    | Nop -> []
  and instructions l = List.concat (List.map ~f:instruction l) in
  expression, instructions

let expression ctx = fst (expression_or_instructions ctx)

let instructions ctx = snd (expression_or_instructions ctx)

let funct ctx name exported_name typ locals body =
  List
    ((Atom "func" :: index (V name) :: export exported_name)
    @ funct_type typ
    @ value_type_list "local" locals
    @ instructions ctx body)

let import f =
  match f with
  | Function _ | Global _ | Data _ | Tag _ -> []
  | Import { name; desc } ->
      [ List
          [ Atom "import"
          ; quoted_name "env"
          ; quoted_name name
          ; List
              (match desc with
              | Fun typ -> Atom "func" :: index (S name) :: funct_type typ
              | Global ty ->
                  [ Atom "global"; index (S name); List [ Atom "mut"; value_type ty ] ])
          ]
      ]

let escape_string s =
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if Poly.(c >= ' ' && c <= '~' && c <> '"' && c <> '\\')
    then Buffer.add_char b c
    else Printf.bprintf b "\\%02x" (Char.code c)
  done;
  Buffer.contents b

let data_contents ctx contents =
  let b = Buffer.create 16 in
  List.iter
    ~f:(fun d ->
      match d with
      | DataI8 c -> Buffer.add_uint8 b c
      | DataI32 i -> Buffer.add_int32_le b i
      | DataI64 i -> Buffer.add_int64_le b i
      | DataBytes s -> Buffer.add_string b s
      | DataSym (symb, ofs) ->
          Buffer.add_int32_le b (Int32.of_int (lookup_symbol ctx symb + ofs))
      | DataSpace n -> Buffer.add_string b (String.make n '\000'))
    contents;
  escape_string (Buffer.contents b)

let field ctx f =
  match f with
  | Function { name; exported_name; typ; locals; body } ->
      [ funct ctx name exported_name typ locals body ]
  | Global { name; typ } ->
      [ List
          (Atom "global"
          :: index (S name)
          :: List [ Atom "mut"; value_type typ ]
          :: expression ctx (Const (I32 0l)))
      ]
  | Tag { name; typ } ->
      [ List [ Atom "tag"; index (S name); List [ Atom "param"; value_type typ ] ] ]
  | Import _ -> []
  | Data { name; contents; _ } ->
      [ List
          (Atom "data"
          :: index (V name)
          :: (expression ctx (Const (I32 (Int32.of_int (lookup_symbol ctx (V name)))))
             @ [ Atom ("\"" ^ data_contents ctx contents ^ "\"") ]))
      ]

let data_size contents =
  List.fold_left
    ~f:(fun sz d ->
      sz
      +
      match d with
      | DataI8 _ -> 1
      | DataI32 _ -> 4
      | DataI64 _ -> 8
      | DataBytes s -> String.length s
      | DataSym _ -> 4
      | DataSpace n -> n)
    ~init:0
    contents

let data_offsets fields =
  List.fold_left
    ~f:(fun (i, addresses) f ->
      match f with
      | Data { name; contents; _ } ->
          i + data_size contents, Code.Var.Map.add name i addresses
      | Function _ | Global _ | Tag _ | Import _ -> i, addresses)
    ~init:(0, Code.Var.Map.empty)
    fields

let f fields =
  let heap_base, addresses = data_offsets fields in
  let ctx =
    { addresses
    ; functions = Code.Var.Map.empty
    ; function_count = 0
    ; constants = StringMap.singleton "__heap_base" heap_base
    }
  in
  let other_fields = List.concat (List.map ~f:(fun f -> field ctx f) fields) in
  let funct_table =
    let functions =
      List.map
        ~f:fst
        (List.sort
           ~cmp:(fun (_, i) (_, j) -> compare i j)
           (Code.Var.Map.bindings ctx.functions))
    in
    [ List
        [ Atom "table"
        ; Atom "funcref"
        ; List (Atom "elem" :: List.map ~f:(fun f -> index (V f)) functions)
        ]
    ]
  in
  Format.printf
    "%a@."
    format_sexp
    (List
       (Atom "module"
       :: (List.concat (List.map ~f:import fields)
          @ other_fields
          @ funct_table
          @ [ List
                [ Atom "memory"; Atom (string_of_int ((heap_base + 0xffff) / 0x10000)) ]
            ])))

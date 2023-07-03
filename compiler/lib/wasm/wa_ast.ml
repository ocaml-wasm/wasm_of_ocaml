type var = Code.Var.t

type symbol =
  | V of var
  | S of string

type packed_type =
  | I8
  | I16

type heap_type =
  | Func
  | Extern
  | Eq
  | I31
  | Type of var

type ref_type =
  { nullable : bool
  ; typ : heap_type
  }

type value_type =
  | I32
  | I64
  | F64
  | Ref of ref_type

type storage_type =
  | Value of value_type
  | Packed of packed_type

type 'typ mut_type =
  { mut : bool
  ; typ : 'typ
  }

type field_type = storage_type mut_type

type global_type = value_type mut_type

type func_type =
  { params : value_type list
  ; result : value_type list
  }

type str_type =
  | Struct of field_type list
  | Array of field_type
  | Func of func_type

type ('i32, 'i64, 'f64) op =
  | I32 of 'i32
  | I64 of 'i64
  | F64 of 'f64

type int_un_op =
  | Clz
  | Ctz
  | Popcnt
  | Eqz

type signage =
  | S
  | U

type int_bin_op =
  | Add
  | Sub
  | Mul
  | Div of signage
  | Rem of signage
  | And
  | Or
  | Xor
  | Shl
  | Shr of signage
  | Rotl
  | Rotr
  | Eq
  | Ne
  | Lt of signage
  | Gt of signage
  | Le of signage
  | Ge of signage

type float_un_op =
  | Neg
  | Abs
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Sqrt

type float_bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | CopySign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type memarg = int32

type expression =
  | Const of (int32, int64, float) op
  | ConstSym of symbol * int
  | UnOp of (int_un_op, int_un_op, float_un_op) op * expression
  | BinOp of (int_bin_op, int_bin_op, float_bin_op) op * expression * expression
  | Load of (memarg, memarg, memarg) op * expression
  | LocalGet of int
  | LocalTee of int * expression
  | GlobalGet of symbol
  | Call_indirect of func_type * expression * expression list
  | Call of symbol * expression list
  | MemoryGrow of int * expression
  | Seq of instruction list * expression
  | Pop
  | RefFunc of symbol
  | Call_ref of var * expression * expression list
  | I31New of expression
  | I31Get of signage * expression
  | ArrayNew of var * expression * expression
  | ArrayNewFixed of var * expression list
  | ArrayNewData of var * var * expression * expression
  | ArrayGet of signage option * var * expression * expression
  | ArrayLength of expression
  | StructNew of var * expression list
  | StructGet of signage option * var * int * expression
  | RefCast of ref_type * expression
  | RefTest of ref_type * expression
  | RefEq of expression * expression
  | RefNull

and instruction =
  | Drop of expression
  | Store of (memarg, memarg, memarg) op * expression * expression
  | LocalSet of int * expression
  | GlobalSet of symbol * expression
  | Loop of func_type * instruction list
  | Block of func_type * instruction list
  | If of func_type * expression * instruction list * instruction list
  | Try of
      func_type
      * instruction list
      * (string * instruction list) list
      * instruction list option
  | Br_table of expression * int list * int
  | Br of int * expression option
  | Return of expression option
  | Throw of string * expression
  | Rethrow of int
  | CallInstr of symbol * expression list
  | Nop
  | Push of expression
  | ArraySet of signage option * var * expression * expression * expression
  | StructSet of signage option * var * int * expression * expression
  | Br_on_cast of int * heap_type * expression
  | Return_call_indirect of func_type * expression * expression list
  | Return_call of symbol * expression list
  | Return_call_ref of symbol * expression * expression list

type import_desc =
  | Fun of func_type
  | Global of global_type

type data =
  | DataI8 of int
  | DataI32 of int32
  | DataI64 of int64
  | DataBytes of string
  | DataSym of symbol * int
  | DataSpace of int

type type_field =
  { name : var
  ; typ : str_type
  ; supertype : var option
  ; final : bool
  }

type module_field =
  | Function of
      { name : var
      ; exported_name : string option
      ; typ : func_type
      ; locals : value_type list
      ; body : instruction list
      }
  | Data of
      { name : var
      ; active : bool
      ; read_only : bool
      ; contents : data list
      }
  | Global of
      { name : symbol
      ; typ : global_type
      ; init : expression
      }
  | Tag of
      { name : symbol
      ; typ : value_type
      }
  | Import of
      { name : string
      ; desc : import_desc
      }
  | Type of type_field list

type var = Code.Var.t

type value_type =
  | I32
  | I64
  | F64

type func_type =
  { params : value_type list
  ; result : value_type list
  }

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

type block_type = value_type option

type symbol =
  | V of var
  | S of string

type expression =
  | Const of (int32, int64, float) op
  | ConstSym of symbol * int
  | UnOp of (int_un_op, int_un_op, float_un_op) op * expression
  | BinOp of (int_bin_op, int_bin_op, float_bin_op) op * expression * expression
  | Load of (memarg, memarg, memarg) op * expression
  | LocalGet of int
  | LocalTee of int * expression
  | GlobalGet of string
  | Call_indirect of func_type * expression * expression list
  | Call of symbol * expression list
  | MemoryGrow of int * expression
  | Seq of instruction list * expression

and instruction =
  | Drop of expression
  | Store of (memarg, memarg, memarg) op * expression * expression
  | LocalSet of int * expression
  | GlobalSet of string * expression
  | Loop of block_type * instruction list
  | Block of block_type * instruction list
  | If of block_type * expression * instruction list * instruction list
  | Try of
      block_type
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

type import_desc =
  | Fun of func_type
  | Global of value_type

type data =
  | DataI8 of int
  | DataI32 of int32
  | DataI64 of int64
  | DataBytes of string
  | DataSym of symbol * int
  | DataSpace of int

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
      ; read_only : bool
      ; contents : data list
      }
  | Global of
      { name : string
      ; typ : value_type
      }
  | Tag of
      { name : string
      ; typ : value_type
      }
  | Import of
      { name : string
      ; desc : import_desc
      }

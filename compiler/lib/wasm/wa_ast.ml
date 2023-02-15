type var = Code.Var.t

type value_type =
  | I32
  | I64
  | F64

type func_type =
  { params : value_type list
  ; result : value_type
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

type expression =
  | Const of (int32, int64, float) op
  | UnOp of (int_un_op, int_un_op, float_un_op) op * expression
  | BinOp of (int_bin_op, int_bin_op, float_bin_op) op * expression * expression
  | Load of (memarg, memarg, memarg) op * expression
  | LocalGet of int
  | Call_indirect of func_type * expression * expression list
  | Call of var * expression list

type instruction =
  | Drop of expression
  | Store of (memarg, memarg, memarg) op * expression * expression
  | LocalSet of int * expression
  | Loop of instruction list
  | Block of instruction list
  | If of expression * instruction list * instruction list
  | Br_table of expression * int list * int
  | Br of int
  | Return of expression option

type import_desc = Fun of func_type

type module_field =
  | Function of
      { name : var
      ; typ : func_type
      ; locals : value_type list
      ; body : instruction list
      }
  | Import of
      { name : string
      ; desc : import_desc
      }

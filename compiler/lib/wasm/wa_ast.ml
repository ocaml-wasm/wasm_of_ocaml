type var = Code.Var.t

type value_type =
  | I32
  | I64
  | F64

type typ =
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

type int_bin_op =
  | Add
  | Sub
  | Mul
  | DivS
  | DivU
  | RemS
  | RemU
  | And
  | Or
  | Xor
  | Shl
  | ShrS
  | ShrU
  | Rotl
  | Rotr
  | Eq
  | Ne
  | LtS
  | LtU
  | GtS
  | GtU
  | LeS
  | LeU
  | GeS
  | GeU

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

type memarg =
  { offset : int32
  ; align : int
  }

type expression =
  | Const of (int32, int64, float) op
  | UnOp of expression
  | BinOp of (int_bin_op, int_bin_op, float_bin_op) op * expression * expression
  | Load of (memarg, memarg, memarg) op
  | LocalGet of int
  | Call_indirect of typ
  | Call of var

type instruction =
  | Drop of expression
  | Store of (memarg, memarg, memarg) op * expression
  | LocalSet of int * expression
  | Loop of instruction list
  | Block of instruction list
  | If of expression * instruction list * instruction list
  | Br_table of int list * int
  | Br of int
  | Return of expression option

type import_desc = Fun of var * typ

type module_field =
  | Function of
      { name : var
      ; typ : typ
      ; locals : value_type list
      ; body : instruction list
      }
  | Import of
      { name : string
      ; desc : import_desc
      }

open Core

type placeholder = unit
  [@@deriving sexp]

type type_param = 
  | VoidT
  | IntT
  | StrT
  | FloatT
  | BoolT
  [@@deriving sexp]

type value =
  | Int of int
  | Str of string
  | F64 of float
  | Bool of bool
  [@@deriving sexp]


type unop =
  | Not
  [@@deriving sexp]

type identifier = string
  [@@deriving sexp]

type binop = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | NotEq
  | LessThan
  | LessEq
  | GreaterThan
  | GreaterEq
  | Land
  | Lor
  [@@deriving sexp]

type expression = 
  | Assign of identifier * expression
  | BinOp of binop * expression * expression
  | UnOp of unop * expression
  | Val of value
  | Call of identifier * expression list
  [@@deriving sexp]

type statement = 
  | Expr of expression
  | If of expression * statement * statement
  | Block of statement list
  | For of expression * expression * expression * statement
  | Return of expression
  [@@deriving sexp]

type typed_arg = 
  | Arg of type_param * identifier
  [@@deriving sexp]

type definition = 
  | FuncDef of type_param * identifier * typed_arg list * statement
  [@@deriving sexp]

type program = definition list 
  [@@deriving sexp]

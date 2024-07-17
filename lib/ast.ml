open Core

type placeholder = unit
  [@@deriving sexp]

type type_param = 
  | VoidT [@key 1]
  | IntT [@key 2]
  | StrT [@key 3]
  | FloatT [@key 4]
  | BoolT [@key 5]
  [@@deriving
    sexp,
    protobuf {protoc="frontend_ir.proto"}]

type value =
  | Int of int [@key 1]
  | Str of string [@key 2]
  | F64 of float [@key 3]
  | Bool of bool [@key 4]
  [@@deriving
  sexp,
  protobuf {protoc="frontend_ir.proto"}]

type unop =
  | Not [@key 1]
  [@@deriving
  sexp,
  protobuf {protoc="frontend_ir.proto"}]

type identifier = string
  [@@deriving
  sexp,
  protobuf {protoc="frontend_ir.proto"}]

type binop = 
  | Add [@key 1]
  | Sub [@key 2]
  | Mul [@key 3]
  | Div [@key 4]
  | Mod [@key 5]
  | Eq [@key 6]
  | NotEq [@key 7]
  | LessThan [@key 8]
  | LessEq [@key 9]
  | GreaterThan [@key 10]
  | GreaterEq [@key 11]
  | Land [@key 12]
  | Lor [@key 13]
  [@@deriving
  sexp,
  protobuf {protoc="frontend_ir.proto"}]

type expression = 
  | Assign of identifier * expression
  | BinOp of binop * expression * expression
  | UnOp of unop * expression
  | Val of value
  | Var of identifier
  | Call of identifier * expression list
  [@@deriving sexp]

type statement = 
  | Expr of expression
  | Declaration of type_param * identifier * expression
  | If of expression * statement * statement
  | Block of statement list
  | For of expression * expression * expression * statement
  | Return of expression
  [@@deriving sexp]

type typed_arg = type_param * identifier
  [@@deriving
  sexp,
  protobuf {protoc="frontend_ir.proto"}]

type definition = type_param * identifier * typed_arg list * statement
  [@@deriving sexp]

type program = definition list 
  [@@deriving sexp]

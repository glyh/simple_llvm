open Core
open Ast

type typed_expression = 
  | TAssign of identifier * typed_expression * type_param [@key 1]
  | TBinOp of binop * typed_expression * typed_expression * type_param [@key 2]
  | TUnOp of unop * typed_expression * type_param [@key 3]
  | TVal of value * type_param [@key 4]
  | TVar of identifier * type_param [@key 5]
  | TCall of identifier * typed_expression list * type_param [@key 6]
  [@@deriving
    sexp,
    protobuf {protoc="frontend_ir.proto"}]

(* HACK: won't compile if you merge the 2 together due to ppx_deriving_protobuf's constraints *)
type typed_statement_list = typed_statement list 
  [@@deriving
    sexp
    , protobuf {protoc="frontend_ir.proto"}
  ]
and typed_statement =
  | TExpr of typed_expression [@key 1]
  | TDeclaration of identifier * typed_expression [@key 2]
  | TIf of typed_expression * typed_statement * typed_statement [@key 3]
  | TBlock of typed_statement_list [@key 4]
  | TFor of typed_expression * typed_expression * typed_expression * typed_statement [@key 5]
  | TReturn of typed_expression [@key 6]
  [@@deriving
    sexp
    , protobuf {protoc="frontend_ir.proto"}
  ]

type typed_definition = type_param * identifier * typed_arg list * typed_statement
  [@@deriving
    sexp
    , protobuf {protoc="frontend_ir.proto"}
  ]

type typed_program = typed_definition list
  [@@deriving
    sexp
    , protobuf {protoc="frontend_ir.proto"}
  ]

let get_expression_type (e: typed_expression) = 
  match e with
  | TAssign(_, _, ty) 
  | TBinOp(_, _, _, ty) 
  | TUnOp(_, _, ty) 
  | TVal(_, ty) 
  | TVar(_, ty) 
  | TCall( _, _, ty) 
  -> ty

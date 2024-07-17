open Core
open Ast

type typed_expression = 
  | TAssign of identifier * typed_expression * type_param
  | TBinOp of binop * typed_expression * typed_expression * type_param
  | TUnOp of unop * typed_expression * type_param
  | TVal of value * type_param
  | TVar of identifier * type_param
  | TCall of identifier * typed_expression list * type_param
  [@@deriving sexp]

and typed_statement =
  | TExpr of typed_expression
  | TDeclaration of identifier * typed_expression
  | TIf of typed_expression * typed_statement * typed_statement
  | TBlock of typed_statement list
  | TFor of typed_expression * typed_expression * typed_expression * typed_statement
  | TReturn of typed_expression
  [@@deriving sexp]

type typed_definition = type_param * identifier * typed_arg list * typed_statement
  [@@deriving sexp]

type typed_program = typed_definition list
  [@@deriving sexp]

let get_expression_type (e: typed_expression) = 
  match e with
  | TAssign(_, _, ty) 
  | TBinOp(_, _, _, ty) 
  | TUnOp(_, _, ty) 
  | TVal(_, ty) 
  | TVar(_, ty) 
  | TCall( _, _, ty) 
  -> ty

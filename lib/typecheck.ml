open Ast
open Typed_ast

exception TypeMismatch of type_param * type_param
exception BinTypeMismatch of binop * type_param * type_param
exception CallMismatch of identifier * type_param list * type_param list

type function_type = type_param list * type_param

let type_to_string (t: type_param): string = 
    match t with
    | VoidT -> "()"
    | IntT -> "int"
    | StrT -> "string"
    | FloatT -> "float"
    | BoolT -> "bool"

let dump_type_chain (params_ty: type_param list): string = 
    match params_ty with
     | [] -> "()" 
     | _ -> (params_ty |> List.map type_to_string |> String.concat "->" )
        
let dump_funtion_type (ty: function_type): string = 
    let (params_ty, return_ty) = ty in 
        (dump_type_chain params_ty) ^ "->" ^ (type_to_string return_ty)

let dump_function_sig (fsig: (identifier * function_type)): string =  
    let (fname, ty) = fsig in
    fname ^ ": " ^ (dump_funtion_type ty)

let definiton_to_function_type 
  ((ret_type, ident, args, _): definition) : (string * (type_param list * type_param)) = 
  (ident, (args |> List.map (fun (ty, _) -> ty), ret_type))

module StrMap = Map.Make(String)

type funcsig_map = function_type StrMap.t
type local_map = type_param StrMap.t

let dump_function_sigs (sigs: funcsig_map): string = 
    let sigs_list = sigs |> StrMap.to_list in
    List.map dump_function_sig sigs_list 
    |> String.concat "\n"

let collect_function_signatures (p: program): funcsig_map =
  p 
  |> List.map definiton_to_function_type
  |> StrMap.of_list

let rec check_expression (sigs: funcsig_map) (locals: local_map) (e: expression): typed_expression = 
  match e with
  | Assign(id, rhs) -> 
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = get_expression_type typed_rhs in
      let var_type =StrMap.find id locals in
      if (rhs_type != var_type)
      then 
        raise (TypeMismatch (rhs_type, var_type))
      else
        TAssign(id, typed_rhs, var_type)
  | BinOp((Add | Sub | Mul | Div | Mod) as op, lhs, rhs) ->
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = get_expression_type typed_rhs in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = get_expression_type typed_lhs in
      begin match (lhs_type, rhs_type) with
      | (IntT, IntT) -> TBinOp(op, typed_lhs, typed_rhs, IntT)
      | (FloatT, FloatT) -> TBinOp(op, typed_lhs, typed_rhs, FloatT)
      | _ -> raise (BinTypeMismatch(op, lhs_type, rhs_type))
      end
  | BinOp((Eq | NotEq) as op, lhs, rhs) -> 
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = get_expression_type typed_rhs in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = get_expression_type typed_lhs in
      if lhs_type == rhs_type then
        TBinOp(op, typed_lhs, typed_rhs, BoolT)
      else 
        raise (BinTypeMismatch(op, lhs_type, rhs_type))
  | BinOp((LessThan | LessEq | GreaterThan | GreaterEq) as op, lhs, rhs) -> 
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = get_expression_type typed_rhs in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = get_expression_type typed_lhs in
      begin match (lhs_type, rhs_type) with
      | (IntT, IntT) -> TBinOp(op, typed_lhs, typed_rhs, BoolT)
      | (FloatT, FloatT) -> TBinOp(op, typed_lhs, typed_rhs, BoolT)
      | _ -> raise (BinTypeMismatch(op, lhs_type, rhs_type))
      end
  | BinOp((Land | Lor) as op, lhs, rhs) -> 
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = get_expression_type typed_rhs in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = get_expression_type typed_lhs in
      begin match (lhs_type, rhs_type) with
      | (BoolT, BoolT) -> TBinOp(op, typed_lhs, typed_rhs, BoolT)
      | _ -> raise (BinTypeMismatch(op, lhs_type, rhs_type))
      end
  | UnOp(Not, inner) -> 
      let typed_inner = check_expression sigs locals inner in
      let inner_type = get_expression_type typed_inner in
      if inner_type != BoolT
      then raise (TypeMismatch(BoolT, inner_type))
      else TUnOp(Not, typed_inner, BoolT)
  | Val(Int i) -> 
      TVal(Int i, IntT)
  | Val(Str s) -> 
      TVal(Str s, StrT)
  | Val(F64 f) -> 
      TVal(F64 f, FloatT)
  | Val(Bool b) -> 
      TVal(Bool b, BoolT)
  | Call(id, exps) -> 
      let exps_checked = List.map (fun e -> check_expression sigs locals e) exps in
      let type_of_exps = List.map get_expression_type exps_checked in
      let (param_types, ret_type) = StrMap.find id sigs in
      if (0 == compare type_of_exps param_types)
      then TCall(id, exps_checked, ret_type)
      else raise (CallMismatch(id, param_types, type_of_exps))
| Var(id) ->
    let var_type = StrMap.find id locals in
    TVar(id, var_type)

let rec check_statement (return_ty: type_param) (sigs: funcsig_map) (locals: local_map) (stmt: statement): (local_map * typed_statement) = 
  match stmt with 
  | Expr(exp) -> (locals, TExpr(check_expression sigs locals exp))
  | Declaration(decl_type, name, exp) -> 
      let typed_rhs = check_expression sigs locals exp in
      let rhs_type = get_expression_type typed_rhs in
      if (rhs_type != decl_type) 
      then
        raise (TypeMismatch (rhs_type, decl_type))
      else
    (StrMap.add name decl_type locals, TDeclaration(name, typed_rhs))
  | If (cond, then_clause, else_clause) -> 
      let typed_cond = check_expression sigs locals cond in
      let (_, then_clause_typed) = check_statement return_ty sigs locals then_clause in
      let (_, else_clause_typed) = check_statement return_ty sigs locals else_clause in
      (locals, TIf(typed_cond, then_clause_typed, else_clause_typed))
  | Block (stmts) ->
      begin match check_statements return_ty sigs locals stmts with
      | (locals, stmts)
      -> (locals, TBlock stmts)
      end
  | For (init, cond, step, inner) ->
      let init_checked = check_expression sigs locals init in
      let cond_checked = check_expression sigs locals cond in
      let step_checked = check_expression sigs locals step in
      let (_, inner_checked) = check_statement return_ty sigs locals inner in
      (locals, TFor(init_checked, cond_checked, step_checked, inner_checked))
  | Return (exp) -> 
      let typed_return_value = check_expression sigs locals exp in
      let return_type_inferred = get_expression_type typed_return_value in 
      if (return_type_inferred != return_ty) 
      then 
        raise (TypeMismatch (return_type_inferred, return_ty))
      else
        (locals, TReturn typed_return_value)

and check_statements (return_ty: type_param) (sigs: funcsig_map) (locals: local_map) (stmts: statement list): (local_map * typed_statement list) = 
  match stmts with
  | [] -> (locals, [])
  | stmt :: rest_stmts ->
      let (locals_new, stmt_checked) = check_statement return_ty sigs locals stmt in
      let (locals_final, rest_checked) = check_statements return_ty sigs locals_new rest_stmts in
      (locals_final, stmt_checked :: rest_checked)

let check_definitons (sigs: funcsig_map) ((ret_ty, name, typed_args, body): definition): typed_definition =
  let initial_params = 
    typed_args |> (List.map (fun (ty, id) -> (id, ty))) |> StrMap.of_list
  in 
  let (_, statement_checked) = check_statement ret_ty sigs initial_params body in
    (ret_ty, name, typed_args, statement_checked)

let typecheck (p: program) = 
  let function_sigs = collect_function_signatures p in
  (*function_sigs |> dump_function_sigs |> print_endline;*)
  p |> List.map (check_definitons function_sigs)

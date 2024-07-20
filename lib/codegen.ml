(*NOTE: 
  Reference: https://github.com/adamrk/llvm-ocaml-tutorial/blob/master/lib/codegen.ml
*)

open Llvm
open Typed_ast
open Ast

exception Unimplemented
exception Unreachable
exception FunctionNotFound of identifier
exception DuplicatedFunctionDefinition of string

let context = Llvm.global_context ()

let builder = Llvm.builder context

let the_module = Llvm.create_module context "simple"

let cg_to_ll_type (ctx: llcontext) (t: type_param): lltype = 
  match t with
  | VoidT -> void_type ctx
  | IntT -> i32_type ctx
  | BoolT -> i1_type ctx
  | FloatT -> float_type ctx
  | StrT -> raise Unimplemented

let cg_generate_value (ctx: llcontext) (v: value): llvalue =
  match v with
  | Int(i) -> const_int (i32_type ctx) i
  | Bool(true) -> const_int (i1_type ctx) 1
  | Bool(false) -> const_int (i1_type ctx) 0
  | F64(f) -> const_float (float_type ctx) f
  | Str(_) -> raise Unimplemented

let get_fn_type (ctx: llcontext) (def:typed_definition): lltype =
  let (ret_ty, _, args, _) = def in
  let ll_ret_ty = cg_to_ll_type ctx ret_ty in
  let ll_args_ty = List.map (fun (t, _) -> cg_to_ll_type ctx t) args |> Array.of_list in
  function_type ll_ret_ty ll_args_ty

let cg_declare_ll_function (ctx: llcontext) (def: typed_definition): llvalue = 
  let (_, name, _, _) = def in
  let fn_type = get_fn_type ctx def in
  begin match lookup_function name the_module with
  | None -> 
      declare_function name fn_type the_module
  | Some _ -> raise (DuplicatedFunctionDefinition(name))
  end

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let cg_create_entry_block_alloca the_function var_name (ty: lltype): llvalue =
  let builder =
    builder_at context (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  build_alloca ty var_name builder

module StrMap = Map.Make(String)
type sym_map = llvalue StrMap.t
type fn_map = lltype StrMap.t

let fold_lefti init f arr =
  let len = Array.length arr in
  let rec aux acc i =
    if i = len then acc
    else aux (f acc i arr.(i)) (i + 1)
  in
  aux init 0

let rec cg_codegen_expr (funs: fn_map) (locals: sym_map) (ctx: llcontext) (exp: typed_expression): llvalue =
  match exp with
  | TAssign(lhs_id, rhs, _) ->
      let rhs_gen = cg_codegen_expr funs locals ctx rhs in
      let lhs_gen = StrMap.find lhs_id locals in
      build_store rhs_gen lhs_gen builder |> ignore;
      rhs_gen
  (* TODO: operators for strings *)

  (* Operators for booleans *)
  | TBinOp(
    (Land | Lor | Eq | NotEq) as bop,
    lhs,
    rhs,
    BoolT
    ) ->
  let build_fn = 
    match bop with
    | Land -> build_and
    | Lor -> build_or
    | Eq -> build_icmp Icmp.Eq
    | NotEq -> build_icmp Icmp.Ne
    | _ -> raise Unreachable
  in
      let lhs_gen = cg_codegen_expr funs locals ctx lhs in
      let rhs_gen = cg_codegen_expr funs locals ctx rhs in
      build_fn lhs_gen rhs_gen "numeric_tmp" builder

  (* Operators for numerics *)
  | TBinOp(
    (Eq | NotEq | LessThan | LessEq | GreaterThan | GreaterEq | Add | Sub | Mul | Div | Mod) as bop, 
    lhs,
    rhs,
    ((IntT | FloatT) as t)) -> 
      let build_fn = 
        match (bop, t) with
        | (Eq, IntT) -> build_icmp Icmp.Eq
        | (Eq, FloatT) -> build_fcmp Fcmp.Oeq
        | (NotEq, IntT) -> build_icmp Icmp.Ne
        | (NotEq, FloatT) -> build_fcmp Fcmp.One
        | (LessThan, IntT) -> build_icmp Icmp.Slt
        | (LessThan, FloatT) -> build_fcmp Fcmp.Olt
        | (LessEq, IntT) -> build_icmp Icmp.Sle
        | (LessEq, FloatT) -> build_fcmp Fcmp.Olt
        | (GreaterThan, IntT) -> build_icmp Icmp.Sgt
        | (GreaterThan, FloatT) -> build_fcmp Fcmp.Ogt
        | (GreaterEq, IntT) -> build_icmp Icmp.Sge
        | (GreaterEq, FloatT) -> build_fcmp Fcmp.Ogt
        | (Add, IntT) -> build_add
        | (Add, FloatT) -> build_fadd
        | (Sub, IntT) -> build_sub
        | (Sub, FloatT) -> build_fsub
        | (Mul, IntT) -> build_mul
        | (Mul, FloatT) -> build_fmul
        | (Div, IntT) -> build_sdiv
        | (Div, FloatT) -> build_fdiv
        | (Mod, IntT) -> build_srem
        | (Mod, FloatT) -> build_frem
        | _ -> raise Unreachable
      in
      let lhs_gen = cg_codegen_expr funs locals ctx lhs in
      let rhs_gen = cg_codegen_expr funs locals ctx rhs in
      build_fn lhs_gen rhs_gen "numeric_tmp" builder
  | TUnOp(Not, inner, BoolT) ->
      let inner_gen = cg_codegen_expr funs locals ctx inner in
      build_not inner_gen "not_tmp" builder
  | TVal(v, _) -> 
      cg_generate_value ctx v
  | TVar(vname, ty) ->
      let alloc = StrMap.find vname locals in
      build_load (cg_to_ll_type ctx ty) alloc vname builder
  | TCall(id, args, _) ->
      let args_gen = List.map (fun arg -> cg_codegen_expr funs locals ctx arg) args in
      let callee = begin match lookup_function id the_module with
      | Some(f) -> f
      | _ -> raise (FunctionNotFound(id))
      end in
      let callee_type = StrMap.find id funs in
      build_call callee_type callee (args_gen |> Array.of_list) "calltmp" builder
  | tast -> 
      print_endline (sexp_of_typed_expression tast |> Sexplib.Sexp.to_string);
      raise Unreachable

let cg_create_argument_allocas (ctx: llcontext) (def: typed_definition) (fn_ref: llvalue): sym_map =
  let (_, _, args, _) = def in
  (params fn_ref) |> fold_lefti StrMap.empty (fun locals i ai ->
      let (ty, var_name) = List.nth args i in
      let alloca = cg_create_entry_block_alloca fn_ref var_name (cg_to_ll_type ctx ty) in
      build_store ai alloca builder |> ignore;
      StrMap.add var_name alloca locals)

let rec cg_codegen_stmt (funs: fn_map) (locals: sym_map) (ctx: llcontext) (stmt: typed_statement) : sym_map =
  match stmt with
  | TExpr(exp) -> 
      cg_codegen_expr funs locals ctx exp |> ignore;
      locals

  | TDeclaration(var_name, init_exp) ->
      let init_exp_gen = cg_codegen_expr funs locals ctx init_exp in
      let exp_ty = get_expression_type init_exp in
      let the_function = block_parent (insertion_block builder) in

      let alloca = cg_create_entry_block_alloca the_function var_name (cg_to_ll_type ctx exp_ty) in
      build_store init_exp_gen alloca builder |> ignore;
      StrMap.add var_name alloca locals

  | TBlock(stmts) -> 
      stmts |>
      List.fold_left
      (fun locals stmt -> cg_codegen_stmt funs locals ctx stmt) 
      locals

  | TReturn(exp) ->
      let exp_gen = cg_codegen_expr funs locals ctx exp in
      build_ret exp_gen builder |> ignore;
      locals

  | TFor(init, cond, step, body) ->
      (* Unlike kaleidoscope, this for statement emulates the C semantic *)
      cg_codegen_expr funs locals ctx init |> ignore;

      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in

      let loop_test_bb = append_block ctx "loop_test" the_function in
      let loop_body_bb = append_block ctx "loop_body" the_function in

      let loop_merge_bb = append_block ctx "loop_merge" the_function in

      position_at_end start_bb builder;
      build_br loop_test_bb builder |> ignore;

      position_at_end loop_test_bb builder;
      let init_cond_gen = cg_codegen_expr funs locals ctx cond in
      let init_cond_test_gen = build_icmp Icmp.Eq (const_int (i1_type ctx) 1) init_cond_gen "ifcond" builder in
      build_cond_br init_cond_test_gen loop_body_bb loop_merge_bb builder |> ignore; 

      position_at_end loop_body_bb builder;
      cg_codegen_stmt funs locals ctx body |> ignore;

      let loop_body_end_bb = insertion_block builder in
      position_at_end loop_body_end_bb builder |> ignore;
      cg_codegen_expr funs locals ctx step|> ignore;
      build_br loop_test_bb builder |> ignore;

      position_at_end loop_merge_bb builder |> ignore;

      locals

  | TIf(cond, _then, _else) ->
      let cond_gen = cg_codegen_expr funs locals ctx cond in
      let cond_compare_gen = build_icmp Icmp.Eq (const_int (i1_type ctx) 1) cond_gen "ifcond" builder in
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in

      let then_bb = append_block ctx "then" the_function in
      position_at_end then_bb builder;
      cg_codegen_stmt funs locals ctx _then |> ignore;
      (* NOTE: the end of the then block may be updated by nesting call of code generation for then statement *)
      let new_then_bb = insertion_block builder in

      let else_bb = append_block ctx "else" the_function in
      position_at_end else_bb builder;
      cg_codegen_stmt funs locals ctx _else |> ignore;
      let new_else_bb = insertion_block builder in

      let merge_bb = append_block ctx "ifcont" the_function in
      (* NOTE: we're not an expression based language so no need for phi insturctions *)

      position_at_end start_bb builder;
      build_cond_br cond_compare_gen then_bb else_bb builder |> ignore;

      position_at_end new_then_bb builder;
      build_br merge_bb builder |> ignore;

      position_at_end new_else_bb builder;
      build_br merge_bb builder |> ignore;

      position_at_end merge_bb builder |> ignore;
      locals

let cg_fill_function_definition (funs: fn_map) (ctx: llcontext) (def: typed_definition) (fn_ref: llvalue) =
  let bb = append_block ctx "entry" fn_ref in
  let (ret_ty, _, _, body) = def in
  position_at_end bb builder;
  let locals = cg_create_argument_allocas ctx def fn_ref in
  cg_codegen_stmt funs locals ctx body |> ignore;
  let return_polyfil = undef (cg_to_ll_type ctx ret_ty) in
  build_ret return_polyfil  builder |> ignore

let clean_up_module (m: llmodule) =
  let iter_b (b: llbasicblock) =
    let insts_to_remove: llvalue list ref = ref [] in
    let met_terminator: bool ref = ref false in
    b |> iter_instrs (fun inst ->
      match (!met_terminator, is_terminator inst) with
      | (false, true) ->
          met_terminator := true
      | (true, _) ->
          insts_to_remove := inst :: !insts_to_remove
      | _ -> ());

    !insts_to_remove |> List.map delete_instruction |> ignore
  in
  let iter_fn (f: llvalue) = 
    f |> iter_blocks iter_b
  in
  m |> iter_functions iter_fn

let generate_program (ctx: llcontext) (p: typed_program) = 
  let ast_and_ir = p |> List.map (fun def -> (def, cg_declare_ll_function ctx def)) in
  let funs = 
    p 
    |> List.map (fun ((_, name, _, _) as def) -> (name, get_fn_type ctx def))
    |> StrMap.of_list
  in
  ast_and_ir |> List.map (fun (def, fn) -> cg_fill_function_definition funs ctx def fn) |> ignore;

  clean_up_module the_module;
  Llvm_analysis.assert_valid_module the_module |> ignore;
  dump_module the_module

exception JITFailed

let run_program () =
  if Llvm_executionengine.initialize () then () else raise JITFailed;
  let engine = Llvm_executionengine.create the_module in
  let fp = Llvm_executionengine.get_function_address
    "main" (Foreign.funptr Ctypes.(void @-> returning int)) engine
  in
    print_endline @@ string_of_int @@ fp ()

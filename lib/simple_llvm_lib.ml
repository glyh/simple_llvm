open Lexing
open Typecheck
open Typed_ast

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse s = Parser_nice.parse_string s

let read_stdin () = 
  let acc = ref "" in 
  try 
    while true do 
      acc := !acc ^ "\n" ^ read_line()
    done;
    ""
  with
    End_of_file -> !acc

let debug (program: typed_program) = 
    let sexp = sexp_of_typed_program program in
      sexp |> Sexplib.Sexp.to_string |> print_endline 

let main () = 
    Parser_nice.pp_exceptions ();
    let code_string = read_stdin () in
    let untyped_ast = parse code_string in
    let typed_ast = typecheck untyped_ast in
    Codegen.generate_program (Llvm.global_context ()) typed_ast;
    Codegen.run_program ()

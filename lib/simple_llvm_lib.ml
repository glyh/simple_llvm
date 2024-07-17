open Lexing
open Typecheck
open Typed_ast

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse s = 
  let lexbuf = Lexing.from_string s in
  try 
    Parser.program_eof Lexer.token lexbuf
  with 
    Parser.Error -> 
      raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))

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
    let code_string = read_stdin () in
    let untyped_ast = parse code_string in
    let typed_ast = typecheck untyped_ast in
    debug typed_ast

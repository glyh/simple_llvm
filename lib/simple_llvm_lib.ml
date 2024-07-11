open Lexing
open Ast
(*open Core*)

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

let main () = 
  read_stdin () |> parse |> sexp_of_program |> Sexplib.Sexp.to_string |> print_endline
  (*print_endline "Hello, world!"*)

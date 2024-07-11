{
open Lexing
open Parser

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'

let drop_str n s = 
  let len = String.length s in
    String.sub s n (len - n)
let drop_str_r n s = 
  let len = String.length s in
    String.sub s 0 (len - n)
}

let digit = ['0'-'9']
let sign = ['-' '+']
let exponent = ['e' 'E']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '_')*

let keyword = ':' (alpha | digit) +

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Rules *)

rule token = parse
  | int_constant { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_constant { F64 (float_of_string (Lexing.lexeme lexbuf)) }
  (* binary operators *)
  | ";" { SEMICOL }
  | "," { COMMA }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | "<" { LT }
  | ">=" { GE }
  | ">" { GT }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "=" { ASSIGN }

  (* preserved words *)
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "return" { RETURN }

  | "true" { TRUE }
  | "false" { FALSE }

  (* primitive types *)
   | "void" { VOID_T }
   | "int" { INT_T }
   | "string" { STR_T }
   | "float" { FLOAT_T }
   | "bool" { BOOL_T }

  (*This is for disambiguiate, as we allow arbitrary sequence of expressions, and
    `ID (..)` is a tuple followed by an ID, while `ID(..)` is a call *)
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 32) lexbuf }
  (* etc. *)
  | whitespace { token lexbuf }
  | newline { token lexbuf } (* just ignore *)
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf } 
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf } 
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf } 
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }

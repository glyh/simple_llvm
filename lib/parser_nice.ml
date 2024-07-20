include Nice_parser.Make(struct
  type result = Ast.program
  type token = Parser.token
  exception ParseError = Parser.Error
  let parse = Parser.program_eof
  include Lexer
end)

%{
  open Ast

%}

%token EOF
%token SEMICOL
%token COMMA
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN

%token ASSIGN

%token ADD
%token SUB
%token MUL
%token DIV
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR
%token NOT

%right ASSIGN
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MUL DIV
%nonassoc NOT 


(* Values *)
%token <string> IDENTIFIER
%token <string> STRING
%token <int> INT
%token <float> F64
%token TRUE
%token FALSE

%token VOID_T
%token INT_T
%token STR_T
%token FLOAT_T
%token BOOL_T

%token IF
%token ELSE
%token FOR
%token RETURN

%start <program> program_eof

%%

program_eof:
  | defs=list(definition) EOF { defs }

definition: 
  | t=type_param id=IDENTIFIER LPAREN RPAREN body=statement {
    FuncDef(t, id, [], body) 
  }
  | t=type_param id=IDENTIFIER LPAREN args=arg_list RPAREN body=statement {
    FuncDef(t, id, args, body) 
  }

type_param: 
  | VOID_T { VoidT }
  | INT_T { IntT }
  | STR_T { StrT }
  | FLOAT_T { FloatT }
  | BOOL_T { BoolT }

arg_list: 
  | a=arg { [a] } 
  | a=arg COMMA rest=arg_list { [a] @ rest }

arg:
  | t=type_param id=IDENTIFIER { Arg(t, id) } 

statement: 
  | LBRACE ss=list(statement) RBRACE {
    Block(ss)
  }
  | RETURN e=expression SEMICOL { Return(e) }
  | e=expression SEMICOL { Expr(e) }
  | IF test=expression then_clause=statement ELSE else_clause=statement { If(test, then_clause, else_clause) }
  | FOR LPAREN initial=expression SEMICOL cond=expression SEMICOL step=expression SEMICOL body=statement {
    For(initial, cond, step, body)
  }
  | SEMICOL { Block([]) }

%inline bin_op:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | EQ { Eq } 
  | NEQ { NotEq } 
  | LT { LessThan } 
  | LE { LessEq } 
  | GT { GreaterThan } 
  | GE { GreaterEq } 
  | AND { Land } 
  | OR { Lor } 

%inline un_op:
  | NOT { Not }

param_list: 
  | e=expression { [e] } 
  | e=expression COMMA rest=param_list { [e] @ rest }

expression: 
  | lhs=IDENTIFIER ASSIGN rhs=expression { Assign(lhs, rhs) }
  | id=IDENTIFIER LPAREN RPAREN { Call(id, []) }
  | id=IDENTIFIER LPAREN l=param_list RPAREN { Call(id, l) }
  | lhs=expression op=bin_op rhs=expression { BinOp(op, lhs, rhs) }
  | op=un_op inner=expression { UnOp(op, inner) }
  | LPAREN inner=expression RPAREN { inner }
  | v=value  { Val v }

value:
  | i=INT { Int(i) }
  | s=STRING { Str(s) }
  | f=F64 { F64(f) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }

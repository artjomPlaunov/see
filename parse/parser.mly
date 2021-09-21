%{
  [@@@coverage exclude_file]
  open Ast
%}

%token  <int>     INT
%token  <string>  ID
%token  LBRACE
%token  RBRACE
%token  EQ
%token  PLUS
%token  COMMA
%token  LPAREN
%token  RPAREN
%token  SEMICOLON
%token  LBRACK
%token  RBRACK
%token  INT_TYPE
%token  VOID_TYPE
%token  RETURN
%token  EOF

%start program

%type <program> program
%type <fun_def> fun_def
%type <type_>   type_

%%

program:
  | fun_defs=list(fun_def); EOF {Prog(fun_defs)}

fun_def:
  | ret_type=type_; name=ID; LPAREN; RPAREN; LBRACE; RBRACE; {Fun(ret_type,name)}


type_:
  | INT_TYPE    {INT_T}
  | VOID_TYPE   {VOID_T}

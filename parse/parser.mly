%{
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
%type <block>   block
%type <stm>     stm
%type <decl>    decl
%type <type_>   type_

%%

program:
  | fun_defs=list(fun_def); EOF; 
    {Prog(fun_defs)}

fun_def:
  | ret_type=type_; name=ID; LPAREN; RPAREN; LBRACE; b=block; RBRACE; 
    {Fun(ret_type,name,b)}

block:
  | stm_list = list(stm); 
    {Block(stm_list)}

stm:
  | var_type=type_; decl_list=separated_list(COMMA,decl); SEMICOLON;
    {Stm_declLst(var_type,decl_list)}
  | type_; name=ID; LBRACK; size=INT; RBRACK; EQ; LBRACE;
    init_lst=separated_list(COMMA,INT); RBRACE; SEMICOLON;
    {Stm_arrayDecl(name,size,init_lst)};

decl:
  | var_name=ID; EQ; value=INT; 
    {Decl(var_name,value)}

type_:
  | INT_TYPE    {INT_T}
  | VOID_TYPE   {VOID_T}









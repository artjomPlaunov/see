%{
  open Ast
%}

%token  <int>     INT
%token  <string>  ID
%token  LBRACE
%token  RBRACE
%token  EQ
%token  LEQ
%token  LT
%token  GT
%token  PLUS
%token  IF
%token  ELSE
%token  ASTERISK
%token  MINUS
%token  COMMA
%token  LPAREN
%token  RPAREN
%token  SEMICOLON
%token  LBRACK
%token  RBRACK
%token  INT_TYPE
%token  VOID_TYPE
%token  RETURN
%token  FOR
%token  EOF

%start program

%type <program>   program
%type <fun_def>   fun_def
%type <paramLst>  paramLst
%type <param>     param
%type <block>     block
%type <stm>       stm
%type <exp>       exp
%type <op>        op
%type <decl>      decl
%type <type_>     type_

%%

program:
  | fun_defs=list(fun_def); EOF; 
    {Prog(fun_defs)}

fun_def:
  | ret_type=type_; name=ID; 
    LPAREN; 
    params=paramLst;
    RPAREN; 
    LBRACE; b=block; RBRACE; 
    {Fun(ret_type,params,(Ident name),b)}

paramLst:
  | params = separated_list(COMMA,param);
    {ParamLst(params)}

param:
  | var_type=type_; name=ID;
    {Param(var_type,(Ident name))}

block:
  | stm_list = list(stm); 
    {Block(stm_list)}

stm:
  | var_type=type_; decl_list=separated_list(COMMA,decl); SEMICOLON;
    {Stm_declLst(var_type,decl_list)}
  | type_; name=ID; LBRACK; size=INT; RBRACK; EQ; LBRACE;
    init_lst=separated_list(COMMA,INT); RBRACE; SEMICOLON;
    {Stm_arrayDecl((Ident name),size,init_lst)}
  | stm_exp=exp; SEMICOLON;{Stm_exp(stm_exp)}
  | FOR; LPAREN; 
    e1=exp; SEMICOLON; e2=exp; SEMICOLON; e3=exp;
    RPAREN; LBRACE; b=block; RBRACE;
    {Stm_for(e1,e2,e3,b)}
  | IF; LPAREN; e=exp; RPAREN; 
    LBRACE; b1=block; RBRACE; 
    ELSE; LBRACE; b2=block; RBRACE; 
    {Stm_ifElse (e, b1, b2)}
  | IF; LPAREN; RPAREN; LBRACE; RBRACE; {Stm_if}

exp:
  | RETURN; return_val=exp; {Return(return_val)} 
  | constant = INT; {Constant(constant)}
  | var=ID; {Variable(Ident(var))}
  | arrID=ID; LBRACK; value=INT; RBRACK; 
    {ArraySubscriptInt(Ident(arrID), value)}
  | arrID=ID; LBRACK; value=ID; RBRACK; 
    {ArraySubscriptVar(Ident(arrID), Ident(value))}
  | funID=ID; LPAREN; argList=separated_list(COMMA,exp); RPAREN; 
    {FunctionCall(Ident(funID), argList)}
  | destID=ID; EQ; e=exp; {Assign(Ident(destID), e)}
  | e1=exp; o=op; e2=exp; {Arith(e1, o, e2)}
  | e1=exp; o=condOp; e2=exp; {Cond(e1,o,e2)}

op:
  | PLUS      {Add}
  | ASTERISK  {Mul}
  | MINUS     {Sub}

condOp:
  | LEQ       {Leq}
  | LT        {Lt}
  | GT        {Gt}

decl:
  | var_name=ID; EQ; value=INT; 
    {Int_decl((Ident var_name),value)}
  | var_name=ID; EQ; value=ID; 
    {Var_decl((Ident var_name),(Ident value))}
  | var_name=ID; {Undef_decl((Ident var_name))}

type_:
  | INT_TYPE    {INT_T}
  | VOID_TYPE   {VOID_T}





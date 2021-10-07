(* Identifiers. *)
type ident = Ident of string [@@deriving show]

(* Three pre-defined program types:
   -  ints
   -  void
   -  arrays (defined by the size of the array). *) 
type type_ = 
  | INT_T
  | VOID_T 
  (*| ARRAY_T of int*) [@@deriving show]

(* A declaration consists of an identifier and an int value. *)
type decl = Decl of ident * int [@@deriving show]

type op = 
  | Mul
  | Add
  | Sub [@@ deriving show]

type  exp   = 
  | Return of exp 
  | Constant of int 
  | Variable of ident 
  | ArraySubscript of ident * int
  | FunctionCall of ident * (exp list)
  | Assign of ident * exp
  | Arith of exp * op * exp
  [@@ deriving show]


(* A statement is either a declaration list or an array declaration. *)
type stm = 
  | Stm_declLst   of type_ * (decl list)
  | Stm_arrayDecl of ident * int * (int list) 
  | Stm_exp       of exp  [@@deriving show] 

(* A block is a list of statements. *)
type block = Block of stm list [@@deriving show]

(* A parameter is a type with an identifier. *)
type param = Param of type_ * ident [@@deriving show]

(* A parameter list is a list of parameters. *)
type paramLst = ParamLst of param list [@@deriving show]

(* A function consists of a type, parameter list, ID, and a block. *)
type fun_def = Fun of type_ * paramLst * ident * block [@@deriving show]

(* A program is a list of functions. *)
type program = Prog of fun_def list [@@deriving show]


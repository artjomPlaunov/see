type ident = string

type type_ = 
  | INT_T
  | VOID_T

type decl = Decl of ident * int

type stm = 
  | Stm_declLst   of type_ * (decl list)
  | Stm_arrayDecl of ident * int * (int list)

type block = Block of stm list

type fun_def = Fun of type_ * ident * block

type program = Prog of fun_def list

let rec print_declLst = function
  | []    ->  ()
  | (Decl(id,init))::t  ->  Printf.printf "  (ID: %s) = (VALUE: %d)\n" id init;
                            print_declLst t

let print_stm s = 
  match s with
  | Stm_declLst (_, declLst)  -> 
      print_string "Declaration List of type INT:\n";
      print_declLst declLst
  | Stm_arrayDecl _  -> ()

let rec print_stmLst = function
  | []    ->  ()
  | h::t  ->  print_stm h;
              print_stmLst t

let print_block = function
  | Block stmLst  ->  print_stmLst stmLst

let print_fun f =
  let (t,id,b) = f in
  let s = 
    match t with
    | INT_T   ->  "INT"
    | VOID_T  ->  "VOID"
  in
    print_string ("Function Name: " ^ id ^ "\n");
    print_string ("Return Type: " ^ s ^ "\n");
    print_block b;
    print_string "\n"


let rec print_funs = function
  | []    ->  ()
  | (Fun (ty,id,b))::t  ->  print_fun (ty,id,b);
                            print_funs t

let print_program = function
  | Prog funs -> print_funs funs

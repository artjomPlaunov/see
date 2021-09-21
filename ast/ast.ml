
type ident = string

type type_ = 
  | INT_T
  | VOID_T

type fun_def = Fun of type_ * ident

type program = Prog of fun_def list

let print_fun f =
  let (t,id) = f in
  let s = 
    match t with
    | INT_T   ->  "int"
    | VOID_T  ->  "void"
  in
    print_string ("Function Name: " ^ id ^ "\n");
    print_string ("Return Type: " ^ s ^ "\n\n")

let rec print_funs = function
  | []    ->  ()
  | (Fun (ty,id))::t  ->  print_fun (ty,id);
                          print_funs t

let print_program = function
  | Prog funs -> print_funs funs

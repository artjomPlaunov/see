
let rec print_declLst = function
  | []    ->  ()
  | (Decl(id,init))::t  ->  Printf.printf "  (ID: %s) = (VALUE: %d)\n" id init;
                            print_declLst t
let rec printArray = function
  | []    ->  ()
  | x::[] ->  Printf.printf "%d}\n" x
  | h::t  ->  Printf.printf "%d, " h;
              printArray t

let print_stm s = 
  match s with
  | Stm_declLst (_, declLst)  -> 
      print_string "Declaration List of type INT:\n";
      print_declLst declLst
  | Stm_arrayDecl (id,size,intLst)  -> 
      Printf.printf 
        "(ID: %s) = (VALUE: INT[%d]): {" id size;
      printArray intLst
let rec print_stmLst = function
  | []    ->  ()
  | h::t  ->  print_stm h;
              print_stmLst t

let print_block = function
  | Block stmLst  ->  print_stmLst stmLst

let print_param = function
  | Param (_,id) -> 
      Printf.printf
        "PARAM (TYPE: INT), (ID: %s)\n" id

let rec print_paramLst = function
  | []    ->  ()
  | h::t  ->  print_param h;
              print_paramLst t

let print_fun f =
  let (t,(ParamLst(p)),id,b) = f in
  let s = 
    match t with
    | INT_T   ->  "INT"
    | VOID_T  ->  "VOID"
  in
    print_string ("Function Name: " ^ id ^ "\n");
    print_string ("Return Type: " ^ s ^ "\n");
    print_paramLst p;
    print_block b;
    print_string "\n"


let rec print_funs = function
  | []    ->  ()
  | (Fun (ty,p,id,b))::t  ->  print_fun (ty,p,id,b);
                              print_funs t

let print_program = function
  | Prog funs -> print_funs funs

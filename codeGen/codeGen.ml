open Ast
open Core

let lbl = ref 0

(* block -> (declaration list, remainder of block). *)
let splitBlock block = 
  let block = match block with Block b -> b in
  let rec aux res lst = 
  match lst with
    | []              ->  (res,[])
    | (Stm_exp _)::_  ->  (res, lst)
    | (Stm_for _)::_  ->  (res, lst)
    | (Stm_ifElse _)::_  ->  (res, lst)
    | h::t            ->  let res = res@[h] in
                          aux res t
  in aux [] block

(* Parameter List -> # of bytes of storage required for parameters. *)
let getParamLstSize p = 
  match p with
  | ParamLst l -> (List.length l)*4

(* Declaration List -> # of bytes of storage required for declarations. *)
let getDeclsSize d = 
  let rec aux res = function
  | []                          ->  res
  | Stm_declLst   (_,l)::t      ->  aux (((List.length l)*4)+res) t
  | Stm_arrayDecl (_,s,_)::t    ->  aux ((4*s)+res) t
  | _ -> res
  in aux 0 d

(* Parameter List -> Declaration List -> # of bytes of stack space needed. *)
let getLocalsSize paramLst decls =
  let l1 = getParamLstSize paramLst in
  let l2 = getDeclsSize decls in
  l1 + l2

(* x86 assembly -> stack push length -> append instruction to push space onto 
   the stack. *)
let pushStack res l = 
  let s = Format.asprintf "  subq $%d, %%rsp" l in
  res@[s] 
   
(* int n -> register used for passing in argument # n. *)
let getParamReg num = 
  match num with
  | 1 ->  "edi"
  | 2 ->  "esi"
  | 3 ->  "edx"
  | 4 ->  "ecx"
  | 5 ->  "r8d"
  | 6 ->  "r9d"
  | _ ->  "rError" 

(*  Op type -> Corresponding x86 instruction prefix. *)
let getOpInstr op = 
  match op with
  | Mul -> "imull"
  | Add -> "addl"
  | Sub -> "subl"

(*  x86 -> Parameter List -> $SP -> (id -> offset) map ->
    Append instructions to x86 code to push parameters onto the stack. *)  

let pushParams res paramLst nextOffset offsetMap = 
  let pLst = match paramLst with ParamLst lst -> lst in
  let rec aux res pLst nextOffset offsetMap paramNum =  
    if paramNum > 6 then (
      let rec aux' res pLst nextOffset offsetMap paramNum = 
      match pLst with
      | [] -> (res, nextOffset,offsetMap)
      | Param((INT_T,Ident id))::t |  Param((VOID_T,Ident id))::t ->  
        let offsetMap = Map.add_exn offsetMap ~key:id ~data:(16+(4*paramNum)) 
        in
        aux' res t nextOffset offsetMap (paramNum+1)
      in aux' res (List.rev pLst) nextOffset offsetMap 0
    ) 
    else
    match pLst with
    | []          ->  (res,nextOffset,offsetMap)
    | Param((INT_T,Ident id))::t |  Param((VOID_T,Ident id))::t ->  
        let offsetMap = Map.add_exn offsetMap ~key:id ~data:nextOffset in
        let reg = getParamReg paramNum in
        let s = Format.asprintf "  movl %%%s, %d(%%rbp)" reg nextOffset in  
        aux (res@[s]) t (nextOffset-4) offsetMap (paramNum+1)
  in aux res pLst nextOffset offsetMap 1

let rec pushDecls res decls offset offsetMap = 
  match decls with
  | []  ->  (res,offset,offsetMap)
  | (Int_decl (Ident id,value))::t ->
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:offset in
      let s = Format.asprintf "  movl $%d %d(%%rbp)" value offset in
      pushDecls (res@[s]) t (offset-4) offsetMap
  | (Var_decl (Ident id, Ident value))::t -> 
      let srcOffset = 
        match Map.find offsetMap value with
        | None -> (-99999)
        | Some n -> n in
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:offset in
      let i1 = Format.asprintf "  movl %d(%%rbp) %%eax" srcOffset in
      let i2 = Format.asprintf "  movl %%eax %d(%%rbp)" offset in
      pushDecls (res@[i1]@[i2]) t (offset-4) offsetMap
  | (Undef_decl (Ident id))::t ->
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:offset in
      pushDecls (res) t (offset-4) offsetMap


let pushArrayVals res vals offset =
  let rec aux acc offset = function
    | []    ->  (acc,offset)
    | h::t  ->  let s = Format.asprintf "  movl $%d %d(%%rbp)" h offset in
                aux (acc@[s]) (offset-4) t in 
  let (tl,offset) = aux [] offset (List.rev vals) in
  ((res@(List.rev tl)),offset)

let rec pushAllDecls res allDecls offset offsetMap = 
  match allDecls with
  | []  ->  (res,offset,offsetMap)
  | (Stm_declLst (_,decls))::t ->
      let (res,offset,offsetMap) =  pushDecls res decls offset offsetMap in
      pushAllDecls res t offset offsetMap
  | (Stm_arrayDecl (Ident id, size, values))::t ->
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:(offset-(4*(size-1)))
      in
      let (res,offset) = pushArrayVals res values offset in
      pushAllDecls res t offset offsetMap
  | _ -> (res,offset,offsetMap)

let getRBPOffset id offsetMap = 
  let offset = 
    match Map.find offsetMap id with
    | None -> (-99999)
    | Some n -> n in
  Format.asprintf "%d(%%rbp)" offset

let getRBPArrayOffset id idx offsetMap = 
  let offset = 
    match Map.find offsetMap id with
    | None -> (-99999)
    | Some n -> n in
  Format.asprintf "%d(%%rbp)" (offset + (idx*4))
  

let genFunCall res offsetMap eLst =
  let rec aux res offsetMap eLst argNum = 
    if argNum > 6 then (
    let rec aux' res offsetMap eLst argNum = 
    match eLst with 
      | [] -> res
      | (Variable (Ident id))::t ->
        let i1 = Format.asprintf "  subq $4, %%rsp" in
        let src1 = getRBPOffset id offsetMap in 
        let i2 = Format.asprintf "  movl %s, %%eax" src1 in
        let i3 = Format.asprintf "  movl %%eax, 0(%%rsp)" in
        aux' (res@[i1;i2;i3]) offsetMap t (argNum+1)
      | _ -> res
    in
    aux' res offsetMap (List.rev(eLst)) 0 
    )
    else
    
    let r = getParamReg argNum in
    match eLst with
    | []  ->  res
    | (Constant v)::t -> 
        let i1 = Format.asprintf "  movl $%d %s" v r in
        let res = res@[i1] in
        aux res offsetMap t (argNum+1)
    | (Variable (Ident id))::t ->
        let src = getRBPOffset id offsetMap in
        let i1 = Format.asprintf "  movl %s %s" src r in
        aux (res@[i1]) offsetMap t (argNum+1)
    | (ArraySubscriptInt ((Ident id), idx))::t ->
        let src = getRBPArrayOffset id idx offsetMap in
        let i1 = Format.asprintf "  movl %s %s" src r in
        aux (res@[i1]) offsetMap t (argNum+1)
    | _ -> res
  in 
  aux res offsetMap eLst 1
  

let genBlock res stms offset offsetMap =
  let aux res exp _ offsetMap = 
  match exp with
    (* Very specific case where destination and sources are all ID's. *)
    | Assign 
      (Ident id, Arith (Variable (Ident id1) ,op, Variable (Ident id2))) ->
        let opInstr = getOpInstr op in
        let source1 = getRBPOffset id1 offsetMap in
        let source2 = getRBPOffset id2 offsetMap in
        let dest    = getRBPOffset id  offsetMap in
        let i1 = Format.asprintf "  movl %s, %%eax" source1 in
        let i2 = Format.asprintf "  %s %s, %%eax" opInstr source2 in
        let i3 = Format.asprintf "  movl %%eax, %s" dest in
        res@[i1;i2;i3]
    | Assign 
      (Ident id, Arith (Variable (Ident id1) ,op, Constant v)) ->
        let opInstr = getOpInstr op in
        let source1 = getRBPOffset id1 offsetMap in
        let dest    = getRBPOffset id  offsetMap in
        let i1 = Format.asprintf "  movl %s, %%eax" source1 in
        let i2 = Format.asprintf "  %s %d, %%eax" opInstr v in
        let i3 = Format.asprintf "  movl %%eax, %s" dest in
        res@[i1;i2;i3]
    | Assign (Ident id, Constant v) ->
        let source = getRBPOffset id offsetMap in
        let i = Format.asprintf "  movl $%d, %s" v source in
        res@[i]
    | Assign (Ident id1, FunctionCall (Ident id2, eLst)) ->
        let s = Format.asprintf "  call %s" id2 in
        let res = genFunCall (res) offsetMap eLst in
        let dest = getRBPOffset id1 offsetMap in
        let i1 = Format.asprintf "  movl %%eax, %s" dest in
        res@[s;i1]
    | FunctionCall ((Ident id), eLst) ->
        let s = Format.asprintf "  call %s" id in
        let res = genFunCall res offsetMap eLst in
        res@[s]
    | Return (Constant c) -> 
        let i1 = Format.asprintf "  movl $%d, %%eax" c in
        let i2 = "  leave" in
        let i3 = "  ret" in
        res@[i1;i2;i3]
    | Return (Variable (Ident id)) ->
        let src = getRBPOffset id offsetMap in
        let i1 = Format.asprintf "  movl %s, %%eax" src in
        let i2 = "  leave" in
        let i3 = "  ret" in
        res@[i1;i2;i3]
    | _ -> res
  in 

  let rec aux' res stms offset offsetMap = 
  match stms with
    | []  -> res
    | (Stm_exp e)::t -> 
        let res = aux res e offset offsetMap in
        aux' res t offset offsetMap
    | (Stm_for (e1,
                Cond(Variable (Ident (id)), Leq, Constant v),e2,Block b))::t ->
        (* Generate code for index initialization. *)
        let res = aux res e1 offset offsetMap in
        let lbl1 = Format.asprintf "L%d" !lbl in 
        let () = lbl := !lbl + 1 in
        let lbl2 = Format.asprintf "L%d" !lbl in
        let () = lbl := !lbl + 1 in
        let i1 = Format.asprintf "  jmp %s" lbl1 in
        let i2 = Format.asprintf "%s:" lbl2 in
        let res = res@[i1] in
        let res = res@[i2] in
        let res = aux' res b offset offsetMap in  
        let res = aux res e2 offset offsetMap in
        let i3 = Format.asprintf "%s:" lbl1 in
        let res = res@[i3] in
        let src = getRBPOffset id offsetMap in
        let i4 = Format.asprintf "  cmpl $%d, %s" v src in
        let i5 = Format.asprintf "  jle %s" lbl2 in
        aux' (res@[i4;i5;]) t offset offsetMap
    | (Stm_ifElse (Cond(ArraySubscriptInt(Ident id,v1), Gt, Constant v2),      
                    Block b1, Block b2))::t ->
        let src = getRBPArrayOffset id v1 offsetMap in
        let i1 = Format.asprintf "  movl %s, %%eax" src in
        let i2 = Format.asprintf "  cmpl %d, %%eax" v2 in 
        let lbl1 = Format.asprintf ".L%d" !lbl in
        let () = lbl := !lbl + 1 in
        let i3 = Format.asprintf "  jg %s" lbl1 in
        let lbl2 = Format.asprintf ".L%d" !lbl in
        let () = lbl := !lbl + 1 in
        let res = aux' (res@[i1;i2;i3]) b2 offset offsetMap in
        let i4 = Format.asprintf "  jmp %s" lbl2 in
        let i5 = Format.asprintf "%s:" lbl1 in
        let res = aux' (res) b1 offset offsetMap in
        let i6 = Format.asprintf "%s:" lbl2 in
        aux' (res@[i6]) t offset offsetMap
    | _ ->  res
  in 
  aux' res stms offset offsetMap


let genFun f = 
  let Fun (_,paramLst,Ident id,block) = f in
  let res = [id ^ ":"; "  pushq %rbp"; "  movq %rsp, %rbp"] in
  let (decls,block) = splitBlock block in
  let offsetMap = Map.empty (module String) in
  let len = getLocalsSize paramLst decls in
  let offset = (-4) in
  let res = pushStack res len in
  let (res,offset,offsetMap) = pushParams res paramLst offset offsetMap in 
  let (res,offset,offsetMap) = pushAllDecls res decls offset offsetMap in
  let res = genBlock res block offset offsetMap in
  res

(* genProg is the workhorse.
   AST -> File Name -> x86 Assembly -> Output File. *)
let genProg ast fname = 
  let rec aux res = function
    | []    ->  res
    | h::t  ->  let res' = res@[genFun h] in
                    aux res' t 
  in 
  let p = 
    match ast with
    | Prog funs -> aux [] funs in 

  let oc = Out_channel.create fname in 
  let printFun x = 
    let _ = List.map x ~f:(fprintf oc "%s\n") in
    fprintf oc "\n" in
  let _ = List.map p ~f:(fun x -> printFun x) in
  Out_channel.close oc;
  

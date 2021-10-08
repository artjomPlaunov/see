open Ast
open Core

let splitBlock block = 
  let block = match block with Block b -> b in
  let rec aux res lst = 
  match lst with
    | []              ->  (res,[])
    | (Stm_exp _)::_  ->  (res, lst)
    | h::t            ->  let res = res@[h] in
                          aux res t
  in aux [] block

let getParamLstSize p = 
  match p with
  | ParamLst l -> (List.length l)*4

let getDeclsSize d = 
  let rec aux res = function
  | []                          ->  res
  | Stm_declLst   (_,l)::t      ->  aux (((List.length l)*4)+res) t
  | Stm_arrayDecl (_,s,_)::t    ->  aux ((4*s)+res) t
  | _ -> res
  in aux 0 d

let getLocalsSize paramLst decls =
  let l1 = getParamLstSize paramLst in
  let l2 = getDeclsSize decls in
  l1 + l2

let pushStack res l = 
  let s = Format.asprintf "  subq $%d, %%rsp" l in
  res@[s] 
    
let getParamReg num = 
  match num with
  | 1 ->  "edi"
  | 2 ->  "esi"
  | 3 ->  "edx"
  | 4 ->  "ecx"
  | 5 ->  "r8d"
  | 6 ->  "r9d"
  | _ ->  "rError" 

let pushParams res paramLst nextOffset offsetMap = 
  let pLst = match paramLst with ParamLst lst -> lst in
  let rec aux res pLst nextOffset offsetMap paramNum =  
    match pLst with
    | []          ->  (res,nextOffset,offsetMap)
    | Param((INT_T,Ident id))::t |  Param((VOID_T,Ident id))::t ->  
        let offsetMap = Map.add_exn offsetMap ~key:id ~data:nextOffset in
        let reg = getParamReg paramNum in
        let s = Format.asprintf "  movl %%%s, %d(%%rbp)" reg nextOffset in  
        aux (res@[s]) t (nextOffset-4) offsetMap (paramNum+1)
  in aux res pLst nextOffset offsetMap 1

let genFun f = 
  let Fun (_,paramLst,Ident id,block) = f in
  let res = [id ^ ":"; "  pushq %rbp"; "  movq %rsp, %rbp"] in
  let (decls,_) = splitBlock block in
  let offsetMap = Map.empty (module String) in
  let len = getLocalsSize paramLst decls in
  let offset = (-4) in
  let res = pushStack res len in
  let (res,_,_) = pushParams res paramLst offset offsetMap in 
  res
(*
  ["pushq %rbp"; "movq %rsp, %rbp"; "popq %rbp"; "ret"] *)

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
  

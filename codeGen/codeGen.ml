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

let rec pushDecls res decls offset offsetMap = 
  match decls with
  | []  ->  (res,offset,offsetMap)
  | (Decl (Ident id,value))::t ->
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:value in
      let s = Format.asprintf "  movl $%d %d(%%rbp)" value offset in
      pushDecls (res@[s]) t (offset-4) offsetMap

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
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:(offset-(4*size)) in
      let (res,offset) = pushArrayVals res values offset in
      pushAllDecls res t offset offsetMap
  | _ -> (res,offset,offsetMap)

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
  let res = genBlock res block 
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
  

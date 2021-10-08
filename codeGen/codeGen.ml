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

let getOpInstr op = 
  match op with
  | Mul -> "imull"
  | Add -> "addl"
  | Sub -> "sub;"

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
      let offsetMap = Map.add_exn offsetMap ~key:id ~data:offset in
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
    if argNum > 6 then res else
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
    | (ArraySubscript ((Ident id), idx))::t ->
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
    | FunctionCall ((Ident id), eLst) ->
        let s = Format.asprintf "  call %s" id in
        let res = genFunCall res offsetMap eLst in
        res@[s]
    | _ -> res
  in 

  let rec aux' res stms offset offsetMap = 
  match stms with
    | []  -> res
    | (Stm_exp e)::t -> 
        let res = aux res e offset offsetMap in
        aux' res t offset offsetMap
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
  

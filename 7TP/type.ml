open Astcommon
open Astt

let mk_texpr e t = { expr=e; ty=t }

let rec check_types ty1 ty2 = match ty1, ty2 with
  | Tint, Tint -> ()
  (* Certains des autres cas sont à autoriser,
     d'autres doivent lever une exception. *)
  | _, _       -> failwith "Not implemented"
  
let rec type_expr = function
  | Astv.Econst c ->
    let ty = match c with
      | Cint _  -> Tint
      | Cbool _ -> Tbool
    in
    mk_texpr (Econst c) ty
      
  | Astv.Eunop (op, e) ->
    let e  = type_expr e in
    let ty = match op with
      | Uminus -> check_types Tint  e.ty; Tint
      | Not    -> check_types Tbool e.ty; Tbool
    in
    mk_texpr (Eunop(op, e)) ty

  | _ -> failwith "Not implemented"
      
      
and type_call (f, params) =
  failwith "Not implemented"
  
(* [ret] est le type éventuel attendu lors d'une instruction [return]. *)
let rec type_block ret b =
  List.map (type_instr ret) b

and type_instr ret = function

  | Astv.Iblock b ->
    let b = type_block ret b in
    Iblock b

  | Astv.Iwhile (e, b) ->
    let e = type_expr  e
    and b = type_block ret b
    in
    check_types Tbool e.ty;
    Iwhile (e, b)

  | Astv.Inewline -> Inewline

  | _ -> failwith "Not implemented"
      

let type_fun_descr fdescr =
  let ret = match fdescr.Astv.name with
      Astv.Function (_, _, fty) -> fty.return_ty
  in
  { name  = fdescr.Astv.name;
    body  = type_block ret fdescr.Astv.body;
    lvars = fdescr.Astv.lvars }
    
let type_prog p =
  let instrs = type_block None p.Astv.instrs
  and funs   = List.map type_fun_descr p.Astv.funs
  in
  { instrs = instrs;
    svars  = p.Astv.svars;
    funs   = funs }

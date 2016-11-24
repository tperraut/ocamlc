open Astcommon
open Astt

let mk_texpr e t = { expr=e; ty=t }

let rec check_types ty1 ty2 = match ty1, ty2 with
  | Tint, Tint -> ()
  | Tbool, Tbool -> ()
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

  | Astv.Ebinop (op, e1, e2) ->
     let e1 = type_expr e1 in
     let e2 = type_expr e2 in
     let ty = match op with
       | Plus | Minus | Mult | Div
       | Lt   | Le  | Gt | Ge      -> check_types Tint e1.ty;  check_types Tint e2.ty;  Tint;
       | And  | Or                 -> check_types Tbool e1.ty; check_types Tbool e2.ty; Tbool;
       | Eq   | Neq                -> check_types e1.ty e2.ty; Tbool;
     in
     mk_texpr (Ebinop(op, e1, e2)) ty

  | Astv.Evar v ->
     let nv = 
       match v with
       | Astv.Static_var (id, ident, ty) -> Evar (  id, ident, ty
       | Astv.Param (id, ident, ty) -> id, ident, ty
       | Astv.Local_var (id, ident, ty) -> id, ident, ty
     in
     mk_texpr () ty

  | Astv.Eif (op, e1, e2) ->
     let op = type_expr op in
     let e1 = type_expr e1 in
     let e2 = type_expr e2 in
     check_types Tbool op.ty;
     check_types e1.ty e2.ty;
     mk_texpr (Eif(op, e1, e2)) e1.ty
    
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

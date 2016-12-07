open Astcommon
open Astt

let mk_texpr e t = { expr=e; ty=t }

let rec splitpair l acc =
  match l with
  | [] -> acc
  | e::s -> splitpair s (e.ty::acc)
                 
let rec check_types ty1 ty2 = match ty1, ty2 with
  | Tint, Tint -> ()
  | Tbool, Tbool -> ()
  (* Certains des autres cas sont à autoriser,
     d'autres doivent lever une exception. *)
  | _, _       -> failwith "Type error"
  
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
     let (v, nt ) = 
       match v with
       | Astv.Static_var (id, ident, ty) -> Evar ( Astv.Static_var (id, ident, ty)), ty
       | Astv.Param (id, ident, ty) -> Evar ( Astv.Param (id, ident, ty)), ty
       | Astv.Local_var (id, ident, ty) -> Evar ( Astv.Local_var (id, ident, ty)), ty
     in
     mk_texpr v nt

  | Astv.Eif (op, e1, e2) ->
     let op = type_expr op in
     let e1 = type_expr e1 in
     let e2 = type_expr e2 in
     check_types Tbool op.ty;
     check_types e1.ty e2.ty;
     mk_texpr ( Eif(op, e1, e2)) e1.ty

  | Astv.Enewarr (ty, e) ->
     let e = type_expr e in
     check_types Tint e.ty;
     mk_texpr ( Enewarr(ty,e)) (Tarr (ty))

  | Astv.Egetarr (e1, e2) ->
     let e1 = type_expr e1 in
     let e2 = type_expr e2 in
     check_types Tint e2.ty;
     mk_texpr ( Egetarr(e1,e2)) e1.ty
  
  | Astv.Ecall (f, params) ->
     let typedparams = List.map type_expr params in
     let ff = let Astv.Function(_, _, fty) = f in
       fty
     in
     let fret =
       match ff.return_ty with
       | Some ty -> ty
       | None -> failwith "function have type Unit()"
     in
     let ftypeofparams = ff.params_ty in
     let typeofparams = splitpair typedparams [] in
     List.iter2 check_types ftypeofparams typeofparams;
     mk_texpr (Ecall (f, typedparams)) fret

      
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
     let e = type_expr  e in
     check_types Tbool e.ty;
     let b = type_block ret b in
     Iwhile (e, b)

  | Astv.Inewline -> Inewline
                   
  | Astv.Iassign (v, e) ->
     let vt = 
       match v with
       | Astv.Static_var (_, _, ty) ->  ty
       | Astv.Param (_, _, ty) -> ty
       | Astv.Local_var (_, _, ty) -> ty
    and e = type_expr e in
    check_types vt e.ty;
    Iassign (v, e)

  | Astv.Isetarr (e1,e2,e) ->  (* TODO fix array type*)
     let e1 = type_expr e1 in
     let e = type_expr e in
     check_types e1.ty e.ty;
     let e2 = type_expr e2 in
     check_types Tint e2.ty;
     Isetarr (e1, e2, e)

  | Astv.Iif (e, b1, b2) ->
     let e = type_expr e in
     check_types Tbool e.ty;
     let b1 = type_block ret b1 in
     let b2 = type_block ret b2 in (* Todo type of block *)
     Iif (e, b1, b2)

  | Astv.Iprint (e) ->
     let e = type_expr e in
     check_types Tint e.ty;
     Iprint (e)

  | Astv.Ireturn (e) ->
     let e = type_expr e in
     Ireturn (e)

  | Astv.Icall (f, params) ->
     let typedparams = List.map type_expr params in
     let ff = let Astv.Function(_, _, fty) = f in
              fty
     in
     let ftypeofparams = ff.params_ty in
     let typeofparams = splitpair typedparams [] in
     List.iter2 check_types ftypeofparams typeofparams;
     Icall (f, typedparams)

  | Astv.Iexit -> Iexit
     
  (*| _ -> failwith "Not implemented"*)
      

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

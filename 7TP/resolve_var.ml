open Astcommon
open Ast
open Printf

module Env = Map.Make(String)
type var_env = Astv.var Env.t
type fun_env = Astv.fname Env.t


module Vset = Set.Make(struct type t = Astv.var let compare = compare end)
type var_set = Vset.t

module Funset = Set.Make(struct type t = Astv.fun_descr let compare = compare end)
type fun_set = Funset.t
    
let new_svar : ident -> ty -> Astv.var =
  let c = ref 0 in
  fun (id: ident) (ty: ty) -> incr c; Astv.Static_var (!c, id, ty)

let new_fun : ident -> fty -> Astv.fname =
  let c = ref 0 in
  fun (id: ident) (fty: fty) -> incr c; Astv.Function (!c, id, fty)

    
let rec resolve_expr env fenv = function
  | Econst c ->
    Astv.Econst c
      
  | Eident id ->
    Astv.Evar (Env.find id env)
      
  | Eunop (op, e) ->
    Astv.Eunop (op, resolve_expr env fenv e)
      
  | Ebinop (binop, e1, e2) ->
    Astv.Ebinop (binop, resolve_expr env fenv e1, resolve_expr env fenv e2)
      
  | Eif (e1, e2, e3) ->
    Astv.Eif (resolve_expr env fenv e1, resolve_expr env fenv e2, resolve_expr env fenv e3)

  | Enewarr (ty, e) ->
    Astv.Enewarr (ty, resolve_expr env fenv e)

  | Egetarr (a, i) ->
    Astv.Egetarr (resolve_expr env fenv a, resolve_expr env fenv i)

  | Ecall c ->
    Astv.Ecall (resolve_call env fenv c)

and resolve_call env fenv (f, args) =
  Env.find f fenv, List.map (resolve_expr env fenv) args
      
let rec resolve_instr env fenv nxt_local = function
  | Idecl_var (id, ty)    ->
    let var, nxt_local = if nxt_local < 0
      then new_svar id ty, nxt_local
      else Astv.Local_var (nxt_local, id, ty), nxt_local + 1
    in
    None, Vset.singleton var, Funset.empty, Env.add id var env, fenv, nxt_local
      
  | Idecl_fun (id, params, fty, b) ->
    let new_param : ident -> ty -> Astv.var =
      let c = ref 0 in
      fun (id: ident) (ty: ty) -> incr c; Astv.Param (!c, id, ty)
    in
    let fn = new_fun id fty in
    let fenv = Env.add id fn fenv in
    let env2 = List.fold_right2
      (fun id ty -> Env.add id (new_param id ty)) params fty.params_ty env
    in
    let b, vset, _, _ = resolve_block env2 fenv 0 b in
    let fdescr = { Astv.name=fn; Astv.body=b; Astv.lvars=Vset.elements vset }
    in
    None, Vset.empty, Funset.singleton fdescr, env, fenv, nxt_local
      
  | Iassign (id, e) ->
    let svar = Env.find id env
    and e    = resolve_expr env fenv e
    in
    Some (Astv.Iassign (svar, e)), Vset.empty, Funset.empty, env, fenv, nxt_local
      
  | Isetarr (a, i, e) ->
    let a = resolve_expr env fenv a
    and i = resolve_expr env fenv i
    and e = resolve_expr env fenv e
    in
    Some (Astv.Isetarr (a, i, e)), Vset.empty, Funset.empty, env, fenv, nxt_local
      
  | Iblock b ->
    let is, vset, fset, nxt_local = resolve_block env fenv nxt_local b in
    Some (Astv.Iblock is), vset, fset, env, fenv, nxt_local
      
  | Iwhile (c, is) ->
    let c  = resolve_expr env fenv c
    and is, vset, fset, nxt_local = resolve_block env fenv nxt_local is
    in
    Some (Astv.Iwhile(c, is)), vset, fset, env, fenv, nxt_local
      
  | Iif (c, is1, is2) ->
    let c = resolve_expr env fenv c in
    let is1, vset1, fset1, nxt_local = resolve_block env fenv nxt_local is1 in
    let is2, vset2, fset2, nxt_local = resolve_block env fenv nxt_local is2 in
    Some (Astv.Iif(c, is1, is2)),
    Vset.union vset1 vset2,
    Funset.union fset1 fset2,
    env, fenv, nxt_local
      
  | Iprint e ->
    Some (Astv.Iprint (resolve_expr env fenv e)), Vset.empty, Funset.empty, env, fenv, nxt_local
      
  | Inewline ->
    Some Astv.Inewline, Vset.empty, Funset.empty, env, fenv, nxt_local
      
  | Icall c ->
    Some (Astv.Icall (resolve_call env fenv c)), Vset.empty, Funset.empty, env, fenv, nxt_local
      
  | Ireturn e ->
    Some (Astv.Ireturn (resolve_expr env fenv e)), Vset.empty, Funset.empty, env, fenv, nxt_local
      
  | Iexit    ->
    Some Astv.Iexit, Vset.empty, Funset.empty, env, fenv, nxt_local
      
	
and resolve_block env fenv nxt_local = function
  | [] -> [], Vset.empty, Funset.empty, nxt_local
  | i::is ->
    let i, vs1, fs1, env, fenv, nxt_local = resolve_instr env fenv nxt_local i in
    let is, vs2, fs2, nxt_local = resolve_block env fenv nxt_local is in
    let is = match i with
      | None -> is
      | Some i -> i :: is
    in
    is, Vset.union vs1 vs2, Funset.union fs1 fs2, nxt_local
      
let resolve_prog p =
  let is, vset, fset, _ = resolve_block Env.empty Env.empty (-1) p in
  { Astv.instrs = is;
    Astv.svars = Vset.elements vset;
    Astv.funs = Funset.elements fset; }

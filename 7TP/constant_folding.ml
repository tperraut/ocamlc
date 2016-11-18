open Astcommon
open Astv

(* On définit des "smart constructors" pour quelques cas dans lesquels des
   simplifications sont possibles :
   - expressions unaires
   - expressions binaires
   - branchement conditionnel
   - boucle conditionnelle
*)
let mk_unop op e =
  match op, e with
    | Uminus, Econst (Cint i) -> Econst (Cint (-i))
    | Not, Econst (Cbool b)   -> Econst (Cbool (not b))
    | _, _ -> Eunop (op, e)

let mk_binop op e1 e2 =
  match op, e1, e2 with
    (* Arithmétique. *)      
    | (Plus | Minus | Mult | Div), Econst (Cint i1), Econst (Cint i2) ->
      let op = match op with
	| Plus  -> ( + )
	| Minus -> ( - )
	| Mult  -> ( * )
	| Div   -> ( / )
	| _ -> assert false
      in Econst (Cint (op i1 i2))

    (* Comparaisons d'entiers. *) 
    | (Lt | Le | Gt | Ge), Econst (Cint i1), Econst (Cint i2) ->
      let op = match op with
	| Lt -> ( < )
	| Le -> ( <= )
	| Gt -> ( > )
	| Ge -> ( >= )
	| _ -> assert false
      in Econst (Cbool (op i1 i2))

    (* Tests d'égalité. *)
    | (Eq | Neq), Econst c1, Econst c2 ->
      let op = match op with
	| Eq  -> ( = )
	| Neq -> ( <> )
	| _ -> assert false
      in Econst (Cbool (op c1 c2))

    (* Sinon. *)
    | _, _, _ -> Ebinop(op, e1, e2)

let mk_Eif c e1 e2 =
  match c with
    | Econst (Cbool true)  -> e1
    | Econst (Cbool false) -> e2
    | _ -> Eif(c, e1, e2)

let mk_Iif c b1 b2 =
  match c with
    | Econst (Cbool true)  -> Iblock b1
    | Econst (Cbool false) -> Iblock b2
    | _ -> Iif(c, b1, b2)
      
let mk_while c b =
  match c with
    | Econst (Cbool false) -> Iblock []
    | _ -> Iwhile(c, b)

  (* Pour chaque constructeur, on effectue la simplification des expressions
     constantes sur les sous-expressions, et on applique un "smart constructor"
     quand il en existe un. *)
let rec fold_constants_expr = function
  | Econst c as e -> e
  | Evar v as e -> e
  | Eunop (op, e) ->
    mk_unop op
      (fold_constants_expr e)
  | Ebinop (op, e1, e2) ->
    mk_binop op
      (fold_constants_expr e1)
      (fold_constants_expr e2)
  | Eif (c, e1, e2) ->
    mk_Eif
      (fold_constants_expr c)
      (fold_constants_expr e1)
      (fold_constants_expr e2)
  | Enewarr (ty, e) -> Enewarr (ty, fold_constants_expr e)
  | Egetarr (a, e) -> Egetarr (fold_constants_expr a,
			       fold_constants_expr e)
  | Ecall call -> Ecall (fold_constants_call call)

and fold_constants_call (fname, args) =
  (fname, List.map fold_constants_expr args)
    
let rec fold_constants_instr = function
  | Iassign (v, e) ->
    Iassign (v, fold_constants_expr e)
  | Isetarr (a, i, e) ->
    Isetarr (fold_constants_expr a,
	     fold_constants_expr i,
	     fold_constants_expr e)
  | Iblock b -> Iblock (fold_constants_block b)
  | Iwhile (c, b) ->
    mk_while
      (fold_constants_expr c)
      (fold_constants_block b)  
  | Iif (c, b1, b2) ->
    mk_Iif
      (fold_constants_expr c)
      (fold_constants_block b1)
      (fold_constants_block b2)
  | Iprint e -> Iprint (fold_constants_expr e)
  | Inewline -> Inewline
  | Ireturn e -> Ireturn (fold_constants_expr e)
  | Icall call -> Icall (fold_constants_call call)
  | Iexit -> Iexit
    
and fold_constants_block (b: block) =
  List.map fold_constants_instr b

let fold_constants (p: prog) =
  { instrs = fold_constants_block p.instrs;
    svars  = p.svars;
    funs   = List.map
      (fun f -> { f with body = fold_constants_block f.body })
      p.funs
  }    

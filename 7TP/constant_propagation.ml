open Astcommon
open Astv
  
module Venv = Map.Make(struct type t = Astv.var let compare = compare end)
type reaching_defs = One of expr | More
type def_env = reaching_defs Venv.t

type def_annot = { mutable def_in:  def_env;
		   mutable def_out: def_env }

type annot_instr = { i: a_instr;
		     annot: def_annot }
and a_block = annot_instr list
and a_instr =
  | Aassign   of var  * expr
  | Asetarr   of expr * expr  * expr
  | Ablock    of a_block
  | Awhile    of expr * a_block
  | Aif       of expr * a_block * a_block
  | Aprint    of expr
  | Anewline
  | Areturn   of expr
  | Acall     of call
  | Aexit
    
let fuzzy_defs vars =
  List.fold_right (fun v env -> Venv.add v More env) vars Venv.empty

(* INITIALISATION *)

(* Remarque/Erratum.
   L'énoncé ici était imprécis vis-à-vis de l'initialisation des variables.

   En supposant qu'une variable déclarée mais non initialisée peut avoir une
   valeur arbitraire (c'est le cas en C), il faut que le dictionnaire initial
   indique que les valeurs de toutes les variables sont inconnues, en
   utilisant [fuzzy_defs] sur l'ensemble des variables.
   Ce corrigé choisit cette voie.x

   Si à l'inverse on suppose comme en Java que toute variable vaut
   initialement [0], alors il faut que le dictionnaire initial indique que
   chaque variable connue vaut [0], en utilisant une variante de la
   fonction [fuzzy_defs].
*)
let annot_instr vars i =
  let env = fuzzy_defs vars in
  { i = i; annot = { def_in = env; def_out = env } }

let rec init_instr vars = function
  | Iassign (v, e)    -> annot_instr vars (Aassign (v, e))
  | Isetarr (a, i, e) -> annot_instr vars (Asetarr (a, i, e))
  | Iblock b          -> annot_instr vars (Ablock (init_block vars b))
  | Iwhile (c, b)     -> annot_instr vars (Awhile (c, init_block vars b))
  | Iif (c, b1, b2)   -> annot_instr vars (Aif (c, init_block vars b1, init_block vars b2))
  | Iprint e          -> annot_instr vars (Aprint e)
  | Inewline          -> annot_instr vars Anewline
  | Ireturn e         -> annot_instr vars (Areturn e)
  | Icall call        -> annot_instr vars (Acall call)
  | Iexit             -> annot_instr vars Aexit
and init_block vars b =
  List.map (init_instr vars) b


(* POINT FIXE *)
let merge_rule _ o1 o2 = match o1, o2 with
  | None, None -> None
  | None, Some d | Some d, None -> Some d
  | Some _, Some _ -> Some More
    
let rec propagate_instr vars pred_out a =
  a.annot.def_in <- pred_out;
  begin
    match a.i with
      | Aassign (v, e)    -> a.annot.def_out <- Venv.add v (One e) pred_out
	
      | Asetarr (_, _, _) -> a.annot.def_out <- pred_out
	
      | Ablock b          -> a.annot.def_out <- propagate_block vars pred_out b

      | Awhile (_, b)     ->
	a.annot.def_out <- propagate_block vars pred_out b;
	a.annot.def_in  <- Venv.merge merge_rule a.annot.def_in a.annot.def_out;
	a.annot.def_out <- propagate_block vars a.annot.def_in b

      | Aif (_, b1, b2)   ->
	a.annot.def_out <- Venv.merge merge_rule
	  (propagate_block vars pred_out b1)
	  (propagate_block vars pred_out b2)
	
      | Aprint _          -> a.annot.def_out <- pred_out
      | Anewline          -> a.annot.def_out <- pred_out
      | Areturn _         -> a.annot.def_out <- pred_out

      | Acall _           ->
	a.annot.def_out <- fuzzy_defs vars

      | Aexit             -> a.annot.def_out <- pred_out
	
  end;
  a.annot.def_out
	
and propagate_block vars pred_out l =
  List.fold_left (fun p_out i -> propagate_instr vars p_out i) pred_out l


(* PROPAGATION *)
let rec strip_a annot = strip_i annot.annot.def_in annot.i
and     strip_b block = List.map strip_a block
and     strip_i defs  = function
  | Aassign (v, e)    -> Iassign (v, strip_e defs e)
  | Asetarr (a, i, e) -> Isetarr (strip_e defs a, strip_e defs i, strip_e defs e)
  | Ablock b          -> Iblock (strip_b b)
  | Awhile (c, b)     -> Iwhile (c, strip_b b)
  | Aif (c, b1, b2)   -> Iif (c, strip_b b1, strip_b b2)
  | Aprint e          -> Iprint (strip_e defs e)
  | Anewline          -> Inewline
  | Areturn e         -> Ireturn (strip_e defs e)
  | Acall c           -> Icall (strip_call defs c)
  | Aexit             -> Iexit
and     strip_e defs e = match e with
  | Econst _ -> e
  | Evar var ->
    begin
      try match Venv.find var defs with
	| One ((Econst _) as c) -> c
	| _                     -> e
      with Not_found            -> e
    end
  | Eunop (op, e) -> Eunop (op, strip_e defs e)
  | Ebinop (op, e1, e2) -> Ebinop (op, strip_e defs e1, strip_e defs e2)
  | Eif (c, e1, e2) -> Eif (strip_e defs c, strip_e defs e1, strip_e defs e2)
  | Enewarr (ty, e) -> Enewarr (ty, strip_e defs e)
  | Egetarr (a, i) -> Egetarr (strip_e defs a, strip_e defs i)
  | Ecall call -> Ecall (strip_call defs call)
and     strip_call defs (f, args) =
  f, List.map (strip_e defs) args

    
let propagate_constants prog =
  let a_prog = init_block prog.svars prog.instrs in
  let _ = propagate_block prog.svars Venv.empty a_prog in
  let result = strip_b a_prog in
  { instrs = result; svars = prog.svars; funs = prog.funs }

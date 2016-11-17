module Venv : Map.S with type key = Astv.var
    
type reaching_defs = One of Astv.expr | More
    
type def_env = reaching_defs Venv.t

type def_annot = { mutable def_in  : def_env;
		   mutable def_out : def_env; }

type annot_instr = { i : a_instr; annot : def_annot; }
    
and a_block = annot_instr list
    
and a_instr =
  | Aassign of Astv.var * Astv.expr
  | Asetarr of Astv.expr * Astv.expr * Astv.expr
  | Ablock  of a_block
  | Awhile  of Astv.expr * a_block
  | Aif     of Astv.expr * a_block * a_block
  | Aprint  of Astv.expr
  | Anewline
  | Areturn of Astv.expr
  | Acall   of Astv.call
  | Aexit
      
val fuzzy_defs : Venv.key list -> reaching_defs Venv.t
  
val annot_instr : a_instr -> annot_instr
  
val init_instr : Astv.instr -> annot_instr
val init_block : Astv.block -> a_block
  
val merge_rule :
  'a -> reaching_defs option -> reaching_defs option -> reaching_defs option
  
val propagate_instr : Venv.key list -> def_env -> annot_instr -> def_env
val propagate_block : Venv.key list -> def_env -> a_block -> def_env
  
val strip_a : annot_instr -> Astv.instr
val strip_b : annot_instr list -> Astv.instr list
val strip_i : def_env -> a_instr -> Astv.instr
val strip_e : def_env -> Astv.expr -> Astv.expr
val strip_call : def_env -> Astv.call -> Astv.call
  
val propagate_constants : Astv.prog -> Astv.prog

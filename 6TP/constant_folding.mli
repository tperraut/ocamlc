val mk_unop : Astcommon.unop -> Astv.expr -> Astv.expr
val mk_binop : Astcommon.binop -> Astv.expr -> Astv.expr -> Astv.expr

val fold_constants_expr : Astv.expr -> Astv.expr
val fold_constants_instr : Astv.instr -> Astv.instr
val fold_constants_block : Astv.block -> Astv.block

val fold_constants : Astv.prog -> Astv.prog

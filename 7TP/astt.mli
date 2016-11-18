open Astcommon

type var = Astv.var
type fname = Astv.fname

type typed_expr = { expr: expr; ty: ty }
and  expr =
  | Econst  of const
  | Evar    of var
  | Eunop   of unop       * typed_expr
  | Ebinop  of binop      * typed_expr * typed_expr
  | Eif     of typed_expr * typed_expr * typed_expr
  | Enewarr of ty         * typed_expr
  | Egetarr of typed_expr * typed_expr
  | Ecall   of call
and call = fname * typed_expr list
      
type block = instr list
and  instr =
  | Iassign   of var        * typed_expr
  | Isetarr   of typed_expr * typed_expr * typed_expr
  | Iblock    of block
  | Iwhile    of typed_expr * block
  | Iif       of typed_expr * block * block
  | Iprint    of typed_expr
  | Inewline
  | Ireturn   of typed_expr
  | Icall     of call
  | Iexit

type fun_descr = { name: fname; body: block; lvars: var list }
    
type prog = { instrs: instr list;
	      svars:  var list;
	      funs:   fun_descr list; }

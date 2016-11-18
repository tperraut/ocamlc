open Astcommon

type var = Static_var   of int * Ast.ident * ty
	   | Param      of int * Ast.ident * ty
	   | Local_var  of int * Ast.ident * ty
type fname = Function   of int * Ast.ident * fty
    
type expr =
  | Econst  of const
  | Evar    of var
  | Eunop   of unop  * expr
  | Ebinop  of binop * expr * expr
  | Eif     of expr  * expr * expr
  | Enewarr of ty    * expr
  | Egetarr of expr  * expr
  | Ecall   of call
and call = fname * expr list

type block = instr list
and  instr =
  | Iassign   of var  * expr
  | Isetarr   of expr * expr  * expr
  | Iblock    of block
  | Iwhile    of expr * block
  | Iif       of expr * block * block
  | Iprint    of expr
  | Inewline
  | Ireturn   of expr
  | Icall     of call
  | Iexit

type fun_descr = { name: fname; body: block; lvars: var list }
      
type prog = { instrs: instr list;
	      svars: var list;
	      funs: fun_descr list; }

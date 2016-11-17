open Astcommon
open Astv
open Printf
  
let push: int -> unit =
  printf "  sub $sp, $sp, 4\n  sw $a%d, 0($sp)\n"

let peek: int -> unit =
  printf "  lw $a%d, 0($sp)\n"

let pop: int -> unit =
  printf "  lw $a%d, 0($sp)\n  add $sp, $sp, 4\n"

let new_label : unit -> string =
  let c = ref 0 in
  fun () -> incr c; sprintf "__label__%05i" !c

let get_label : Astv.var -> string = function
  | Astv.Static_var (n, id) -> sprintf "__var__%05i__%s" n id
  | _ -> assert false

let get_fun_label : Astv.fname -> string = function
  | Astv.Function (n, id) -> sprintf "__fun__%05i__%s" n id
    
let rec generate_expr (e : Astv.expr) : unit = 
  match e with
      
    | Econst (Cint i)  -> printf "  li $a0, %d\n" i
    | Econst (Cbool b) -> printf "  li $a0, %d\n" (if b then 1 else 0)

    | Evar var ->
      begin match var with
	| Static_var _ -> printf "  la $a0, %s\n  lw $a0, 0($a0)\n" (get_label var)
	| Param (n, _) -> failwith "Not implemented"
	| Local_var (n, _)  -> failwith "Not implemented"
      end

    | Eunop (Uminus, e) ->
      generate_expr e;
      printf "  neg $a0, $a0\n"
    | Eunop (Not, e) ->
      generate_expr e;
      printf "  li $a1, 1\n  sub $a0, $a1, $a0\n"
      
    | Ebinop ((Plus | Mult) as op, Econst (Cint i), e)
    | Ebinop ((Plus | Mult | Minus | Div) as op, e, Econst (Cint i))
	when -32768 <= i && i < 32768 ->
      generate_expr e;
      let op = match op with
	| Plus -> "add"
	| Mult -> "mul"
	| Minus -> "sub"
	| Div  -> "div"
	| _    -> assert false
      in
      printf "  %s $a0, $a0, %d\n" op i
	
    | Ebinop (op, e1, e2) ->
      generate_expr e1;
      push 0;
      generate_expr e2;
      pop 1;
      let op = match op with
	| Plus -> "add"
	| Mult -> "mul"
	| Minus -> "sub"
	| Div  -> "div"
	| Eq   -> "seq"
	| Neq  -> "sne"
	| Lt   -> "slt"
	| Le   -> "sle"
	| Gt   -> "sgt"
	| Ge   -> "sge"
	| And  -> "and"
	| Or   -> "or"
      in
      printf "  %s $a0, $a1, $a0\n" op

    | Eif (c, e_then, e_else) ->
      let else_label = new_label()
      and end_label  = new_label()
      in
      generate_expr c;
      printf "  beqz $a0, %s\n" else_label;
      generate_expr e_then;
      printf "  b %s\n" end_label;
      printf "%s:\n" else_label;
      generate_expr e_else;
      printf "%s:\n" end_label

    | Enewarr e ->
      generate_expr e;
      printf "  add $a0, $a0, 1\n  mul $a0, $a0, 4\n";
      printf "  jal malloc\n"

    | Egetarr (e, i) ->
      generate_expr e;
      push 0;
      generate_expr i;
      printf "  mul $a0, $a0, 4\n";
      pop 1;
      printf "  add $a0, $a1, $a0\n";
      printf "  lw  $a0, 0($a0)\n"

    | Ecall c ->
      generate_call c

and generate_call (f, params) =
  (* On a besoin ici du code correspondant aux actions de l'appelant. *)
  failwith "Not implemented"

(* Génération de code pour les blocs. *)
let rec generate_instr : instr -> unit = function

  | Iassign (var, e) ->
    generate_expr e;
    begin match var with
      | Static_var _ ->
	printf "  la $a1, %s\n  sw $a0, 0($a1)\n" (get_label var)
      | Local_var (n, id) -> failwith "Not implemented"
      | Param _ -> failwith "Not  implemented"
    end
      
  | Isetarr (a, i, e) ->
    generate_expr a;
    push 0;
    generate_expr i;
    push 0;
    generate_expr e;
    pop 1;
    printf "  mul $a1, $a1, 4\n";
    pop 2;
    printf "  add $a1, $a2, $a1\n";
    printf "  sw  $a0, 0($a1)\n"
	
  | Iblock b      -> generate_block b
    
  | Iwhile (c, b) ->
    let cond_label = new_label()
    and end_label  = new_label()
    in
    printf "%s:\n" cond_label;
    generate_expr c;
    printf "  beqz $a0, %s\n" end_label;
    generate_block b;
    printf "  b %s\n" cond_label;
    printf "%s:\n" end_label

  (* Construction ajoutée aujourd'hui. *)
  | Iif (c, b_then, b_else) ->
    (* Création de deux étiquettes pour le début et la fin du bloc "else". *)
    let else_label = new_label()
    and end_label  = new_label()
    in
    (* Évaluation de la condition. *)
    generate_expr c;
    (* Si le résultat est [false], c'est-à-dire [0], sauter au bloc "else". *)
    printf "  beqz $a0, %s\n" else_label;
    (* Sinon, on passe à l'instruction suivante, qui est le bloc "then". *)
    generate_block b_then;
    (* À la fin du bloc "then", sauter à la fin du branchement. *)
    printf "  b %s\n" end_label;
    (* Début du bloc "else". *)
    printf "%s:\n" else_label;
    generate_block b_else;
    (* Fin du branchement. *)
    printf "%s:\n" end_label

  | Iprint e ->
    (* Calcul de l'expression, puis appel système d'affichage. *)
    generate_expr e;
    printf "  li $v0, 1\n  syscall\n"
      
  | Inewline ->
    (* Appel système pour l'affichage du caractère '\n' *)
    printf "  li $v0, 11\n  li $a0, 10\n  syscall\n"
      
  | Iexit ->
    (* Appel système de fin du programme. *)
    printf "  li $v0, 10\n  syscall\n"

  | Icall c ->
    generate_call c

  | Ireturn e ->
    generate_expr e
      
and generate_block (b: block) : unit =
  List.iter generate_instr b

let generate_fun (fdescr: fun_descr) : unit =
  (* On a besoin ici du code correspondant aux actions de l'appelée. *)
  failwith "Not implemented"

let init () = printf "
  li  $a0, 1024       # Appel système sbrk pour réserver 1024 octets.
  li  $v0, 9
  syscall

  la  $a0, nxt_loc    # L'appel système a placé dans $v0 l'adresse de début
  sw  $v0, 0($a0)     # de la zone réservée, à sauvegarder dans nxt_loc.

  add $v0, $v0, 1024  # Calcul de max_loc, 1024 octets plus loin.
  la  $a0, max_loc   
  sw  $v0, 0($a0)
                      # Initialisation terminée.

"

let system_vars () = printf "
nxt_loc:
  .word 0
max_loc:
  .word 0
"

let end_exec () = printf "
end_exec:                       # Fin de l'exécution
  li $v0, 10
  syscall
"
    
let built_ins () = printf "
malloc:
  la   $v0, nxt_loc             # Sauvegarde de l'adresse de début de bloc
  lw   $v1, 0($v0)

  add  $a1, $v1, $a0            # Calcul de l'adresse de fin...
  la   $a2, max_loc
  lw   $a2, 0($a2)
  bgt  $a1, $a2, out_of_memory  # ... et arrêt en cas de dépassement

  sw   $a1, 0($v0)              # Sinon confirmation de l'allocation
  move $a0, $v1
  jr   $ra                      # et retour au point d'appel

out_of_memory:                  # Affichage d'un message d'erreur
  la $a0, __const_out_of_memory
  li $v0, 4
  syscall
  b end_exec
"

let constants () = printf "
__const_out_of_memory:
  .asciiz \"out of memory\"
"

let generate_prog (p : Astv.prog) : unit =
  (* Le code. *)  
  printf "  .text\nmain:\n";
  (* 1. Initialisation. *)
  init ();
  (* 2. Le programme lui-même. *)
  List.iter generate_instr p.instrs;
  end_exec ();
  (* 2'. Les fonctions définies par l'utilisateur. *)
  List.iter generate_fun p.funs;
  (* 3. Les fonctions primitives. *)
  built_ins ();

  (* Les données. *)
  printf "  .data\n";
  (* 1. Les variables utilisées par les primitives. *)
  system_vars ();
  constants ();
  (* 2. Les variables de l'utilisateur. *)
  List.iter (fun var -> printf "%s:\n  .word 0\n" (get_label var)) p.svars

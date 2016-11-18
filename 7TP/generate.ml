open Astcommon
open Astt
open Printf
  
let push =
  printf "  sub $sp, $sp, 4\n  sw $a%d, 0($sp)\n"

let peek =
  printf "  lw $a%d, 0($sp)\n"

let pop =
  printf "  lw $a%d, 0($sp)\n  add $sp, $sp, 4\n"

let new_label =
  let c = ref 0 in
  fun () -> incr c; sprintf "__label__%05i" !c

(* Association d'une étiquette à une variable statique. *)
let get_label = function
  | Astv.Static_var (n, id, _) -> sprintf "__var__%05i__%s" n id
  | _ -> assert false

let get_fun_label  = function
  | Astv.Function (n, id, _) -> sprintf "__fun__%05i__%s" n id
    
let rec generate_expr e = 
  match e.expr with
      
    | Econst (Cint i)  -> printf "  li $a0, %d\n" i
    (* On représente [true] par l'entier [1] et [false] par l'entier [0]. *)
    | Econst (Cbool b) -> printf "  li $a0, %d\n" (if b then 1 else 0)

    (* On charge l'adresse de la variable avec [la] puis on lit la valeur à
       cette adresse avec [lw]. L'étiquette de la variable est donnée par la
       fonction [get_label]. *)
    | Evar var ->
      begin match var with
	| Astv.Static_var _ -> printf "  la $a0, %s\n  lw $a0, 0($a0)\n" (get_label var)
	| Astv.Param (n, _, _) -> printf "  lw $a0, %d($fp)\n" (4 * (n+1))
	| Astv.Local_var (n, _, _)  -> printf "  lw $a0, %d($fp)\n" (-4 * (n+1))
      end

    (* Le [-] unaire est directement représenté par [neg].
       Pour la négation logique, on traduit [Not e] par [1 - e]. *)
    | Eunop (Uminus, e) ->
      generate_expr e;
      printf "  neg $a0, $a0\n"
    | Eunop (Not, e) ->
      generate_expr e;
      printf "  li $a1, 1\n  sub $a0, $a1, $a0\n"
      
    | Ebinop ((Plus | Mult) as op,
	      { expr = Econst (Cint i); ty = _ },
	      e)
    | Ebinop ((Plus | Mult | Minus | Div) as op,
	      e,
	      { expr = Econst (Cint i); ty = _ })
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
	(* Les autres opérateurs binaires correspondent directement
	   à des instructions MIPS. *)
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
      (* Création de deux étiquettes pour le début et la fin du bloc "else". *)
      let else_label = new_label()
      and end_label  = new_label()
      in
      (* Évaluation de la condition. *)
      generate_expr c;
      (* Si le résultat est [false], c'est-à-dire [0], sauter au bloc "else". *)
      printf "  beqz $a0, %s\n" else_label;
      (* Sinon, on passe à l'instruction suivante, qui est le bloc "then". *)
      generate_expr e_then;
      (* À la fin du bloc "then", sauter à la fin du branchement. *)
      printf "  b %s\n" end_label;
      (* Début du bloc "else". *)
      printf "%s:\n" else_label;
      generate_expr e_else;
      (* Fin du branchement. *)
      printf "%s:\n" end_label

    | Enewarr (_, e) ->
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

and generate_call = function
  | f, args ->
    List.iter (fun arg -> generate_expr arg; push 0) args;
    printf "  jal %s\n" (get_fun_label f);
    printf "  add $sp, $sp, %d\n" (List.length args * 4)

(* Génération de code pour les blocs. *)
let rec generate_instr = function

  | Iassign (var, e) ->
    (* Affectation : d'abord calcul de l'expression [e], puis mise à jour de la
       variable. L'adresse de la variable est obtenue avec [la] et l'étiquette
       donnée par [get_label], puis la mise à jour est effectuée avec [sw]. *)
    generate_expr e;
    begin match var with
      | Astv.Static_var _ ->
	printf "  la $a1, %s\n  sw $a0, 0($a1)\n" (get_label var)
      | Astv.Local_var (n, id, _) ->
	printf "  sw $a0, %d($fp)\n" (-4 * (n+1))
      | Astv.Param _ -> failwith "Invalid assignment"
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
      
and generate_block b =
  List.iter generate_instr b

let generate_fun fdescr =
  let local_size = 4 * (List.length fdescr.lvars) in
  printf "
%s:\n
  sub  $sp, $sp, 8     # Sauvegarde de $ra et $fp
  sw   $ra, 0($sp)
  sw   $fp, 4($sp)
  move $fp, $sp        # Définition du nouveau $fp
  sub  $sp, $sp, %d    # Allocation des variables locales
" (get_fun_label fdescr.name) local_size;
  generate_block fdescr.body;
  printf "
  add  $sp, $sp, %d    # Désallocation des variables locales
  lw   $ra, 0($sp)     # Restauration de $ra et $fp
  lw   $fp, 4($sp)
  add  $sp, $sp, 8
  jr   $ra             # Retour à l'appelant
" local_size

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

let generate_prog p =
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

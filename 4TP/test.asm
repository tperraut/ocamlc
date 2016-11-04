  .text
main:

  li  $a0, 1024       # Appel système sbrk pour réserver 1024 octets.
  li  $v0, 9
  syscall

  la  $a0, nxt_loc    # L'appel système a placé dans $v0 l'adresse de début
  sw  $v0, 0($a0)     # de la zone réservée, à sauvegarder dans nxt_loc.

  add $v0, $v0, 1024  # Calcul de max_loc, 1024 octets plus loin.
  la  $a0, max_loc   
  sw  $v0, 0($a0)
                      # Initialisation terminée.

  li $a0, 18
  la $a1, __var__00001__y
  sw $a0, 0($a1)
  li $a0, 1
  la $a1, __var__00002__x
  sw $a0, 0($a1)
__label__00001:
  la $a0, __var__00002__x
  lw $a0, 0($a0)
  sub $sp, $sp, 4
  sw $a0, 0($sp)
  li $a0, 10
  lw $a1, 0($sp)
  add $sp, $sp, 4
  sle $a0, $a1, $a0
beqz $a0, __label__00002
  la $a0, __var__00002__x
  lw $a0, 0($a0)
  sub $sp, $sp, 4
  sw $a0, 0($sp)
  la $a0, __var__00001__y
  lw $a0, 0($a0)
  lw $a1, 0($sp)
  add $sp, $sp, 4
  add $a0, $a1, $a0
  li $v0, 1
  syscall
  li $v0, 11
  li $a0, 10
  syscall
  la $a0, __var__00002__x
  lw $a0, 0($a0)
  add $a0, $a0, 1
  la $a1, __var__00002__x
  sw $a0, 0($a1)
j __label__00001
  __label__00002:
  li $v0, 11
  li $a0, 10
  syscall
  li $v0, 10
  syscall

end_exec:                       # Fin de l'exécution
  li $v0, 10
  syscall

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
  .data

nxt_loc:
  .word 0
max_loc:
  .word 0

__const_out_of_memory:
  .asciiz "out of memory"
__var__00001__y:
  .word 0
__var__00002__x:
  .word 0

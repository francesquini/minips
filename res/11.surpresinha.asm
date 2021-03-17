.data

         texto:  .asciiz "A Universidade Federal do ABC (UFABC) é uma instituição pública federal de ensino superior no ABC paulista."
    disciplina:  .ascii  "Arquitetura de Computadores."

.text

main:
    la      $t0, imprime
    la      $t1, disciplina
    srl     $t1, $t1, 2
    sll     $t1, $t1, 2
    addi    $t1, $t1, 4
    lw      $t2, 0($t0)
    sw      $t2, 0($t1)
    lw      $t2, 4($t0)
    sw      $t2, 4($t1)
    lw      $t2, 8($t0)
    sw      $t2, 8($t1)
    lw      $t2, 12($t0)
    sw      $t2, 12($t1)
    lw      $t2, 16($t0)
    sw      $t2, 16($t1)
    lw      $t2, 20($t0)
    sw      $t2, 20($t1)
    jalr    $t1
    nop
    addi    $v0, $zero, 10  # exit syscall code = 10
    syscall

imprime:
    # Imprime texto
    la       $a0, texto   # coloca em a0 o parâmetro da chamada a syscall
    li       $v0, 4       # 4 é o código para imprimir uma string
    syscall
    jr      $ra
    nop

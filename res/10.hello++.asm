.data
         texto:  .asciiz "Que milagre o senhor por aqui..."
.text

main:
    la      $t0, imprime
    la      $s0, aqui
    jalr    $s0, $t0
    nop
aqui:
    addi    $v0, $zero, 10  # exit syscall code = 10
    syscall

imprime:
    # Imprime prompt
    la       $a0, texto   # coloca em a0 o parâmetro da chamada a syscall
    li       $v0, 4       # 4 é o código para imprimir uma string
    syscall
    jr       $s0
    nop

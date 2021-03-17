#Código mínimo. Soma 2 números fixos, imprime o resultado e sai

    addi $t0, $zero, 3
    addi $t1, $zero, 4
    add  $s0, $t0, $t1      # $s0 = 3 + 4

    # Imprime resultado
    addi $v0, $zero, 1      # print_int syscall code = 1
    add  $a0, $zero, $s0
    syscall

    # Sai do programa
    addi    $v0, $zero, 10  # exit syscall code = 10
    syscall

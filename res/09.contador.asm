# O contador

.data

.text

j main
nop

conta:
    addi $v0, $zero, 1
    la   $t0, conta
    lw   $t1, 0($t0)
    addi $t1, $t1, 1
    sw   $t1, 0($t0)
    jr   $ra
    nop

main:
    jal  conta
    nop
    move $a0, $v0
    li   $v0, 1      # print_int syscall code = 1
    syscall
    slti $t0, $a0, 9
    beq  $t0, $zero, fim    # roda 10 vezes
    nop
    j    main
    nop
fim:
    # Sai do programa
    addi $v0, $zero, 10  # exit syscall code = 10
    syscall

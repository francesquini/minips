.data
    x_len: .word 10
    x: .word 2, 1, 5, 9, 7, 8, 4, 0, 3, 6
    prompt1: .asciiz "Vetor antes: "
    prompt2: .asciiz "Vetor depois: "

.text
    j main
    nop

separa:
    addiu   $sp,$sp,-32
    sw      $fp,28($sp)
    move    $fp,$sp
    sw      $4,32($fp)
    sw      $5,36($fp)
    sw      $6,40($fp)
    lw      $2,40($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $2,0($2)
    sw      $2,16($fp)
    lw      $2,36($fp)
    sw      $2,8($fp)
    lw      $2,36($fp)
    sw      $2,12($fp)
    beq     $zero, $zero, $L2
    nop
$L4:
    lw      $2,12($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $3,0($2)
    lw      $2,16($fp)
    slt     $2,$2,$3
    bne     $2,$0,$L3
    nop
    lw      $2,8($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $2,0($2)
    sw      $2,20($fp)
    lw      $2,8($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $3,12($fp)
    sll     $3,$3,2
    lw      $4,32($fp)
    addu    $3,$4,$3
    lw      $3,0($3)
    sw      $3,0($2)
    lw      $2,12($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $3,20($fp)
    sw      $3,0($2)
    lw      $2,8($fp)
    addiu   $2,$2,1
    sw      $2,8($fp)
$L3:
    lw      $2,12($fp)
    addiu   $2,$2,1
    sw      $2,12($fp)
$L2:
    lw      $3,12($fp)
    lw      $2,40($fp)
    slt     $2,$3,$2
    bne     $2,$0,$L4
    nop
    lw      $2,8($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $2,0($2)
    sw      $2,20($fp)
    lw      $2,8($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $3,40($fp)
    sll     $3,$3,2
    lw      $4,32($fp)
    addu    $3,$4,$3
    lw      $3,0($3)
    sw      $3,0($2)
    lw      $2,40($fp)
    sll     $2,$2,2
    lw      $3,32($fp)
    addu    $2,$3,$2
    lw      $3,20($fp)
    sw      $3,0($2)
    lw      $2,8($fp)
    move    $sp,$fp
    lw      $fp,28($sp)
    addiu   $sp,$sp,32
    jr       $31
    nop

quicksort:
    addiu   $sp,$sp,-40
    sw      $31,36($sp)
    sw      $fp,32($sp)
    move    $fp,$sp
    sw      $4,40($fp)
    sw      $5,44($fp)
    sw      $6,48($fp)
    lw      $3,44($fp)
    lw      $2,48($fp)
    slt     $2,$3,$2
    beq     $2,$0,$L8
    nop
    lw      $6,48($fp)
    lw      $5,44($fp)
    lw      $4,40($fp)
    jal     separa
    nop
    sw      $2,24($fp)
    lw      $2,24($fp)
    addiu   $2,$2,-1
    move    $6,$2
    lw      $5,44($fp)
    lw      $4,40($fp)
    jal     quicksort
    nop
    lw      $2,24($fp)
    addiu   $2,$2,1
    lw      $6,48($fp)
    move    $5,$2
    lw      $4,40($fp)
    jal     quicksort
    nop

$L8:
    move    $sp,$fp
    lw      $31,36($sp)
    lw      $fp,32($sp)
    addiu   $sp,$sp,40
    jr      $31
    nop

print_v:
    move $t0, $zero      #t0 é o indice
    lw $t1, x_len        #t1 é o limite
    la $t3, x            #t3 guarda o endereco base
print_v_loop:
    sll $t4, $t0, 2             #t4 é o endereço do elemento
    add $t4, $t4, $t3
    lw $a0, 0($t4)
    li $v0, 1 # 1 é o código para imprimir um inteiro
    syscall
    li $a0,' '    # Imprime espaço
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall
    addi $t0, $t0, 1
    beq   $t0, $t1, print_v_end
    nop
    j print_v_loop
    nop
print_v_end:
    jr $ra
    nop

main:

    la $a0, prompt1  # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4       # 4 é o código para imprimir uma string
    syscall
    jal print_v
    nop
    li $a0,'\n'    # Imprime quebra de linha
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    la      $a0, x
    li      $a1, 0
    lw      $a2, x_len
    jal     quicksort
    nop

    la $a0, prompt2  # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4       # 4 é o código para imprimir uma string
    syscall
    jal print_v
    nop
    li $a0,'\n'    # Imprime quebra de linha
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    #Finaliza execução
    li $v0, 10  # exit syscall code = 10
    syscall

# Seção de dados
.data
    msg:       .asciiz "O valor impresso no final deve ser 4 e não deve imprimir 'Erro'.\n"
    errotext:  .asciiz "Erro\n"
    oktext:    .asciiz "Valor final: "

#Seção de texto
.text
    la $a0, msg # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall

    li $s5, 0

    beq $zero, $zero, um
    nop
    addi $s5, $s5, 1
    la $a0, errotext # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall
    j erro
    nop
um:
    addi $s5, $s5, 1

    li $t1, 42
    li $s1, 43
    beq $t1, $s1, erro
    nop
    addi $s5, $s5, 1

    li $t1, 42
    li $s1, 42
    bne $t1, $s1, erro
    nop
    addi $s5, $s5, 1

    li $s1, 43
    bne $t1, $s1, dois
    nop
    addi $s5, $s5, 1
    la $a0, errotext # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall


dois:
    li $s1, 73
    li $s2, 0
    move $zero, $s1
    beq $s1, $zero, erro
    nop
    addi $s5, $s5, 1

    j saida

erro:
    la $a0, errotext # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall

saida:

    la $a0, oktext # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall

    move $a0, $s5
    li $v0, 1
    syscall

    # Imprime \n
    li $a0, '\n'
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    #Finaliza execução
    li $v0, 10  # exit syscall code = 10
    syscall

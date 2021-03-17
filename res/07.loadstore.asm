.data
    x_len: .word 10
    x: .word 2, 8, 5, 4, 1, 3, 7, 6, 0, 9
    prompt1: .asciiz "Vetor antes: "
    prompt2: .asciiz "Vetor depois: "

.text
j main
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
    beq  $t0, $t1, print_v_end
    nop
    j print_v_loop
    nop
print_v_end:
    jr $ra
    nop

soma_dez:
    move $t0, $zero      #t0 é o indice
    lw $t1, x_len        #t1 é o limite
    la $t3, x            #t3 guarda o endereco base
soma_dez_loop:
    sll $t4, $t0, 2             #t4 é o endereço do elemento
    add $t4, $t4, $t3
    lw $t5, 0($t4)
    addi $t5, $t5, 10
    sw $t5, 0($t4)
    addi $t0, $t0, 1
    beq  $t0, $t1, soma_dez_end
    nop
    j soma_dez_loop
    nop
soma_dez_end:
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

    jal soma_dez
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

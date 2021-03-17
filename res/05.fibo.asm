# Seção de dados
.data
    prompt:  .asciiz "Digite quantos termos da sequência de Fibonacci devem ser impressos: "    # String com \0 no final
    prompt2: .asciiz "Sequência: "
    prompt3: .asciiz "E o termo seguinte (que é o retorno da função): "

#Seção de texto
.text
    # Imprime prompt
    la $a0, prompt # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4       # 4 é o código para imprimir uma string
    syscall

    # Le inteiro em $v0
    li $v0, 5 # 5 é o código para ler um inteiro
    syscall
    move $s0, $v0

    # Imprime saida
    la $a0, prompt2  # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4       # 4 é o código para imprimir uma string
    syscall

    move $a0, $s0
    jal fibo
    nop
    move $s1, $v0 # Salva o retorno
    # Imprime \n
    li $a0, '\n'
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    # Imprime prompt
    la $a0, prompt3 # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4       # 4 é o código para imprimir uma string
    syscall
    move $a0, $s1
    li $v0, 1 # 1 é o código para imprimir um inteiro
    syscall
    # Imprime \n
    li $a0, '\n'
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    #Finaliza execução
    li $v0, 10  # exit syscall code = 10
    syscall


fibo:
    move $s0, $a0
    beq  $s0, $zero, fibo_zero # Trata caso especial 0
    nop
    addi $t0, $zero, 0         # a = 0
    addi $t1, $zero, 1         # b = 1
    addi $t2, $zero, 1         # i = 1
fibo_loop:

    move $a0, $t0
    li $v0, 1 # 1 é o código para imprimir um inteiro
    syscall
    # Imprime espaço
    li $a0, ' '
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    slt  $t3, $t2, $s0         # i < n entao 1, senao 0
    beq  $t3, $zero, fibo_ret  # então terminou
    nop
    add  $t4, $t0, $t1         # tmp = a + b
    add  $t0, $zero, $t1       # a = b
    add  $t1, $zero, $t4       # b = tmp
    addi $t2, $t2, 1           # i++
    j    fibo_loop
    nop
fibo_zero:
    addi $v0, $zero, 0    # 0
    jr   $ra              # retorna
    nop
fibo_ret:
    add  $v0, $zero, $t1  # preenche resultado
    jr   $ra              # retorna
    nop

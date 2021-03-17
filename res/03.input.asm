# Seção de dados
.data
    prompt:  .asciiz "Digite n: "    # String com \0 no final
    output:  .asciiz "Você digitou: "

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
    la $a0, output  # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4       # 4 é o código para imprimir uma string
    syscall

    # imprime inteiro lido
    move $a0, $s0
    li $v0, 1 # 1 é o código para imprimir um inteiro
    syscall
    # Imprime \n
    li $a0,'\n'
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    #Finaliza execução
    li $v0, 10  # exit syscall code = 10
    syscall

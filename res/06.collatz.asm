.data
    prompt:  .asciiz "Digite o n para calcular o comprimento da sequencia de collatz(n). 0 (zero) finaliza a execução: "    # String com \0 no final
    prompt2: .asciiz "Collatz(n) = "

.text

inicio:
    la $a0, prompt # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall

    # Le inteiro em $v0
    li $v0, 5 # 5 é o código para ler um inteiro
    syscall
    move $a0, $v0

    beq $a0, $zero, saida
    nop
    jal collatz
    nop
    move $a0, $v0
    li $v0, 1 # 1 é o código para imprimir um inteiro
    syscall
    # Imprime \n
    li $a0,'\n'
    li $v0, 11 # 11 é o código para imprimir um caractere
    syscall

    j inicio
    nop
saida:
    #Finaliza execução
    li $v0, 10  # exit syscall code = 10
    syscall

collatz:
    li      $2,1
    li      $5,1
    beq     $4,$2,collatz_end2
    nop
    li      $7,1
collatz_loop:
    srl     $3,$4,31
    sll     $2,$4,1
    addu    $2,$2,$4
    addu    $3,$3,$4
    andi    $6,$4,0x1
    addiu   $2,$2,1
    srl     $4,$3,1
    beq     $6,$0,collatz_end1
    nop
    move    $4,$2
collatz_end1:
    move    $2,$5
    addiu   $5,$5,1
    bne     $4,$7,collatz_loop
    nop
    addiu   $2,$2,1
    jr      $31
    nop
collatz_end2:
    li      $2,1
    jr      $31
    nop

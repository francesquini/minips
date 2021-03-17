.data
    msg:       .asciiz "O valor impresso no final deve ser 8191 e não deve imprimir 'Erro'.\n"
    errotext:  .asciiz "Erro\n"
    
.text

main:

    # Notem que as somas são todas potências de 2. Vocês podem 
    # usar isso para achar o ponto do erro do seu emulador (caso exista).

    la $a0, msg # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall

    addi $t0, $zero, 0
    beq  $t0, $zero, t_dois
    addi $t0, $t0, 1 # Branch delay slot - Taken
    
t_dois:
    addi $t1, $zero, 1
    bne  $t0, $t1, erro
    addi $t0, $t0, 2 # Branch delay slot - Not taken
    beq  $t0, $t1, erro
    addi $t0, $t0, 4 # Branch delay slot - Not taken
    
    j  t_tres
    addi $t0, $t0, 8  # Branch delay slot
    addi $t0, $t0, 1  # Não deveria rodar
t_tres:
    jal vai
    addi $t0, $t0, 16 # Branch delay slot
    addi $t0, $t0, 32            
    
    la   $t2, vai2
    jalr $t2
    addi $t0, $t0, 64  # Branch delay slot
    addi $t0, $t0, 128
     
    la   $t2, vai3
    la   $s0, t_quatro
    jalr $s0, $t2
    addi $t0, $t0, 256 # Branch delay slot
    addi $t0, $t0, 512                  

t_quatro:
    move $a0, $t0  
    li $v0, 1    
    syscall
    
    addi    $v0, $zero, 10  # exit syscall code = 10
    syscall
       
vai:
   jr $ra   
   addi $t0, $t0, 1024 # Branch delay slot   

vai2:
   jr $ra   
   addi $t0, $t0, 2048 # Branch delay slot   
   
vai3: 
   jr $s0      
   addi $t0, $t0, 4096 # Branch delay slot
       
erro:
    la $a0, errotext # coloca em a0 o parâmetro da chamada a syscall
    li $v0, 4    # 4 é o código para imprimir uma string
    syscall
    addi    $v0, $zero, 10  # exit syscall code = 10
    syscall  
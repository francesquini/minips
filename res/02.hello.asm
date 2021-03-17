.data
  texto: .asciiz "Ola mundo!"

.text

la $a0, texto
li $v0, 4
SYSCALL

li $a0, 10
li $v0, 11
SYSCALL

li $v0, 10
SYSCALL

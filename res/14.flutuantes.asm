.data
        pi: .float  3.14159265
         e: .float  2.71828182
       pid: .double 3.1415926535897932
        ed: .double 2.7182818284590452
    tfloat: .asciiz "\nDigite um float: "
   tdouble: .asciiz "\nDigite um double: "
   trespon: .asciiz "Você digitou: "
      erro: .asciiz "\nErro na representação"
.text


    lwc1 $f12, pi
    li   $v0, 2  # print float syscall code = 2
    syscall

    la   $t0, pi
    lw   $t1, 0($t0)
    mfc1 $t2, $f12
    bne  $t1, $t2, erro_p
    nop

    li $a0, '\n'
    li $v0, 11
    syscall

    lwc1 $f12, e
    li   $v0, 2  # print float syscall code = 2
    syscall

    la   $t0, e
    lw   $t1, 0($t0)
    mfc1 $t2, $f12
    bne  $t1, $t2, erro_p
    nop

    li $a0, '\n'
    li $v0, 11
    syscall

    ldc1 $f12, pid
    li   $v0, 3  # print double syscall code = 3
    syscall

    la     $t0, pid
    lw     $t1, 0($t0)
    lw     $t2, 4($t0)
    mfc1.d $t4, $f12
    bne    $t1, $t4, erro_p
    nop
    bne    $t2, $t5, erro_p
    nop

    li $a0, '\n'
    li $v0, 11
    syscall

    ldc1 $f12, ed
    li   $v0, 3  # print double syscall code = 3
    syscall

    la     $t0, ed
    lw     $t1, 0($t0)
    lw     $t2, 4($t0)
    mfc1.d $t4, $f12
    bne    $t1, $t4, erro_p
    nop
    bne    $t2, $t5, erro_p
    nop

    la $a0, tfloat
    li $v0, 4
    syscall
    li   $v0, 6
    syscall
    mov.s $f12, $f0
    la $a0, trespon
    li $v0, 4
    syscall
    li   $v0, 2  # print float syscall code = 2
    syscall

    la $a0, tdouble
    li $v0, 4
    syscall
    li   $v0, 7
    syscall
    mov.d $f12, $f0
    la $a0, trespon
    li $v0, 4
    syscall
    li   $v0, 3  # print float syscall code = 2
    syscall

    li    $v0, 10  # exit syscall code = 10
    syscall

erro_p:
    la $a0, erro
    li $v0, 4
    syscall
    li    $v0, 10  # exit syscall code = 10
    syscall

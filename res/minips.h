/*
 * Emilio Francesquini <e.francesquini@ufabc.edu.br> 04/2021
 */

#ifndef __MINIPS
#define __MINIPS

#ifdef __mips__
int main();

void __start() {
    main();
}
#else
#include <stdio.h>
#include <stdlib.h>
#endif

// Syscall 1
__attribute__ ((noinline))
void p_int(int x) {
#ifdef __mips__
    asm volatile(
        "li $v0, 1;"
        "move $a0, %0;"
        "syscall;"
        :
        : "r" (x)
        : "$v0");
#else
    printf("%d", x);
#endif
}

// Syscall 2
__attribute__((noinline))
void p_float(float f) {
#ifdef __mips__
    asm volatile(
        "li $v0, 2;"
        "syscall;"
        :
        : "r" (f)
        : "$v0");
#else
    printf("%.7f", f);
#endif
}

// Syscall 3
__attribute__((noinline))
void p_double(double d) {
#ifdef __mips__
    asm volatile(
        "li $v0, 3;"
        "syscall;"
        :
        : "r" (d)
        : "$v0");
#else
    printf("%lf", d);
#endif
}

// Syscall 4
__attribute__ ((noinline))
void p_str(const char str[]) {
#ifdef __mips__
    asm volatile(
        "li $v0, 4;"
        "move $a0, %0;"
        "syscall;"
        :
        : "r" (str)
        : "$v0", "$a0");
#else
    printf("%s", str);
#endif
}

// Syscall 5
__attribute__ ((noinline))
int read_int() {
    int read;
#ifdef __mips__
    asm volatile(
        "li $v0, 5;"
        "syscall;"
        "move %0, $v0;"
        : "=r" (read)
        :
        : "$v0");
#else
    scanf("%d", &read);
#endif
    return read;
}

// Syscall 6
__attribute__ ((noinline))
float read_float() {
    float read;
#ifdef __mips__
    asm volatile (
        "li $v0, 6;"
        "syscall;"
        "mfc1 %0, $f0;"
        : "=r" (read)
        :
        : "$v0", "$f0");
#else
    scanf("%f", &read);
#endif
    return read;
}

// Syscall 7
__attribute__ ((noinline))
double read_double() {
#ifdef __mips__
    register double read __asm("$a0");
    asm volatile (
        "li $v0, 7;"
        "syscall;"
        "mfc1 $a0, $f0;"
        "mfc1 $a1, $f1;"
        : "=r" (read)
        :
        : "$v0");
    return read;
#else
    double read;
    scanf("%lf", &read);
    return read;
#endif
}

// Syscall 10
__attribute__ ((noreturn, noinline))
void halt() {
#ifdef __mips__
    asm volatile("li $v0, 10; syscall" : : : "$v0");
    __builtin_unreachable();
#else
    exit(0);
#endif
}

// Syscall 11
__attribute__ ((noinline))
void p_char(char c) {
#ifdef __mips__
    asm volatile(
        "li $v0, 11;"
        "move $a0, %0;"
        "syscall;"
        :
        : "r" (c)
        : "$v0", "$a0");
#else
    printf("%c", c);
#endif
}
#endif

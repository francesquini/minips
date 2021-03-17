#ifndef __MINIPS
#define __MINIPS

int main();

void __start() {
    main();
}

// Syscall 1
__attribute__ ((noinline))
void p_int(int x) {
    asm volatile(
        "li $v0, 1;"
        "move $a0, %0;"
        "syscall;"
        :
        : "r" (x)
        : "$v0");
}

// Syscall 2
__attribute__((noinline))
void p_float(float f) {
    asm volatile(
        "li $v0, 2;"
        "syscall;"
        :
        : "r" (f)
        : "$v0");
}

// Syscall 3
__attribute__((noinline))
void p_double(double d) {
    asm volatile(
        "li $v0, 3;"
        "syscall;"
        :
        : "r" (d)
        : "$v0");
}

// Syscall 4
__attribute__ ((noinline))
void p_str(const char str[]) {
    asm volatile(
        "li $v0, 4;"
        "move $a0, %0;"
        "syscall;"
        :
        : "r" (str)
        : "$v0", "$a0");
}

// Syscall 5
__attribute__ ((noinline))
int read_int() {
    int read;
    asm volatile(
        "li $v0, 5;"
        "syscall;"
        "move %0, $v0;"
        : "=r" (read)
        :
        : "$v0");
    return read;
}

// Syscall 6
__attribute__ ((noinline))
float read_float() {
    float read;
    asm volatile (
        "li $v0, 6;"
        "syscall;"
        "mfc1 %0, $f0;"
        : "=r" (read)
        :
        : "$v0", "$f0");
    return read;
}

// Syscall 7
__attribute__ ((noinline))
double read_double() {
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
}

// Syscall 10
__attribute__ ((noreturn, noinline))
void halt() {
    asm volatile("li $v0, 10; syscall" : : : "$v0");
    __builtin_unreachable();
}

// Syscall 11
__attribute__ ((noinline))
void p_char(char c) {
    asm volatile(
        "li $v0, 11;"
        "move $a0, %0;"
        "syscall;"
        :
        : "r" (c)
        : "$v0", "$a0");
}
#endif

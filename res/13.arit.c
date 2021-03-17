#include "minips.h"

char t1[] = "Quadrados: ";
char lf = '\n';

int square(int num) {
    return num * num;
}


int primo(int n) {
    if (n % 2 == 0) return 0;
    for (int i = 3; i < n; i += 2) {
        if (n % i == 0) {
            return 0;
        }
    }
    return 1;
}

void divide () {
    for (int i = -5; i <= 5; i++) {
        for (int j = -5; j <= 5; j++) {
            if (j == 0) continue;
            p_char('\t');
            p_int(i);
            p_char('/');
            p_int(j);
            p_str(" = ");
            int q = i / j;
            int r = i % j;
            p_int(q);
            p_str(" r ");
            p_int(r);
            p_char(lf);
        }
    }
}

int main() {
    p_str(t1); // String armazenada em .data
    for (int i = -5; i <= 5; i ++) {
        p_int(square(i));
        p_char(' ');
        p_int(1);
    }

    p_str("\nPrimos: "); // String armazenada em .rodata
    for (int i = 2; i <= 100; i ++) {
        if (primo(i)) {
            p_int(i);
            p_char(' ');
        }
    }
    p_char(lf);

    p_str("Divisores:\n");
    divide();

    halt();
    return 0; //Linha inalcançável
}

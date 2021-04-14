/* Gerador de números aleatórios simples.
 * Emilio Francesquini <e.francesquini@ufabc.edu.br> 03/2021
 */

#include "minips.h"

/*
 * Este simples gerador de números aleatórios é baseado naquele
 * proposto por George Marsaglia (MWC - multiply with carry). Apesar
 * de ser muito simples, ele é capaz de passar pela série de testes
 * Marsaglia's DIEHARD para geradores de números aleatórios. A
 * implementação abaixo é uma adaptação daquela feita por John
 * D. Cook.
 */

int rand_z;
int rand_w;

int random_int() {
    rand_z = 36969 * (rand_z & 65535) + (rand_z >> 16);
    rand_w = 18000 * (rand_w & 65535) + (rand_w >> 16);
    return (rand_z << 16) + rand_w;
}

void init_random(int seed) {
    int n1 = seed * 48947;
    int n2 = seed * 104623;
    rand_z = (n1 != 0) ? n1 : 362436069;
    rand_w = (n2 != 0) ? n2 : 521288629;
}

int main() {
    p_str("A semente e fixa. "
          "Para a semente 42 deve imprimir a seguinte sequencia:\n"
          "-1742029533 -1272795027 402859010 728825109 -621355405"
          "-593937550 -803078983 719364131 -1510153639 1510292939"
          "\n\n"
          "Sequencia gerada:\n");

    init_random(42);

    for (int i = 0; i < 10; i++) {
        int r = random_int();
        p_int(r);
        p_char(' ');
    }
    p_char('\n');
    halt();
    return 0; //Linha inalcançável
}

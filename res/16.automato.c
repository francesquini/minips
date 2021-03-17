/* Autômato celular simples
 * https: // en.wikipedia.org/wiki/Elementary_cellular_automaton
 * Emilio Francesquini <e.francesquini@ufabc.edu.br> 03/2021
 */

#include "minips.h"

#define COLS 79

int atual[COLS];
int proxima[COLS];

int regra[2][2][2];

void initRegra(int r) {
    int bit = 1;
    for (int i = 0; i < 2 ; i++) {
        for (int j = 0; j < 2 ; j++) {
            for (int k = 0; k < 2 ; k++) {
                regra[i][j][k] = !!(r & bit);
                bit <<= 1;
            }
        }
    }
}

int aplicaRegra (int pos) {
    int v0 = pos > 0 ? atual[pos - 1] : atual[pos];
    int v1 = atual[pos];
    int v2 = pos < COLS - 1 ? atual[pos + 1] : atual[pos];
    return regra[v0][v1][v2];
}

void calculaProximaLinha () {
    // Daria pra fazer em um vetor só, porém menos claro didaticamente
    for (int i = 0; i < COLS; i++)
        proxima[i] = aplicaRegra(i);
    for (int j = 0; j < COLS; j++)
        atual[j] = proxima[j];
}

void imprimeLinha() {
    for (int i = 0; i < COLS; i++) {
        p_char (atual[i] ? '#' : ' ');
    }
    p_char('\n');
}

int main() {
    p_str("Digite a regra que deseja rodar (tente 18, 60, 30, 94): ");
    int regra = read_int();

    p_str("Iteracoes: ");
    int n = read_int();

    initRegra(regra);

    // Inicializa vetores
    for (int i = 0; i < COLS; i++) {
        atual[i] = 0;
        proxima[i] = 0;
    }
    atual[COLS/2] = 1;

    imprimeLinha();
    for (int i = 0; i < n; i++) {
        calculaProximaLinha();
        imprimeLinha();
    }
    halt();
    return 0; //Linha inalcançável
}
